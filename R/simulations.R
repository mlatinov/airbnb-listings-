#### Function for Simulation to Test Stan Models ####
source("R/utilis.R")

#### Simulation of only price ~ a + aj + ak + bj * Super Host i + f(lat, lon) ####
sim_simple_price_model <- function(
  ## Settings 
  n_neighbourhood_groups = 6,
  n_neighbourhoods = 10,
  obs_per_neighbourhood = 50,

  ## Random price intercept 
  baseline_price = log(240),
  sigma_neighbourhood_group_price = 0.6,
  sigma_neighbourhood_price       = 0.25,
  
  ## Random Super Host Slope
  baseline_super_host_eff = 0.12,
  sigma_super_host_eff = 0.05,
  
  ## Location GP function parameters 
  gp_rho = 2.5,
  gp_eta = 1.5,
  c      = 1.5, 
  m1     = 8,
  m2     = 8,
  
  ## Observation Price variation 
  sd_obs = 0.5
){

  ## Create Indexes 
  ids <- rsims::make_nested_ids(
    levels = list(
      neighbourhood_group = n_neighbourhood_groups,
      neighbourhoods      = n_neighbourhoods,
      i                   = obs_per_neighbourhood
    )
  )

  ## Simulate Super Host Informed from the data 
  super_host <- rbinom(nrow(ids), size = 1, prob = 0.27)

  ### Model paramters ###

  # Random intercepts
  alpha_neighbourhood_group <- sigma_neighbourhood_group_price * rnorm(length(unique(ids$neighbourhood_group_id)), 0, 1)
  alpha_neighbourhood       <- sigma_neighbourhood_price * rnorm(length(unique(ids$neighbourhoods_id)), 0, 1)

  # Random Super Host Slope 
  b_super_host_neighbourhood <- baseline_super_host_eff + sigma_super_host_eff * rnorm(length(alpha_neighbourhood_group), 0, 1)

  # Project the coordinates 
  latitude  <- runif(nrow(ids), 41.35, 41.46)
  longitude <- runif(nrow(ids), 2.092, 2.221)
  coords <- project_to_local_km(longitude, latitude)
  
  # Location effect computed with 2D HSGP over the coordinates 
  f_location <- rsims::simulate_hsgp_2d(
    M1 = m1,
    M2 = m2,
    latitude  = coords$y_km,
    longitude = coords$x_km,
    rho   = gp_rho,
    alpha = gp_eta,
    c     = c 
  )

  ## Linear Predictor ##
  mu_i <- (
    baseline_price 
    + alpha_neighbourhood_group[ids$neighbourhood_group_id] 
    + alpha_neighbourhood[ids$neighbourhoods_id] 
    + b_super_host_neighbourhood[ids$neighbourhood_group_id] * super_host 
    + f_location
  )

  # Sample price from LogNormal Distribution 
  price <- exp(rnorm(nrow(ids), mean = mu_i, sd = sd_obs))

  # Return the simulation data 
  sim_data <- data.frame(
    latitude   = latitude,
    longitude  = longitude,
    y_projected  = coords$y_km,
    x_projected  = coords$x_km,
    superhost    = super_host,
    price      = price,
    log_price  = log(price),
    i                      = ids$i_id,
    neighbourhood_group_id = ids$neighbourhood_group_id,
    neighbourhoods_id      = ids$neighbourhoods_id
  )
  return(sim_data)
}

#### Simulate price ~ a + aj + ak + bjxi + yjxi + f(lat, lon) ... where aj,bj,yj are correlated in the simulation ###
simulate_cor_test_model <- function(
  ## Settings 
  n_neighbourhood_groups = 10,
  n_neighbourhoods       = 69,
  obs_per_neighbourhood  = 10,

  ## Random price intercept  
  baseline_price = log(240),
  sigma_neighbourhood_group_price = 0.6,
  sigma_neighbourhood_price       = 0.25,
  
  ## Random Super Host Slope
  baseline_super_host_eff = 0.12,
  sigma_super_host_eff = 0.05,

  ## Random Accommodates Slope
  baseline_acc_eff = 0.1,
  sigma_acc_eff    = 0.01,

  ## Correlation Index 
  cor_price_acc = 0.6, 
  cor_price_sh  = 0.1,
  cor_acc_sh    = 0.1,
  
  ## Location GP function parameters 
  gp_rho = 2.5,
  gp_eta = 1.5,
  c      = 1.5, 
  m1     = 8,
  m2     = 8,
  
  ## Observation Price variation 
  sd_obs = 0.5
){
  
  ## Create Indexes 
  ids <- rsims::make_nested_ids(
    levels = list(
      neighbourhood_group = n_neighbourhood_groups,
      neighbourhoods      = n_neighbourhoods,
      i                   = obs_per_neighbourhood
    )
  )
  
  # Simulate Covariates Superhost and accommodates from Bernoulli and Negative Binomial Distribution 
  super_host   <- rbinom(nrow(ids), size = 1, prob = 0.27)
  accommodates <- pmin(rnbinom(nrow(ids), size = 3, mu = 3.8), 16)
  accommodates_c <- accommodates - mean(accommodates)

  ## Model Correlated parameters 
  v <- rsims::make_correlated_effects(
    n     = n_neighbourhood_groups,
    means = c(0, 0, 0),
    sds   = c(sigma_neighbourhood_group_price, sigma_super_host_eff,sigma_acc_eff),
    correlation_matrix = matrix(
      c(
        1,            cor_price_sh, cor_price_acc, 
        cor_price_sh, 1,            cor_acc_sh, 
        cor_price_acc, cor_acc_sh,            1
      ), 
      nrow = 3, byrow = TRUE
    )
  )

  ## Recover the correlated model parameters 
  alpha_group <- baseline_price          + v[, 1]
  beta_group  <- baseline_super_host_eff + v[, 2]
  gamma_group <- baseline_acc_eff        + v[, 3]

  # Random Neighbourhood intercept
  alpha_neighbourhoods <- sigma_neighbourhood_price * rnorm(length(unique(ids$neighbourhoods_id)), 0, 1)

  # Project the coordinates 
  latitude  <- runif(nrow(ids), 41.35, 41.46)
  longitude <- runif(nrow(ids), 2.092, 2.221)
  coords    <- project_to_local_km(longitude, latitude)
  
  # Location effect computed with 2D HSGP over the coordinates 
  f_location <- rsims::simulate_hsgp_2d(
    M1 = m1,
    M2 = m2,
    latitude  = coords$y_km,
    longitude = coords$x_km,
    rho   = gp_rho,
    alpha = gp_eta,
    c     = c 
  )

  ## Linear predictor 
  mu_i <- (
    alpha_group[ids$neighbourhood_group_id] 
    + alpha_neighbourhoods[ids$neighbourhoods_id]
    + beta_group[ids$neighbourhood_group_id]  * super_host 
    + gamma_group[ids$neighbourhood_group_id] * accommodates_c 
    + f_location
  )
    
  # Sample from Normal Distribution log price and transform it back 
  price <- exp(rnorm(nrow(ids), mean = mu_i, sd = sd_obs))

  # Combine and return the simulated data 
  sim_data <- data.frame(
    i     = ids$i_id,
    price = price,
    y_projected  = coords$y_km,
    x_projected  = coords$x_km,
    log_price = log(price),
    latitude = latitude,
    longitude = longitude,
    superhost = super_host,
    accommodates = accommodates,
    neighbourhood_group_id = ids$neighbourhood_group_id,
    neighbourhoods_id      = ids$neighbourhoods_id
  )
  return(sim_data)
}

#### Simulate airbnb data .. The same function as cor test model but with full adj set added ####
simulate_airbnb_v1 <- function(
  ## Settings 
  n_neighbourhood_groups = 6,
  n_neighbourhoods = 10,
  obs_per_neighbourhood = 50,

  ## Random price intercept 
  baseline_price = log(240),
  sigma_neighbourhood_group_price = 0.6,
  sigma_neighbourhood_price       = 0.25,
  
  ## Random Super Host Slope
  baseline_super_host_eff = 0.08,
  sigma_super_host_eff = 0.03,

  ## Random Accommodates Slope
  baseline_acc_eff = 0.08,
  sigma_acc_eff = 0.01,

  ## Correlation Index 
  cor_price_acc = 0.5,
  cor_price_sh = 0.1,
  cor_acc_sh = 0.1,
  
  ## Location GP function parameters 
  gp_rho = 2.5,
  gp_eta = 1.5,
  c      = 1.5, 
  m1     = 8,
  m2     = 8,

  ## Fixed covariates effects
  beta_guest_lead_months = 0.015,
  beta_minimum_nights = -0.025,

  ## Observation Price variation 
  sd_obs = 0.1
){

  ## Create Indexes 
  ids <- rsims::make_nested_ids(
    levels = list(
      neighbourhood_group = n_neighbourhood_groups,
      neighbourhoods      = n_neighbourhoods,
      i                   = obs_per_neighbourhood
    )
  )
  ### Simulate Covariates ###
  super_host   <- rbinom(nrow(ids), size = 1, prob = 0.27)
  accommodates <- pmin(rnbinom(nrow(ids), size = 3, mu = 3.8), 16)
  guest_lead_months <- rnbinom(nrow(ids), size = 0.2, mu = 9)
  minimum_nights    <- rnbinom(nrow(ids), size = 1.2, mu = 13.5)
  room_type         <- sample(
    x = c("Entire home/apt","Hotel room", "Private room" ,"Shared room"),
    size = nrow(ids),
    replace = TRUE, 
    prob = c(0.7616, 0.00467, 0.22643, 0.0073)
  )

  ## Center Covariates 
  accommodates_c      <- accommodates      - mean(accommodates)
  guest_lead_months_c <- guest_lead_months - mean(guest_lead_months)
  minimum_nights_c    <- minimum_nights    - mean(minimum_nights)

  ## Model Correlated parameters 
  v <- rsims::make_correlated_effects(
    n     = n_neighbourhood_groups,
    means = c(0, 0, 0),
    sds   = c(sigma_neighbourhood_group_price, sigma_super_host_eff,sigma_acc_eff),
    correlation_matrix = matrix(
      c(
        1,            cor_price_sh, cor_price_acc, 
        cor_price_sh, 1,            cor_acc_sh, 
        cor_price_acc, cor_acc_sh,            1
      ), 
      nrow = 3, byrow = TRUE
    )
  )

  ## Recover the correlated model parameters 
  alpha_group <- baseline_price          + v[, 1]
  beta_group  <- baseline_super_host_eff + v[, 2]
  gamma_group <- baseline_acc_eff        + v[, 3]

  # Random Neighbourhood intercept
  alpha_neighbourhoods <- sigma_neighbourhood_price * rnorm(length(unique(ids$neighbourhoods_id)), 0, 1)

  ## Room Effect 
  room_effects <- c(
    "Entire home/apt" = 0.3,  
    "Hotel room"      = 0.25, 
    "Private room"    = -0.1,
    "Shared room"     = -0.3 
  )
  room_effect <- room_effects[room_type]

  # Project the coordinates 
  latitude  <- runif(nrow(ids), 41.35, 41.46)
  longitude <- runif(nrow(ids), 2.092, 2.221)
  coords    <- project_to_local_km(longitude, latitude)
  
  # Location effect computed with 2D HSGP over the coordinates 
  f_location <- rsims::simulate_hsgp_2d(
    M1 = m1,
    M2 = m2,
    latitude  = coords$y_km,
    longitude = coords$x_km,
    rho   = gp_rho,
    alpha = gp_eta,
    c     = c 
  )

  ## Linear predictor 
  mu_i <- (
    alpha_group[ids$neighbourhood_group_id] + alpha_neighbourhoods[ids$neighbourhoods_id]
    + beta_group[ids$neighbourhood_group_id]  * super_host
    + gamma_group[ids$neighbourhood_group_id] * accommodates_c
    + beta_guest_lead_months * guest_lead_months_c
    + beta_minimum_nights    * minimum_nights_c
    + room_effect[room_type]
    + f_location
  )

  # Sample from Normal Distribution log price and transform it back 
  price <- exp(rnorm(nrow(ids), mean = mu_i, sd = sd_obs))

  # Combine and return the simulated data 
  sim_data <- data.frame(
    i = ids$i_id,
    price = price,
    log_price = log(price),
    latitude  = latitude,
    longitude = longitude,
    y_km  = coords$y_km,
    x_km  = coords$x_km,
    room_type = room_type,
    superhost = super_host,
    accommodates = accommodates,
    minimum_nights = minimum_nights,
    guest_lead_months = guest_lead_months,
    neighbourhood_group_id = ids$neighbourhood_group_id,
    neighbourhoods_id      = ids$neighbourhoods_id
  )
 return(sim_data)
}




