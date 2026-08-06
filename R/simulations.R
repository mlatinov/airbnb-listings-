#### Function for Simulation to Test Stan Models ####
source("R/utilis.R")

#### Simulation of only price ~ aj + bj * Super Host i + f(lat, lon) ####
sim_simple_price_model <- function(
  ## Settings 
  n_neighbourhoods = 20,
  obs_per_neighbourhood = 100,

  ## Random price intercept 
  baseline_price = log(240),
  sigma_neighbourhood_price = 0.35,
  
  ## Random Super Host Slope
  baseline_super_host_eff = 0.12,
  sigma_super_host_eff = 0.05,
  
  ## Location GP function parameters 
  gp_rho = 2.5,
  gp_eta = 1.5,
  c      = 1.5, 
  m1     = 20,
  m2     = 20,
  
  ## Observation Price variation 
  sd_obs = 0.5
){

  ## Create Indexes 
  ids <- rep(1:n_neighbourhoods, each = obs_per_neighbourhood)

  ## Simulate Super Host Informed from the data 
  super_host <- rbinom(length(ids), size = 1, prob = 0.27)

  ### Model paramters ###

  # Random intercept 
  alpha_neighbourhood <- baseline_price + sigma_neighbourhood_price * rnorm(unique(ids), 0, 1)

  # Random Super Host Slope 
  b_super_host_neighbourhood <- baseline_super_host_eff + sigma_super_host_eff * rnorm(unique(ids), 0, 1)

  # Project the coordinates 
  latitude  <- runif(length(ids), 41.35, 41.46)
  longitude <- runif(length(ids), 2.092, 2.221)
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
  mu_i <- alpha_neighbourhood[ids] + b_super_host_neighbourhood[ids] * super_host + f_location

  # Sample price from LogNormal Distribution 
  price <- exp(rnorm(length(ids), mean = mu_i, sd = sd_obs))

  # Return the simulation data 
  sim_data <- data.frame(
    neighbourhoods_id = ids,
    latitude = latitude,
    longitude = longitude,
    super_host = super_host,
    price      = price
  )
  return(sim_data)
}

#### Simulate price ~ aj + bjxi + yjxi + f(lat, lon) ... where aj,bj,yj are correlated in the simulation ###
simulate_cor_test_model <- function(
  ## Settings 
  n_neighbourhoods = 20,
  obs_per_neighbourhood = 100,

  ## Random price intercept 
  baseline_price = log(240),
  sigma_neighbourhood_price = 0.35,
  
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
  m1     = 20,
  m2     = 20,
  
  ## Observation Price variation 
  sd_obs = 0.5
){
  
  # Create Ids 
  ids <- rep(1:n_neighbourhoods, each = obs_per_neighbourhood)

  # Simulate Covariates Superhost and accommodates from Bernoulli and Negative Binomial Distribution 
  super_host <- rbinom(length(ids), size = 1, prob = 0.27)
  accommodates <- pmin(rnbinom(length(ids), size = 3, mu = 3.8), 16)

  ## Model Correlated parameters 
  v <- rsims::make_correlated_effects(
    n     = n_neighbourhoods,
    means = c(0, 0, 0),
    sds   = c(sigma_neighbourhood_price, sigma_super_host_eff,sigma_acc_eff),
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
  alpha_j <- baseline_price          + v[, 1]
  beta_j  <- baseline_super_host_eff + v[, 2]
  gamma_j <- baseline_acc_eff        + v[, 3]

  # Project the coordinates 
  latitude  <- runif(length(ids), 41.35, 41.46)
  longitude <- runif(length(ids), 2.092, 2.221)
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
  mu_i <- alpha_j[ids] + beta_j[ids] * super_host + gamma_j[ids] * accommodates + f_location

  # Sample from Normal Distribution log price and transform it back 
  price <- exp(rnorm(length(ids), mean = mu_i, sd = sd_obs))

  # Combine and return the simulated data 
  sim_data <- data.frame(
    ids = ids,
    price = price,
    latitude = latitude,
    longitude = longitude,
    super_host = super_host,
    accommodates = accommodates
  )
  return(sim_data)
}

#### Simulate airbnb data .. The same function as cor test model but with full adj set added ####
simulate_airbnb_v1 <- function(
  ## Settings 
  n_neighbourhoods = 20,
  obs_per_neighbourhood = 100,

  ## Random price intercept 
  baseline_price = log(30),
  sigma_neighbourhood_price = 0.35,
  
  ## Random Super Host Slope
  baseline_super_host_eff = 0.08,
  sigma_super_host_eff = 0.03,

  ## Random Accommodates Slope
  baseline_acc_eff = 0.8,
  sigma_acc_eff = 0.01,

  ## Correlation Index 
  cor_price_acc = 0.5,
  cor_price_sh = 0.1,
  cor_acc_sh = 0.1,
  
  ## Location GP function parameters 
  gp_rho = 2.5,
  gp_eta = 1.5,
  c      = 1.5, 
  m1     = 20,
  m2     = 20,

  ## Fixed covariates effects
  beta_guest_lead_months = 0.015,
  beta_minimum_nights = -0.05,

  ## Observation Price variation 
  sd_obs = 0.1
){
  
  ## Create ids 
  ids <- rep(1:n_neighbourhoods, each = obs_per_neighbourhood)

  ### Simulate Covariates ###
  super_host   <- rbinom(length(ids), size = 1, prob = 0.27)
  accommodates <- pmin(rnbinom(length(ids), size = 3, mu = 3.8), 16)
  guest_lead_months <- rnbinom(length(ids), size = 0.2, mu = 9)
  minimum_nights    <- rnbinom(length(ids), size = 1.2, mu = 13.5)
  room_type         <- sample(
    x = c("Entire home/apt","Hotel room", "Private room" ,"Shared room"),
    size = length(ids),
    replace = TRUE, 
    prob = c(0.7616, 0.00467, 0.22643, 0.0073)
  )
    ## Model Correlated parameters 
  v <- rsims::make_correlated_effects(
    n     = n_neighbourhoods,
    means = c(0, 0, 0),
    sds   = c(sigma_neighbourhood_price, sigma_super_host_eff,sigma_acc_eff),
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
  alpha_j <- baseline_price          + v[, 1]
  beta_j  <- baseline_super_host_eff + v[, 2]
  gamma_j <- baseline_acc_eff        + v[, 3]

  ## Room Effect 
  room_effects <- c(
    "Entire home/apt" = 0.3,  
    "Hotel room"      = 0.25, 
    "Private room"    = -0.1,
    "Shared room"     = -0.3 
  )
  room_effect <- room_effects[room_type]

  # Project the coordinates 
  latitude  <- runif(length(ids), 41.35, 41.46)
  longitude <- runif(length(ids), 2.092, 2.221)
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
  mu_i <- (alpha_j[ids] 
    + beta_j[ids]  * super_host 
    + gamma_j[ids] * accommodates
    + beta_guest_lead_months * guest_lead_months
    + beta_minimum_nights    * minimum_nights
    + room_effect
    + f_location
  )

  # Sample from Normal Distribution log price and transform it back 
  price <- exp(rnorm(length(ids), mean = mu_i, sd = sd_obs))

  # Combine and return the simulated data 
  sim_data <- data.frame(
    ids = ids,
    price = price,
    log_price = log(price),
    latitude = latitude,
    longitude = longitude,
    room_type = room_type,
    superhost = super_host,
    accommodates = accommodates,
    minimum_nights = minimum_nights,
    guest_lead_months = guest_lead_months
  )
  return(sim_data)
}




