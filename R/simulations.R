#### Function for Simulation to Test Stan Models ####
source("R/utilis.R")

#### Simulation of only price ~ aj + bj Super Host i ####
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

  # Location effect 
  f_gp <- f_coordinates(n = length(ids), rho = gp_rho, eta = gp_eta)
  
  ## Linaer Predictor ##
  mu_i <- alpha_neighbourhood[ids] + b_super_host_neighbourhood[ids] * super_host + f_gp

  # Sample price from LogNormal Distribution 
  price <- exp(rnorm(length(ids), mean = mu_i, sd = sd_obs))

  # Return the simulation data 
  sim_data <- data.frame(
    neighbourhoods_id = ids,
    super_host        = super_host,
    price             = price
  )
  return(sim_data)
}