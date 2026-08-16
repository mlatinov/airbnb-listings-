
#### Functions to Fit Stan Models ####
stan_fit_airbnb_v1 <- function(model_data){

  # Transform the data into list as the Stan expect 
  stan_data <- list(
    # Settings and Indexing 
    N          = nrow(model_data),
    prior_only = 0,
    N_hoods        = length(unique(model_data$neighbourhoods_id)),
    N_hood_groups  = length(unique(model_data$neighbourhood_group_id)),
    hoods_id       = model_data$neighbourhoods_id, 
    hood_groups_id = model_data$neighbourhood_group_id,
      
    # 2D HSGP Settings 
    M1 = 8,
    M2 = 8,
    x_km = model_data$x_projected,
    y_km = model_data$y_projected,
    c    = 1.5,
      
    # Covariates 
    super_host   = model_data$superhost,   
    accommodates = model_data$accommodates, 
    room_type_id = model_data$room_type_id,
    guest_lead_months = model_data$guest_lead_months,
    minimum_nights    = model_data$minimum_nights,
      
    # Outcome 
    log_price = model_data$log_price
  )

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(stan_file = "Stan/models/airbnb_v1.stan")

  # Safe intial function for the 2D HSGP parameters 
  init = function() list(
    alpha_gp = 0.3,
    rho_gp   = 2,
    z_gp     = rep(0, 8 * 8)
  )

  # Sample from the model 
  sample <- stan_model$sample(
    data = stan_data,
    # HMC Settings 
    parallel_chains = 4,
    iter_sampling = 1000,
    output_dir    = "Stan/results/",
    seed          = 42,
    # Initialize the sampling from Good Location 
    init = init
  )
}

#### Multithreading Version of Airbnb V1 ####
stan_fit_airbnb_v2 <- function(model_data){
    # Transform the data into list as the Stan expect 
  stan_data <- list(
    # Settings and Indexing 
    N          = nrow(model_data),
    prior_only = 0,
    N_hoods        = length(unique(model_data$neighbourhoods_id)),
    N_hood_groups  = length(unique(model_data$neighbourhood_group_id)),
    hoods_id       = model_data$neighbourhoods_id, 
    hood_groups_id = model_data$neighbourhood_group_id,
      
    # 2D HSGP Settings 
    M1 = 8,
    M2 = 8,
    x_km = model_data$x_projected,
    y_km = model_data$y_projected,
    c    = 1.5,
      
    # Covariates 
    super_host   = model_data$superhost,   
    accommodates = model_data$accommodates, 
    room_type_id = model_data$room_type_id,
    guest_lead_months = model_data$guest_lead_months,
    minimum_nights    = model_data$minimum_nights,
      
    # Outcome 
    log_price = model_data$log_price
  )

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(
    stan_file = "Stan/models/airbnb_performance_v2.stan",
    cpp_options = list(stan_threads = TRUE)
  )

  # Safe intial function for the 2D HSGP parameters 
  init = function() list(
    alpha_gp = 0.3,
    rho_gp   = 2,
    z_gp     = rep(0, 8 * 8)
  )

  # Sample from the model 
  sample <- stan_model$sample(
    data = stan_data,
    # HMC Settings 
    parallel_chains = 4,
    threads_per_chain = 2,
    iter_sampling = 1000,
    output_dir    = "Stan/results/",
    seed          = 42,
    # Initialize the sampling from Good Location 
    init = init
  )
}