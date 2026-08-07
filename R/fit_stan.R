
#### Functions to Fit Stan Models ####
stan_fit_airbnb_v1 <- function(model_data){

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(stan_file = "Stan/models/airbnb_v1.stan")

  # Sample from the model 
  sample <- stan_model$sample(
    data = list(
      # Settings and Indexing 
      N = nrow(model_data),
      prior_only = 0,
      N_neighbourhoods  = length(unique(model_data$neighbourhood)),
      neighbourhoods_id = model_data$neighbourhoods_id, 
      
      # 2D HSGP Settings 
      M1 = 20,
      M2 = 20,
      x_km = model_data$x_projected,
      y_km = model_data$y_projected,
      c    = 1.5,
      
      # Covariates 
      super_host = model_data$superhost,   
      
      # Outcome 
      log_price = model_data$log_price
    ),
    # HMC Settings 
    chains = 1,
    iter_sampling = 100,
    output_dir = "Stan/results/",
    seed = 42 
  )

}