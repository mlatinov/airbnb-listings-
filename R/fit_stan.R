
#### Functions to Fit Stan Models ####
stan_fit_airbnb <- function(
  model_data, 
  prior_only = 0, 
  stan_file_path, 
  iter_sampling = 1000,
  parallel_chains = 4, 
  set_seed = 42
){
  # Transform the data into list as the Stan expect 
  stan_data <- transform_data_to_stanlist(model_data)

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(stan_file = stan_file_path)

  # Sample from the model 
  sample <- stan_model$sample(
    data = stan_data,
    # HMC Settings 
    parallel_chains = parallel_chains,
    iter_sampling = iter_sampling,
    output_dir    = tempdir(),
    seed          = set_seed,
    # Initialize the sampling from Good Location 
    init = safe_stan_init(chains = parallel_chains)
  )
  # Save the results 
  out_path <- tempfile(fileext = ".rds")
  sample$save_object(file = out_path) 
  
  # Return the outpath 
  return(out_path)
}

#### Multithreading Version of Airbnb Stan Model ####
stan_fit_airbnb_performance <- function(
  model_data, 
  prior_only = 0, 
  stan_file_path, 
  iter_sampling = 1000,
  threads_per_chain = 2,
  parallel_chains = 4,
  set_seed = 42
){
  # Transform the data into list as the Stan expect  
  stan_data <- transform_data_to_stanlist(model_data, prior_only = prior_only)

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(
    stan_file = stan_file_path,
    cpp_options = list(stan_threads = TRUE)
  )

  # Sample from the model 
  sample <- stan_model$sample(
    data = stan_data,
    # HMC Settings 
    parallel_chains   = parallel_chains,
    threads_per_chain = threads_per_chain,
    iter_sampling = iter_sampling,
    output_dir    = tempdir(),
    seed          = set_seed,
    # Initialize the sampling from Good Location 
    init = safe_stan_init(chains = parallel_chains)
  )
  # Save the results 
  out_path <- tempfile(fileext = ".rds")
  sample$save_object(file = out_path) 
  
  # Return the outpath 
  return(out_path)
}