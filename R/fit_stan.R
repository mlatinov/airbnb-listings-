
#### Functions to Fit Stan Models ####
stan_fit_airbnb_v1 <- function(model_data){

  # Transform the data into list as the Stan expect 
  stan_data <- list(
    # Settings and Indexing 
    N          = nrow(model_data),
    prior_only = 0,
    N_hoods       = length(unique(model_data$neighbourhoods_id)),
    N_hood_groups = length(unique(model_data$neighbourhood_group_id)),
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
      
    # Outcome 
    log_price = model_data$log_price
  )

  # Compile the Stan model 
  stan_model <- cmdstanr::cmdstan_model(stan_file = "Stan/models/airbnb_v1.stan")
  intial     <- stan_model$pathfinder(data = stan_data, seed = 123)
  
  # Safe intial function fro the 2D HSGP parameters 
  init = function() list(
    alpha_gp = 0.3,
    rho_gp   = 2,
    z_gp     = rep(0, 8 * 8),
    Lr       = diag(3)
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

## PPC ##
stanviz::plot_ppc_dens(
  model = sample, 
  y        = model_data$log_price,
  yrep_var = "log_price_rep"
)

stanviz::plot_ppc_by_group(
  model = sample,
  y = model_data$log_price,
  yrep_var = "log_price_rep",
  group    = model_data$neighbourhood_group_id
)

## Posterior of the effect ##
stanviz::plot_param_halfeye(
  model = sample,
  pars = "z_gp",
  null_line = NULL
)

## Predictive Errors ## 
stanviz::plot_ppc_error_scatter(
  model = sample,
  y        = model_data$log_price,
  yrep_var = "log_price_rep",
  x = model_data$superhost
)


##### Diagnostics #####
 data = list(
      # Settings and Indexing 
      N          = nrow(model_data),
      prior_only = 0,
      N_hoods       = length(unique(model_data$neighbourhoods_id)),
      N_hood_groups = length(unique(model_data$neighbourhood_group_id)),
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
      
      # Outcome 
      log_price = model_data$log_price
)
a <- diagnose_gp_funnel(fit = sample, stan_data = data)

## Rho Gp vs z_gp ##
