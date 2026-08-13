#### Libraries ####
library(tidyverse)
library(patchwork)

## Helper function for spatial projections ##
project_to_local_km <- function(longitude, latitude) {
  stopifnot(length(longitude) == length(latitude))

  lat0 <- mean(latitude)
  lon0 <- mean(longitude)
  out <- data.frame(
    x_km = (longitude - lon0) * 111.32 * cos(lat0 * pi / 180),
    y_km = (latitude  - lat0) * 111.32
  )

  attr(out, "lon0") <- lon0
  attr(out, "lat0") <- lat0
  out
}

#### Function to clean the data and prepare it for modeling ####
clean_airbnb_data <- function(data_raw){

  data_clean <- data_raw %>% 
    ## Subset the data to the relevent features 
    select(
      ## Location ## 
      latitude,
      longitude,
      neighbourhood_cleansed,
      neighbourhood_group_cleansed,
      ## Room Features ##
      bathrooms,
      bedrooms,
      beds,
      price,
      room_type,
      accommodates,
      ## Host Features ##
      host_is_superhost,
      hosts_time_as_host_years,
      hosts_time_as_host_months,
      hosts_time_as_user_months,
      hosts_time_as_user_years,
      minimum_nights
    ) %>%
    ## Rename the covariates ##
    rename(
      neighbourhood       = neighbourhood_cleansed,
      neighbourhood_group = neighbourhood_group_cleansed,
      superhost           = host_is_superhost
    ) %>%
    mutate(
      ## Recover the total time of host as host and host as user in months 
      host_months_host = (hosts_time_as_host_years * 12) + hosts_time_as_host_months,
      host_months_user = (hosts_time_as_user_years * 12) + hosts_time_as_user_months,

      ## How long someone was on Airbnb as a guest before they ever became a host
      guest_lead_months = pmax(host_months_user - host_months_host, 0),
      
      ## Fix the price feature Transform it into numerical value
      price = parse_number(price),
      log_price = log(price),

      ## Encode Superhost as 0 and 1 
      superhost = ifelse(superhost == TRUE, 1, 0),

      ## Add the projected coordinates 
      x_projected = project_to_local_km(longitude,latitude)$x,
      y_projected = project_to_local_km(longitude,latitude)$y,

      ## Transform the groping variable as numeric 
      neighbourhoods_id = as.integer(as.factor(neighbourhood)),
      neighbourhood_group_id = as.integer(as.factor(neighbourhood_group))
    ) %>%
    ## Remove the Host features which were already used to create new features 
    select(-hosts_time_as_host_years,-hosts_time_as_host_months,-hosts_time_as_user_months,-hosts_time_as_user_years) %>%
    ## Remove the missing values from the price and superhost 
    filter(!is.na(price), !is.na(superhost))
}

#### Function to vizualize distributions of numerical features ####
viz_dist <- function(model_data, feature){
  ggplot(data = model_data, aes(x  = .data[[feature]]))+
    geom_density(fill = "lightblue")+
    theme_minimal()+
    labs(
      x = paste0(feature),
      y = "Count",
      title = paste0("Distribution of ",feature,"")
    )+
    theme(
      title = element_text(size = 20)
    )
}
viz_dist_cond <- function(model_data, dist_feature, adjust){
  ggplot(data = model_data, aes(x = .data[[dist_feature]], fill = .data[[adjust]]))+
    geom_density()+
    labs(
      title = paste0("Conditional distribution of ",dist_feature," conditioned on ",adjust,""),
      x = paste0(dist_feature),
      y = "Density",
      fill = adjust
    ) +
    scale_fill_viridis_d(option = "inferno")+
    facet_wrap(~.data[[adjust]])+
    theme_minimal()+
    theme(
      title = element_text(size = 15)  
    )
}
#### Function to viz relantionship between two variables ####
viz_relan <- function(model_data, x_var, y_var){
  ggplot(data = model_data, aes(x = .data[[x_var]], y = .data[[y_var]]))+
    geom_point() +
    geom_smooth() +
    theme_minimal() +
    labs(
      x = x_var,
      y = y_var,
      title = paste0("Relantionship between ",x_var," and ", y_var)
    )+
    theme(title = element_text(size = 15))
}

#### Function to viz relantionship betweene x y and z 
viz_relan_z <- function(model_data, x_var, y_var, adjust){
  ggplot(data = model_data, 
    aes(
      x = .data[[x_var]], 
      y = .data[[y_var]], 
      color = as.factor(.data[[adjust]]))
    ) +
    geom_point() +
    geom_smooth() +
    theme_minimal()+
    facet_wrap(~as.factor(.data[[adjust]]))+
    scale_fill_viridis_d(option = "A")+
    labs(
      x = x_var,
      y = y_var,
      color = adjust,
      title = paste0(
        "Relantionship between ", x_var, " ",y_var," adjusting for " ,adjust,""
      )
    ) +
    theme(
      title = element_text(size = 15)
    )
}

#### Functions to compare simulated data and model data ####
compare_relan <- function(
  sim_data,
  model_data,
  x_var,
  y_var
){
  ## Sample viz 
  sample_viz <- viz_relan(model_data = model_data, x_var = x_var, y_var = y_var) +
    labs(title = "Sample Data")

  ## Simulation data viz 
  sim_viz  <- viz_relan(model_data = sim_data, x_var = x_var, y_var = y_var) +
    labs(title = "Simulated Data")

  ## Combine the Viz ##
  compare <- (sample_viz / sim_viz) + plot_annotation(
    title = "Comparison betweem Simulated Model Data and Sample Data",
    theme = theme(
      title = element_text(size = 15)
    )
  ) 
  return(compare)
}
comapare_dist <- function(
  sim_data,
  model_data,
  x_var
){
  ## Sample viz 
  sample_viz <- viz_dist(model_data = model_data, feature = x_var) +
    labs(title = "Sample Data")

  ## Simulated viz
  sim_viz <- viz_dist(model_data = sim_data, feature = x_var) +
    labs(title = "Simulated Data")

  ## Combine the Viz ##
  compare <- (sample_viz / sim_viz) + plot_annotation(
    title = "Comparison betweem Simulated Model Data and Sample Data",
    theme = theme(
      title = element_text(size = 15)
    )
  )
  return(compare)
}
compare_relan_z <- function(
  sim_data,
  model_data,
  x_var,
  y_var,
  z_var
){
  ## Sample Data
  sample_viz <- viz_relan_z(model_data = model_data, x_var = x_var, y_var = y_var, adjust = z_var) +
    labs(title = "Sample Data")

  ## Simulated Data 
  sim_viz <- viz_relan_z(model_data = sim_data, x_var = x_var, y_var = y_var, adjust = z_var) +
    labs(title = "Simulated Data")

  ## Combine the Viz ##
  compare <- (sample_viz / sim_viz) + plot_annotation(
    title = "Comparison betweem Simulated Model Data and Sample Data",
    theme = theme(
      title = element_text(size = 15)
    )
  )
  return(compare)
}

#### Stan diagnostics functions ###
diagnose_gp_funnel <- function(fit, stan_data) {
 
  # ---- 1. Recompute lambda[m] exactly as in `transformed data` ----
  M1 <- stan_data$M1
  M2 <- stan_data$M2
  c_ <- stan_data$c
  Lx <- c_ * max(abs(stan_data$x_km)) + 1e-6
  Ly <- c_ * max(abs(stan_data$y_km)) + 1e-6
 
  lambda <- numeric(M1 * M2)
  j1_idx <- numeric(M1 * M2)
  j2_idx <- numeric(M1 * M2)
  m <- 1
  for (j1 in 1:M1) {
    for (j2 in 1:M2) {
      lambda[m] <- (j1 * pi / (2 * Lx))^2 + (j2 * pi / (2 * Ly))^2
      j1_idx[m] <- j1
      j2_idx[m] <- j2
      m <- m + 1
    }
  }
  lambda_rank <- rank(lambda)  # 1 = lowest frequency = most likely informed
 
  # ---- 2. Pull posterior draws for z_gp and rho_gp ----
  draws <- as_draws_df(fit$draws(variables = c("z_gp", "rho_gp", "alpha_gp")))
 
  z_gp_sd <- draws %>%
    select(starts_with("z_gp[")) %>%
    summarise(across(everything(), sd)) %>%
    pivot_longer(everything(), names_to = "param", values_to = "post_sd") %>%
    mutate(m = as.integer(gsub("z_gp\\[|\\]", "", param)),
           lambda = lambda[m],
           lambda_rank = lambda_rank[m])
 
  cat("\n--- z_gp posterior SD summary ---\n")
  cat("Fraction of z_gp components with post_sd > 0.9 (prior-dominated): ",
      round(mean(z_gp_sd$post_sd > 0.9), 3), "\n")
  cat("Fraction with post_sd < 0.5 (strongly informed): ",
      round(mean(z_gp_sd$post_sd < 0.5), 3), "\n\n")
 
  # ---- 3. Plot: posterior SD vs frequency rank ----
  # A funnel/mismatch signature = high-lambda_rank (high freq) sitting near
  # SD=1 (prior), low-lambda_rank (low freq) pulled well below 1.
  p1 <- ggplot(z_gp_sd, aes(x = lambda_rank, y = post_sd)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = "z_gp[m] posterior SD vs spectral frequency rank",
         subtitle = "Points near SD=1 are prior-dominated (flat); points well below are likelihood-informed (curved)",
         x = "lambda rank (1 = lowest frequency)", y = "posterior SD of z_gp[m]") +
    theme_minimal()
  print(p1)
 
  # ---- 4. Pairs plots: rho_gp vs a few informed / uninformed z_gp ----
  informed_idx   <- z_gp_sd %>% arrange(post_sd) %>% slice(1:3) %>% pull(m)
  uninformed_idx <- z_gp_sd %>% arrange(desc(post_sd)) %>% slice(1:3) %>% pull(m)
 
  pick <- c(informed_idx, uninformed_idx)
  pair_df <- draws %>%
    select(rho_gp, all_of(paste0("z_gp[", pick, "]"))) %>%
    pivot_longer(-rho_gp, names_to = "param", values_to = "value") %>%
    mutate(group = ifelse(param %in% paste0("z_gp[", informed_idx, "]"),
                           "informed (low lambda)", "uninformed (high lambda)"))
 
  p2 <- ggplot(pair_df, aes(x = rho_gp, y = value)) +
    geom_point(alpha = 0.3, size = 0.6) +
    facet_wrap(~ param, ncol = 3) +
    labs(title = "rho_gp vs z_gp[m]: funnel check",
         subtitle = "Funnel/banana shape expected for informed components only") +
    theme_minimal()
  print(p2)
 
  # ---- 5. Treedepth saturation vs rho_gp ----
  sdiag <- fit$sampler_diagnostics(format = "df")
  max_td <- fit$metadata()$max_treedepth
  if (is.null(max_td)) max_td <- max(sdiag$treedepth__)
 
  td_df <- sdiag %>%
    select(.chain, .iteration, treedepth__) %>%
    bind_cols(rho_gp = draws$rho_gp) %>%
    mutate(hit_max = treedepth__ >= max_td)
 
  cat("--- Treedepth saturation vs rho_gp ---\n")
  cat("Mean rho_gp | treedepth saturated:     ",
      round(mean(td_df$rho_gp[td_df$hit_max]), 3), "\n")
  cat("Mean rho_gp | treedepth NOT saturated: ",
      round(mean(td_df$rho_gp[!td_df$hit_max]), 3), "\n\n")
 
  p3 <- ggplot(td_df, aes(x = rho_gp, fill = hit_max)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
    labs(title = "rho_gp distribution: saturated vs non-saturated treedepth iterations",
         fill = "hit max_treedepth") +
    theme_minimal()
  print(p3)
 
  invisible(list(z_gp_sd = z_gp_sd, pair_df = pair_df, td_df = td_df,
                 informed_idx = informed_idx, uninformed_idx = uninformed_idx))
}