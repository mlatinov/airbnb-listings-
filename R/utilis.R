library(tidyverse)

#### Function to clean the data and prepare it for modeling ####
clean_airbnb_data <- function(data_raw){

  data_clean <- data_raw %>% 
    ## Subset the data to the relevent features 
    select(
      ## Location ## 
      latitude,
      longitude,
      neighbourhood_cleansed,
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
      lan = latitude,
      lon = longitude,
      neighbourhood = neighbourhood_cleansed,
      superhost     = host_is_superhost
    ) %>%
    mutate(
      ## Recover the total time of host as host and host as user in months 
      host_months_host = (hosts_time_as_host_years * 12) + hosts_time_as_host_months,
      host_months_user = (hosts_time_as_user_years * 12) + hosts_time_as_user_months,
      
      ## Fix the price feature Transform it into numerical value
      price = parse_number(price),

      ## Encode Superhost as 0 and 1 
      superhost = ifelse(superhost == TRUE, 1, 0)
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
#### Simulation helper for location ####
f_coordinates <- function(
  n = 100,
  eta = 1.5,
  rho = 2.5
){

  # Generate lon and lat 
  lon <- runif(n, min = 2.092, max = 2.221)
  lat <- runif(n, min = 41.35, max = 41.46)
  coord <- cbind(lon, lat)

  # Build a distance matrix 
  D <- as.matrix(dist(coord))

  # Turn the Distance matrix into kernel
  K <- eta^2 * exp(-D^2 / (2 * rho^2))
  diag(K) <- diag(K) + 1e-6 

  # Cholesky and draw the latent surface
  L <- t(chol(K))
  z <- rnorm(n, 0, 1)
  f <- as.numeric(L %*% z)

  # Return the GP f of coordinates 
  return(f)
}