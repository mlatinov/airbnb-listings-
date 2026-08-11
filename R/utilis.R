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
