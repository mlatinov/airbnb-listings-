#### Main Target Pipeline from the project ####

# Libraries 
library(tidyverse)
library(targets)

#### Source Function 
tar_source("R/utilis.R")
tar_source("R/simulations.R")
tar_source("R/fit_stan.R")

#### Pipeline ####
list(

  ## Load the Raw data 
  tar_target(
    name = data_raw,
    command = read_csv("data/listings.csv")
  ),
  ## Clean and subset the raw data 
  tar_target(
    name = model_data,
    command = clean_airbnb_data(data_raw)
  ),
  tar_target(
    name = sim_airbnb_data,
    command = simulate_airbnb_v1()
  ),
  ## Run the Airbnb Model 1 on the simulated data 
  tar_target(
    name = simulation_recovery,
    command = stan_fit_airbnb_v1(sim_airbnb_data)
  )
)