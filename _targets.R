#### Main Target Pipeline from the project ####

# Libraries 
library(tidyverse)
library(targets)
library(paws.storage)
library(paws.compute)

#### Source Function ####
tar_source("R/utilis.R")
tar_source("R/simulations.R")
tar_source("R/fit_stan.R")

#### AWS Integration ####
tar_option_set(
  resources = tar_resources(
    aws = tar_resources_aws(bucket = "stan-airbnb-s3",prefix = "intial_run")
  )
)

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
  ## Prior Predictive Checks on the Stan Airbnb Model Version 1 ##
  tar_target(
    name = prior_pc,
    command = stan_fit_airbnb(
      model_data, 
      prior_only = 1, 
      stan_file_path = "Stan/models/candidates/airbnb_v1.stan"
    ),
    format     = "file",
    repository = "aws"
  ),
  ## Simulate a Airbnb Data 
  tar_target(
    name = sim_airbnb_data,
    command = simulate_airbnb()
  ),
  ## Run the Airbnb Model v1 on the simulated data 
  tar_target(
    name = simulation_recovery,
    command = stan_fit_airbnb(
      sim_airbnb_data,
      stan_file_path = "Stan/models/candidates/airbnb_v1.stan"
    ),
    format     = "file",
    repository = "aws"  
  ),
  ## Run the Airbnb Performance Model Airbnb V1 on the simulated data 
  tar_target(
    name = simulation_recovery_performace,
    command = stan_fit_airbnb_performance(
      sim_airbnb_data,
      stan_file_path = "Stan/models/candidates/airbnb_performance_v1.stan",
      threads_per_chain = 2
    ),
    format     = "file",
    repository = "aws"
  )
  ## Run the Competitor Models 
  ## Compare the Models in Diagnostic Document and Performace Review Pick the Best one for the Final Report 
  ## Final Report 
)