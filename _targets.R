#### Main Target Pipeline from the project ####

# Libraries 
library(tidyverse)
library(targets)

#### Source Function 
tar_source("R/utilis.R")

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
  ## Create a Simulation for Stage 1 Only superhost price and location coordinates 
  
)