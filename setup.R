# Setting up

# Load packages
library(tidyverse)

# Read data
team_data <- read_csv("data/epl2020.csv")
player_data <- read_csv("data/players_1920_fin.csv")

# Clean data
team_data <- team_data %>%  
  select(-X1) # removing the first col because it is not helpful 

