# Setting up

# Load packages
library(tidyverse)
library(anytime)
library(lubridate)

# Read data
team_data <- read_csv("data/epl2020.csv")
player_data <- read_csv("data/players_1920_fin.csv")
missing_team_data <- read_csv("data/missing-epl2020.csv")

# Clean data
team_data <- team_data %>%  
  select(-X1, -matchDay) # removing the first col because it is not helpful, matchDay

# Tidying the team_data cols
team_data <- team_data %>% 
  rename(Referee = Referee.x, # renaming to be consistent (w/o x)
         HS = HS.x, 
         HST = HST.x,
         HF = HF.x,
         HC = HC.x,
         HY = HY.x,
         HR = HR.x,
         AS = AS.x,
         AST = AST.x,
         AF = AF.x,
         AC = AC.x,
         AY = AY.x,
         AR = AR.x,
         B365H = B365H.x,
         B365D = B365D.x,
         B365A = B365A.x)

# Tidy the missing EPL 2019-20 data to same format as existing data
missing_team_data <- missing_team_data %>% 
  mutate(match_id = row_number()) %>% # add match id for each row
  select(match_id, everything()) %>% # move match id to front of df
  select(-Div, -HTHG, -HTAG, -HTR) # removing redudant cols
missing_team_data <- missing_team_data[, -c(25:103)]

missing_team_data <- missing_team_data %>% 
  group_by(match_id) %>% 
  pivot_longer(4:5, names_to = "h_a") %>% 
  rename(teamId = value) %>% 
  mutate(h_a = replace(mpg, cyl==4, NA))
  
str(missing_team_data)

  