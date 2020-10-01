####---SETTING UP THE DATA SETS - INCL CLEANING
# Paarth Arora, Oct 1 2020

# Read data
team_data <- read_csv("data/epl2020.csv")
player_data <- read_csv("data/players_1920_fin.csv")
missing_team_data <- read_csv("data/missing-epl2020.csv")

# CLEANING THE ORIGINAL DATA (only part of season)
team_data <- team_data %>%  
  select(-X1, -matchDay, -matchtime) %>% # removing the first col because it is not helpful, matchDay
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
         B365A = B365A.x,
         conceded = missed,
         losses = loses)

team_data$date <- as.Date(team_data$date) # convert to date

n <- 2 # adding a match_id to original data
match_id <- rep(1:288, each=n)
team_data <- team_data %>% 
  mutate(match_id)

### CLEANING THE MISSING GAME DATA 
# Tidy the missing EPL 2019-20 data to same format as existing data
missing_team_data <- missing_team_data %>% 
  mutate(match_id = row_number()) %>% # add match id for each row
  select(match_id, everything()) %>% # move match id to front of df
  select(-Div, -HTHG, -HTAG, -HTR) # removing redundant cols
missing_team_data <- missing_team_data[, -c(25:103)]

missing_team_data <- missing_team_data %>% 
  group_by(match_id) %>% 
  pivot_longer(4:5, names_to = "h_a") %>% # spreading the teamid col into two cols
  rename(teamId = value) %>% # renaming column to align to existing data
  ungroup() %>% 
  mutate(h_a = replace(h_a, h_a == c("HomeTeam","AwayTeam"), c("h","a"))) # renaming values to be h and a to align with existing data

# adding scored and conceded cols from FTHG and FTAG
missing_team_data$scored <- ifelse(missing_team_data$h_a == "h", 
                                   missing_team_data$FTHG, 
                                   ifelse(missing_team_data$h_a == "a", 
                                                                  missing_team_data$FTAG, NA))

missing_team_data$conceded <- ifelse(missing_team_data$h_a == "h", 
                                   missing_team_data$FTAG, 
                                   ifelse(missing_team_data$h_a == "a", 
                                          missing_team_data$FTHG, NA))

missing_team_data$date <- 
  as.Date(missing_team_data$Date, format = "%d/%m/%y") # converting to date format

# adding new cols and cleaning old ones
missing_team_data <- missing_team_data %>% 
  group_by(match_id) %>% 
  mutate(result = case_when( # adding the result col
    FTHG == FTAG ~ "d", 
    h_a == "h" & FTHG > FTAG ~ "w",
    h_a == "h" & FTHG < FTAG ~ "l",
    h_a == "a" & FTHG > FTAG ~ "l",
    h_a == "a" & FTHG < FTAG ~ "w",
    TRUE ~ "NA")) %>% 
  ungroup() %>% 
  mutate(pts = case_when( # adding the points col
    result == "w" ~ 3,
    result == "d" ~ 1,
    result == "l" ~ 0,
    TRUE ~ 0)) %>% 
  mutate(HtrgPerc = HST/HS, # adding shots on target cols
         AtrgPerc = AST/AS) %>% 
  select(-Date, -Time, -FTHG, -FTAG, -FTR) # removing redundant cols

missing_team_data <- missing_team_data %>% 
  mutate(wins = case_when(result == "w" ~ 1, TRUE ~ 0),
         draws = case_when(result == "d" ~ 1, TRUE ~ 0),
         losses = case_when(result == "l" ~ 1, TRUE ~ 0))

#### MERGING THE DATASETS
full_2020_data <- bind_rows(team_data, missing_team_data)

# adding a match_id to merged data
n <- 2 
match_id <- rep(1:380, each=n)
full_2020_data$match_id <- match_id
