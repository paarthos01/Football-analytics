####---SETTING UP THE DATA SETS - INCL CLEANING
# Paarth Arora, Oct 1 2020

# Read data
team_data <- read_csv("data/full-epl2020.csv")
player_data <- read_csv("data/players_1920_fin.csv")
rankings_data <- read_csv("data/rankings-epl2020.csv")
#missing_team_data <- read_csv("data/missing-epl2020.csv")

# CLEANING THE FULL SEASON DATA
team_data <- team_data %>%  
  select(1:27, -Div, -Time, -HTHG, -HTAG, -HTR) # removing the first col because it is not helpful, matchDay
#  rename(Referee = Referee.x, # renaming to be consistent (w/o x)
#         HS = HS.x, 
#         HST = HST.x,
#         HF = HF.x,
#         HC = HC.x,
#         HY = HY.x,
#         HR = HR.x,
#         AS = AS.x,
#         AST = AST.x,
#         AF = AF.x,
#         AC = AC.x,
#         AY = AY.x,
#         AR = AR.x,
#         B365H = B365H.x,
#         B365D = B365D.x,
#         B365A = B365A.x,
#         conceded = missed,
#         losses = loses,
#         team = teamId) %>% 
#  mutate(team = replace(team, 
#                          team == "Man Utd", "Man United")) # making team names consistent
  
str(team_data)
team_data$Date <- as.Date(team_data$Date, format = "%d/%m/%Y") # convert to date

n <- 1 # adding a match_id to original data
match_id <- rep(1:380, each = n)

team_data <- team_data %>% 
  mutate(match_id)  %>% 
  select(match_id, everything()) %>% # move match id to front of df
  group_by(match_id) %>% 
  pivot_longer(3:4, names_to = "h_a", values_to = "team") %>% # spreading the teamid col into one col
  ungroup() %>% 
  mutate(h_a = replace(h_a, h_a == c("HomeTeam","AwayTeam"), c("h","a"))) %>% # renaming values to be h and a to align with existing data
  select(h_a, everything()) %>% # Moving col to front
  select(team, everything()) # Moving col to front

# adding scored and conceded cols from FTHG and FTAG
team_data$scored <- ifelse(team_data$h_a == "h", 
                           team_data$FTHG, 
                                   ifelse(team_data$h_a == "a", 
                                          team_data$FTAG, NA))

team_data$conceded <- ifelse(team_data$h_a == "h", 
                             team_data$FTAG, 
                                   ifelse(team_data$h_a == "a", 
                                          team_data$FTHG, 
                                          NA))

# adding new cols and cleaning old ones
team_data <- team_data %>% 
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
  mutate(wins = case_when(result == "w" ~ 1, TRUE ~ 0),
         draws = case_when(result == "d" ~ 1, TRUE ~ 0),
         losses = case_when(result == "l" ~ 1, TRUE ~ 0)) %>% 
  group_by(team) %>% 
  mutate(cum_points = cumsum(pts), # Update cumulative cols, assuming df ordered by game dates
         cum_goal = cumsum(scored),
         cum_con = cumsum(conceded)) %>% 
  ungroup() %>% 
  mutate(shots = ifelse(h_a == "h", HS, AS), # Add shots and shots on target columm
         shots_target = ifelse(h_a == "h", HST, AST),
         yellow_cards = ifelse(h_a == "h", HY, AY),
         red_cards = ifelse(h_a == "h", HR, AR),
         fouls = ifelse(h_a == "h", HF, AF),
         corners = ifelse(h_a == "h", HC, AC),
         shot_accuracy = ifelse(h_a == "h", HtrgPerc, AtrgPerc)) %>% 
  mutate(h_a2 = h_a,
         team2 = team) %>% 
  group_by(match_id) %>% 
  pivot_wider(names_from = h_a2, values_from = team2) %>% 
  ungroup() %>% 
  group_by(match_id) %>% # Cleaning to develop an opposition col
  fill(h, .direction = "downup") %>% 
  fill(a, .direction = "downup") %>% 
  mutate(opposition = ifelse(h_a == "h", a, h)) %>% # Adding opposition col
  ungroup() %>% 
  select(-h, -a) # removing cols

# Merging with EPL 2019-2020 final rankings table
team_data <- team_data %>% 
  full_join(rankings_data, by = c("opposition" = "team")) %>% 
  rename(opposition_rank = rank,
         opposition_rank_group = rank_group)

# Write to CSV
#write.csv(team_data, file = "data/cleaned-2020-data.csv")
