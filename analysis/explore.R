# Exploratory analysis

# 1000 goals, 9000 shots, 3000 shots on target ~ whole season
team_data %>% 
  select(scored, shots, shots_target) %>%
  summarise(sum_goals = sum(scored),
            sum_shots = sum(shots),
            sum_shots_target = sum(shots_target))

# Liverpool were the runaway champions
team_data %>% 
  group_by(team) %>% 
  summarise(total_points = sum(pts)) %>% 
  ggplot(aes(x = reorder(team, total_points), y = total_points)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Liverpool were the runaway champions.",
       subtitle = "Final point tally.",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Team",
       y = "Total points")

# Home teams score more goals and concede less
team_data %>% 
  group_by(h_a) %>% 
  summarise(scored = sum(scored), conceded = sum(conceded)) %>% 
  ggplot(aes(x = h_a, y = scored)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Home teams score more goals and concede less.",
       subtitle = "All teams, EPL 2019-2020.",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Home or away",
       y = "Total goals scored")

# Most teams experienced 'Home advantage'
team_data %>% 
  group_by(team, h_a) %>% 
  summarise(total_points = sum(pts)) %>% 
  ggplot(aes(x = reorder(team, total_points), y = total_points, fill = h_a)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Most teams experienced 'Home advantage'.",
       subtitle = "Each team earned more points at Home except for Southampton.",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Team",
       y = "Total points",
       fill = "Home / away")

# Most teams scored more goals at Home
team_data %>% 
  group_by(team, h_a) %>% 
  summarise(goals = mean(scored)) %>% 
  ggplot(aes(x = reorder(team, goals), y = goals, fill = h_a)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Most teams scored more goals at Home.",
       subtitle = "Each team scored more goals at Home except for Southamption, 
Chelsea and Crystal Palace",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Team",
       y = "Avg goals scored per game",
       fill = "Home / away")
