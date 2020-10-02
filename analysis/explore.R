# Exploratory analysis

# 1000 goals, 9000 shots, 3000 shots on target ~ whole season
full_2020_data %>% 
  mutate(shots = ifelse(h_a == "h", HS, AS)) %>% 
  mutate(shots_target = ifelse(h_a == "h", HST, AST)) %>% 
  select(scored, shots, shots_target) %>%
  summarise(sum_goals = sum(scored),
            sum_shots = sum(shots),
            sum_shots_target = sum(shots_target))

# Liverpool were the runaway champions
full_2020_data %>% 
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

# Most teams experienced 'Home advantage'
full_2020_data %>% 
  filter(result == "w") %>% 
  group_by(team) %>% 
  count(h_a) %>% 
  ggplot(aes(x = reorder(team, n), y = n, fill = h_a)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Most teams experienced 'Home advantage'.",
       subtitle = "Each team won more Home games except for Southampton.",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Team",
       y = "No of wins",
       fill = "Home / away")

# Most teams scored more goals at Home
full_2020_data %>% 
  group_by(team, h_a) %>% 
  summarise(goals = mean(scored)) %>% 
  ggplot(aes(x = reorder(team, goals), y = goals, fill = h_a)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Most teams scored more goals at Home.",
       subtitle = "Each team scored more goals at Home except for Chelsea and 
Crystal Palace",
       caption = "Source: EPL 2019-2020 season data.",
       x = "Team",
       y = "Avg goals scored per game",
       fill = "Home / away")

## LINEAR REGRESSION - GOALS SCORED
# Low correlation between no of shots and no of goals
lm_goals1 <- full_2020_data %>% 
  mutate(shots = ifelse(h_a == "h", HS, AS)) %>% 
  select(shots, scored) %>% 
  select(shots, scored)

model_goals1 <- lm(scored ~ shots, data = lm_goals1)
model_goals1$coefficients # For each shot, 0.088 extra goals (so would take 10 shots for 1 goal) 
summary(model_goals1)

model_fn1 = function (x) {
  beta0 = model_goals1$coefficients[1]
  beta1 = model_goals1$coefficients[2]
  
  beta0 + beta1*x
}

lm_goals1 %>% 
  ggplot(aes(x = shots, y = scored)) +
  geom_point() +
  geom_function(fun = model_fn1, colour = "blue")

# No  of shots on target is a significant predictor of no of goals
lm_goals2 <- full_2020_data %>% 
  mutate(shots_target = ifelse(h_a == "h", HST, AST)) %>% 
  select(shots_target, scored)

model_goals2 <- lm(scored ~ shots_target, data = lm_goals2)
model_goals2$coefficients # For each shot on target, 0.307 extra goals (so would take 3 shots on target for 1 goal) 
summary(model_goals2) # 95% confidence interval that coefficient is between 0.279 and 0.334 is and p-value <0.05 
summary(model_goals2)$r.squared # shots on target explain about 40% of the variance in goals - not great

model_fn2 = function (x) {
  beta0 = model_goals2$coefficients[1]
  beta1 = model_goals2$coefficients[2]
  
  beta0 + beta1*x
}

lm_goals2 %>% 
  ggplot(aes(x = shots_target, y = scored)) +
  geom_point() +
  geom_function(fun = model_fn2, colour = "blue")

# Correlation between no of yellow cards and no of goals
lm_goals3 <- full_2020_data %>% 
  mutate(yellow_cards = ifelse(h_a == "h", HY, AY)) %>% 
  select(scored, yellow_cards)

model_goals3 <- lm(scored ~ yellow_cards, data = lm_goals3)
model_goals3$coefficients # For each yellow card, -0.1 less goal (so would take 10 yellow cards to have 1 less goal) 
summary(model_goals3)$coefficients # 95% confidence interval that coefficient is between 0.279 and 0.334 is and p-value <0.05 
summary(model_goals3)$r.squared # shots on target explain about 40% of the variance in goals - not great

# Low interaction between YCs and shots on target to explain goals
# But it explains slightly more of the variance (slightly higher R Squared)
lm_goals4 <- full_2020_data %>% 
  mutate(yellow_cards = ifelse(h_a == "h", HY, AY),
         shots_target = ifelse(h_a == "h", HST, AST)) %>% 
  select(scored, yellow_cards, shots_target)

model_interact <- lm(scored ~ shots_target + yellow_cards + shots_target:yellow_cards, data = lm_goals4)
summary(model_interact)$coefficients
summary(model_interact)$r.squared

model_fn4 = function (x) {
  beta0 = model_interact$coefficients[1]
  beta1 = model_interact$coefficients[2]
  beta2 = model_interact$coefficients[3]
  beta3 = model_interact$coefficients[4]
  
  beta0 + beta1*x + beta2*x + beta3*x
}

lm_goals4 %>% 
  ggplot(aes(x = shots_target, y = scored)) +
  geom_point() +
  geom_function(fun = model_fn4, colour = "blue") +
  geom_function(fun = model_fn2, colour = "red") # plotting the trendline for just shots on target to show the difference

# Trying to predict goals scored using model_interact
set.seed(12345)
grab_row <- sample(1:nrow(lm_goals4), 10)
lm_goals4_fit <- lm_goals4[-grab_row, ]
lm_goals4_test <- lm_goals4[grab_row, ]

model_interact_test <- lm(scored ~ shots_target + yellow_cards + shots_target:yellow_cards, 
                     data = lm_goals4_fit)

(test <- data.frame(prediction = predict(model_interact_test, lm_goals4_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_goals4_test$scored)) # Almost there!

# No  collinearity with yellow cards and shots on target
# Actually inverse relationship!
cor(lm_goals4[, 2:3])

# Using a linear model to predict a teams goals, using that teams shots data, is difficult
# Reducing the no of rows of data from 750 to only 38 limits the model
selected_team <- "Leicester"

lm_goals_teams <- full_2020_data %>% 
  mutate(yellow_cards = ifelse(h_a == "h", HY, AY),
         shots_target = ifelse(h_a == "h", HST, AST)) %>% 
  select(team, scored, yellow_cards, shots_target) %>% 
  filter(team == selected_team)

grab_team_row <- sample(1:nrow(lm_goals_teams), 1)
lm_goals_teams_fit <- lm_goals_teams[-grab_team_row, ]
lm_goals_teams_test <- lm_goals_teams[grab_team_row, ]

model_interact_teams <- lm(scored ~ shots_target, 
                          data = lm_goals_teams_fit)

(test <- data.frame(prediction = predict(model_interact_teams, lm_goals_teams_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_goals_teams_test$scored)) # Almost there!

## LINEAR REGRESSION - GOALS CONCEDED

# Expect shots on target to have inverse relationship (negative) to goals conceded
lm_conceded <- full_2020_data %>% 
  mutate(shots_target = ifelse(h_a == "h", HST, AST)) %>% 
  select(shots_target, conceded)

model_conceded <- lm(conceded ~ shots_target, data = lm_conceded)
model_conceded$coefficients # For each shot on target, 0.307 extra goals (so would take 3 shots on target for 1 goal) 
summary(model_conceded) # 95% confidence interval that coefficient is between 0.279 and 0.334 is and p-value <0.05 
summary(model_conceded)$r.squared # shots on target explain about 40% of the variance in goals - not great

model_fn5 = function (x) {
  beta0 = model_conceded$coefficients[1]
  beta1 = model_conceded$coefficients[2]
  
  beta0 + beta1*x
}

lm_goals2 %>% 
  ggplot(aes(x = shots_target, y = scored)) +
  geom_point() +
  geom_function(fun = model_fn2, colour = "blue")

## LOGISTIC REGRESSION


