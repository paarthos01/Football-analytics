# Linear Regression

## LINEAR REGRESSION - GOALS SCORED
# No  of shots on target is a significant predictor of no of goals
lm_goals0 <- team_data %>% 
  select(shots_target, scored)

model_goals0 <- lm(scored ~ shots_target, data = lm_goals2)
model_goals0$coefficients # For each shot on target, 0.307 extra goals (so would take 3 shots on target for 1 goal) 
summary(model_goals0) # 95% confidence interval that coefficient is between 0.279 and 0.334 is and p-value <0.05 
summary(model_goals0)$r.squared # shots on target explain about 40% of the variance in goals - not great

model_fn0 = function (x) {
  beta0 = model_goals0$coefficients[1]
  beta1 = model_goals0$coefficients[2]
  
  beta0 + beta1*x
}

lm_goals0 %>% 
  ggplot(aes(x = shots_target, y = scored)) +
  geom_point() +
  geom_function(fun = model_fn0, colour = "blue")

# Collection of variables used to predict goals scored
lm_goals1 <- team_data %>% 
  select(shots_target, h_a, corners, fouls, opposition_rank_group, scored)

model_goals1 <- lm(scored ~ shots_target + h_a + corners + opposition_rank_group, 
                   data = lm_goals1) 
#contrasts(lm_goals1$h_a) 
summary(model_goals1)$coefficients 
summary(model_goals1)$r.squared # Collection of variables explains 42% of the variability in goals scored
car::Anova(model_goals1) # Opposition rank group is a weaker predictor of goals than shots on target

# Trying to predict goals scored using collection of variables above
set.seed(12345)
grab_row <- sample(1:nrow(lm_goals1), 10)
lm_goals1_fit <- lm_goals1[-grab_row, ]
lm_goals1_test <- lm_goals1[grab_row, ]
test_model <- model_goals1

(test <- data.frame(prediction = predict(test_model, lm_goals1_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_goals1_test$scored)) # Almost there!

# Fortunately no  collinearity with yellow cards and shots on target
# Corners and fouls not not highly correlated (not >0.5)
# Though shots on target and corners are slightly correlated
cor(lm_goals1$shots_target,lm_goals1$corners)
mctest::omcdiag(model_goals1) # Testing for multi-collinearity
mctest::imcdiag(mod = model_goals1, method = "VIF") # Testing for multi-collinearity - Looks safe

# Using a linear model to predict a teams goals, using that teams data, is difficult
# Reducing the no of rows of data from 750 to only 38 limits the model
selected_team <- "Norwich"

lm_goals_teams <- team_data %>% 
  select(team, shots_target, h_a, corners, opposition_rank_group, scored) %>% 
  filter(team == selected_team)

grab_team_row <- sample(1:nrow(lm_goals_teams), 1)
lm_goals_teams_fit <- lm_goals_teams[-grab_team_row, ]
lm_goals_teams_test <- lm_goals_teams[grab_team_row, ]

model_interact_teams <- lm(scored ~ shots_target + h_a + corners + opposition_rank_group, 
                           data = lm_goals_teams_fit)

(test <- data.frame(prediction = predict(model_interact_teams, lm_goals_teams_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_goals_teams_test$scored)) # Almost there!

## LINEAR REGRESSION - GOALS CONCEDED
# Expect shots on target to have inverse relationship (negative) to goals conceded
lm_conceded <- team_data %>% 
  select(shots_target, h_a, corners, fouls, yellow_cards, red_cards, opposition_rank_group, conceded)

model_conceded <- lm(conceded ~ shots_target + h_a + fouls + yellow_cards + opposition_rank_group, 
                     data = lm_conceded) 
summary(model_conceded)$coefficients 
summary(model_conceded)$r.squared
car::Anova(model_conceded) # Corners are not significant predictor - Remove from model

mctest::omcdiag(model_conceded) # Testing for multi-collinearity - Looks safe
mctest::imcdiag(mod = model_conceded, method = "VIF") # Testing for multi-collinearity - Looks safe

# Trying to predict goals conceded using collection of variables above
set.seed(12345)
grab_row <- sample(1:nrow(lm_conceded), 10)
lm_conceded_fit <- lm_conceded[-grab_row, ]
lm_conceded_test <- lm_conceded[grab_row, ]
test_model <- model_conceded

(test <- data.frame(prediction = predict(test_model, lm_conceded_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_conceded_test$conceded)) # Almost there!

# Using a linear model to predict a teams conceded goals, using that teams data, is difficult
# Reducing the no of rows of data from 750 to only 38 limits the model
selected_team <- "Arsenal"

lm_conceded_teams <- team_data %>% 
  select(team, shots_target, h_a, corners, fouls, yellow_cards, 
         red_cards, opposition_rank_group, conceded) %>% 
  filter(team == selected_team)

grab_team_row <- sample(1:nrow(lm_conceded_teams), 1)
lm_conceded_teams_fit <- lm_conceded_teams[-grab_team_row, ]
lm_conceded_teams_test <- lm_conceded_teams[grab_team_row, ]

model_interact_teams <- lm(conceded ~ shots_target + h_a + fouls + yellow_cards + corners + opposition_rank_group,
                           data = lm_goals_teams_fit)

(test <- data.frame(prediction = predict(model_interact_teams, lm_conceded_teams_test, 
                                         interval = "confidence"), # giving a lwr and upper range using 95% confidence interval
                    actual = lm_conceded_teams_test$conceded)) # Almost there!
