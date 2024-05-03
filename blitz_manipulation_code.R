library(tidyverse)
library(caret)
library(xgboost)

setwd("Data/Data042323/Data/Football Analytics/Blitz 2024")

nfl_data <- read_csv("nfl_data.csv")

nfl_data_passes <- nfl_data %>% 
  mutate_at(vars(ThrowAccuracy, OnTargetThrow, Catchable, ThrowDepth, Completion, Sacked), as.numeric) %>% 
  filter(ThrowAccuracy >= 0, Catchable >= 0, OnTargetThrow >= 0, PassingAttempt == 1)

nfl_carries <- nfl_data %>% 
  filter(Carries == "NULL")


# accuracy 5 and 6 have NULLs for on target and catch-able
#  only a few 7s but have non Null data
nfl_data_accuracy <- nfl_data_passes %>%
  select(Player, Season, ThrowAccuracy) %>% 
  mutate(accuracy7 = ifelse(ThrowAccuracy == 7, 1, 0),
         accuracy6 = ifelse(ThrowAccuracy == 6, 1, 0),
         accuracy5 = ifelse(ThrowAccuracy == 5, 1, 0),
         accuracy4 = ifelse(ThrowAccuracy == 4, 1, 0),
         accuracy3 = ifelse(ThrowAccuracy == 3, 1, 0),
         accuracy2 = ifelse(ThrowAccuracy == 2, 1, 0),
         accuracy1 = ifelse(ThrowAccuracy == 1, 1, 0),
         accuracy0 = ifelse(ThrowAccuracy == 0, 1, 0)) %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            total_accuracy7 = sum(accuracy7),
            total_accuracy6 = sum(accuracy6),
            total_accuracy5 = sum(accuracy5),
            total_accuracy4 = sum(accuracy4),
            total_accuracy3 = sum(accuracy3),
            total_accuracy2 = sum(accuracy2),
            total_accuracy1 = sum(accuracy1),
            total_accuracy0 = sum(accuracy0),
            accuracy7_percentage = total_accuracy7 / total_passes,
            accuracy6_percentage = total_accuracy6 / total_passes,
            accuracy5_percentage = total_accuracy5 / total_passes,
            accuracy4_percentage = total_accuracy4 / total_passes,
            accuracy3_percentage = total_accuracy3 / total_passes,
            accuracy2_percentage = total_accuracy2 / total_passes,
            accuracy1_percentage = total_accuracy1 / total_passes,
            accuracy0_percentage = total_accuracy0 / total_passes)

nfl_target <- nfl_data_passes %>% 
  select(Season, Player, OnTargetThrow) %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            on_target_perc = sum(OnTargetThrow) / total_passes)

nfl_catchable <- nfl_data_passes %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            catchable_pct = sum(Catchable) / total_passes)

nfl_short_acc <- nfl_data_passes %>%
  filter(ThrowDepth <= 9) %>% 
  group_by(Season, Player) %>% 
  summarise(total_short_passes = n(),
            on_target_short = sum(OnTargetThrow) / total_short_passes)

nfl_medium_acc <- nfl_data_passes %>% 
  filter(ThrowDepth > 10 & ThrowDepth <= 19) %>% 
  group_by(Season, Player) %>% 
  summarise(total_medium_passes = n(),
            on_target_medium = sum(OnTargetThrow) / total_medium_passes)

nfl_deep_acc <- nfl_data_passes %>% 
  filter(ThrowDepth > 19) %>% 
  group_by(Season, Player) %>% 
  summarise(total_deep_passes = n(),
            on_target_deep = sum(OnTargetThrow) / total_deep_passes)

nfl_comp_per <- nfl_data_passes %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            comp_per = sum(Completion) / total_passes)
  


all_nfl_qb_data <- nfl_catchable %>% 
  left_join(nfl_data_accuracy, by = c("Player", "Season")) %>% 
  left_join(nfl_deep_acc, by = c("Player", "Season")) %>% 
  left_join(nfl_medium_acc, by = c("Player", "Season")) %>% 
  left_join(nfl_short_acc, by = c("Player", "Season")) %>% 
  left_join(nfl_target, by = c("Player", "Season")) %>% 
  left_join(nfl_comp_per, by = c("Player", "Season")) %>% 
  filter(total_passes.x >= 30) %>% 
  select(-c(total_passes.y, total_passes.x.x, total_passes.y.y))

write_csv(all_nfl_qb_data, "nfl_qb_data.csv")


nfl_total_points <- nfl_data_passes %>% 
  group_by(Season, Player) %>% 
  mutate(PasserPoints = as.numeric(PasserPoints)) %>% 
  summarise(total_points = sum(PasserPoints, na.rm = TRUE))

write_csv(nfl_total_points, "nfl_total_passer_points.csv")



################################################################################
college_data <- read_csv("cfb_data.csv")

cfb_data_passes <- college_data %>% 
  mutate_at(vars(ThrowAccuracy, OnTargetThrow, Catchable, ThrowDepth, Completion), as.numeric) %>% 
  filter(ThrowAccuracy >= 0, Catchable >= 0, OnTargetThrow >= 0, PassingAttempt == 1)

# colleges have 0-4, no 5-6
#  only a few 7s but have non Null data
cfb_data_accuracy <- cfb_data_passes %>%
  select(Player, Season, ThrowAccuracy) %>% 
  mutate(accuracy7 = ifelse(ThrowAccuracy == 7, 1, 0),
         accuracy4 = ifelse(ThrowAccuracy == 4, 1, 0),
         accuracy3 = ifelse(ThrowAccuracy == 3, 1, 0),
         accuracy2 = ifelse(ThrowAccuracy == 2, 1, 0),
         accuracy1 = ifelse(ThrowAccuracy == 1, 1, 0),
         accuracy0 = ifelse(ThrowAccuracy == 0, 1, 0)) %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            total_accuracy7 = sum(accuracy7),
            total_accuracy4 = sum(accuracy4),
            total_accuracy3 = sum(accuracy3),
            total_accuracy2 = sum(accuracy2),
            total_accuracy1 = sum(accuracy1),
            total_accuracy0 = sum(accuracy0),
            accuracy7_percentage = total_accuracy7 / total_passes,
            accuracy4_percentage = total_accuracy4 / total_passes,
            accuracy3_percentage = total_accuracy3 / total_passes,
            accuracy2_percentage = total_accuracy2 / total_passes,
            accuracy1_percentage = total_accuracy1 / total_passes,
            accuracy0_percentage = total_accuracy0 / total_passes)

cfb_target <- cfb_data_passes %>% 
  select(Season, Player, OnTargetThrow) %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            on_target_perc = sum(OnTargetThrow) / total_passes)

cfb_catchable <- cfb_data_passes %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            catchable_pct = sum(Catchable) / total_passes)

cfb_short_acc <- cfb_data_passes %>%
  filter(ThrowDepth <= 9) %>% 
  group_by(Season, Player) %>% 
  summarise(total_short_passes = n(),
            on_target_short = sum(OnTargetThrow) / total_short_passes)

cfb_medium_acc <- cfb_data_passes %>% 
  filter(ThrowDepth > 10 & ThrowDepth <= 19) %>% 
  group_by(Season, Player) %>% 
  summarise(total_medium_passes = n(),
            on_target_medium = sum(OnTargetThrow) / total_medium_passes)

cfb_deep_acc <- cfb_data_passes %>% 
  filter(ThrowDepth > 19) %>% 
  group_by(Season, Player) %>% 
  summarise(total_deep_passes = n(),
            on_target_deep = sum(OnTargetThrow) / total_deep_passes)

cfb_comp_per <- cfb_data_passes %>% 
  group_by(Season, Player) %>% 
  summarise(total_passes = n(),
            comp_per = sum(Completion) / total_passes)


all_cfb_qb_data <- cfb_catchable %>% 
  left_join(cfb_data_accuracy, by = c("Player", "Season")) %>% 
  left_join(cfb_deep_acc, by = c("Player", "Season")) %>% 
  left_join(cfb_medium_acc, by = c("Player", "Season")) %>% 
  left_join(cfb_short_acc, by = c("Player", "Season")) %>% 
  left_join(cfb_target, by = c("Player", "Season")) %>% 
  left_join(cfb_comp_per, by = c("Player", "Season")) %>% 
  filter(total_passes.x >= 30) %>% 
  select(-c(total_passes.y, total_passes.x.x, total_passes.y.y))

write_csv(all_cfb_qb_data, "cfb_qb_data.csv")

##########################################################################
accuracy <- read_csv("nfl_qb_accuracy.csv")
decision_making <- read_csv("decision_making.csv")
improv <- read_csv("Improv.csv")
down_distance <- read_csv("total_air_yards_by_down_and_2_minute.csv")
points <- read_csv("nfl_total_passer_points.csv")

all_data <- points %>% 
  left_join(decision_making) %>% 
  left_join(improv) %>% 
  left_join(down_distance) %>% 
  left_join(accuracy) %>% 
  select(-total_passes.x)

write_csv(all_data, "all_qb_nfl_data.csv")

all_qb_nfl_data <- read_csv("all_qb_nfl_data.csv") %>% 
  distinct(Player)

all_cb_nfl_data <- read_csv("cfb_qb_data.csv") %>% 
  mutate(Player = ifelse(Player == "Gardner Minshew II", "Gardner Minshew", Player))

all_cb_nfl_data2 <- all_cb_nfl_data %>% 
  filter(Player %in% all_qb_nfl_data$Player) %>% 
  select(-total_passes.x) %>% 
  group_by(Player) %>% 
  mutate(last_season = max(Season)) %>% 
  filter(Season == last_season)

write_csv(all_cb_nfl_data2, "cfb_accuracy.csv")

####################################
cfb_accuracy <- read_csv("cfb_accuracy.csv")
cfb_decision <- read_csv("cfb_decision_making.csv") %>% 
  mutate(Player = ifelse(Player == "Gardner Minshew II", "Gardner Minshew", Player))

cfb_improv <- read_csv("Improv_cfb_filtered2.csv") %>% 
  group_by(Player) %>% 
  mutate(last_season = max(Season)) %>% 
  filter(Season == last_season)

cfb_situation <- read_csv("cfb_total_air_yards_by_down_and_2_minute_new.csv") %>% 
  group_by(Player) %>% 
  mutate(last_season = max(Season)) %>% 
  filter(Season == last_season)

all_cfb_data <- cfb_decision %>% 
  left_join(cfb_accuracy) %>% 
  left_join(cfb_situation) %>% 
  left_join(cfb_improv)

write_csv(all_cfb_data, "all_cfb_data_last_season.csv")

#########################################
all_cfb_last_season <- read_csv("all_cfb_data_last_season.csv") %>% 
  mutate(Season = Season + 1)
all_nfl_data <- read_csv("all_qb_nfl_data.csv")
clusters <- read_csv("nfl_qb_cluster.csv") %>% 
  select(-season)

all_data <- clusters %>% 
  left_join(all_nfl_data) %>% 
  na.omit() %>% 
  left_join(all_cfb_last_season, by = c("Player"))
  
write_csv(all_data, "ALL_QB_DATA.csv")


##################################################################
# interceptable passes is interceptions + dropped_interceptions

total_points <- nfl_data %>% 
  filter(RunType != "QB Kneel") %>% 
  mutate_at(vars(PasserPoints, RusherPoints), as.numeric) %>% 
  mutate(PasserPoints = ifelse(is.na(PasserPoints), 0, PasserPoints),
         RusherPoints = ifelse(is.na(RusherPoints), 0, RusherPoints)) %>% 
  mutate(pass_run_points = PasserPoints + RusherPoints) %>% 
  group_by(Player, Season) %>% 
  summarise(all_points = sum(pass_run_points))





total_passer_points <- nfl_data %>%  
  mutate_at(vars(PasserPoints), as.numeric) %>% 
  mutate(PasserPoints = ifelse(is.na(PasserPoints), 0, PasserPoints)) %>% 
  group_by(Player, Season) %>% 
  summarise(total_passer_points = sum(PasserPoints))

write_csv(total_passer_points, "total_passer_points.csv")


table(nfl_data$RunType)
#########################################################
nfl_data <- read_csv("nfl_data.csv")

rushes <- nfl_data %>% 
  filter(Carries == 1) %>% 
  mutate_at(vars(RushingYardsAfterContact, RushingTacklesBroken), as.numeric) %>% 
  group_by(Player, Season) %>% 
  summarise(total_rushes = n(),
            avg_rush_yds_att = sum(RushingYardsAfterContact) / total_rushes,
            avg_rush_tackles_broken_per = sum(RushingTacklesBroken) / total_rushes)

write_csv(rushes, "rushing_qb_data.csv")

###############################################################

#all_data <- read_csv("ALL_QB_DATA.csv") %>% 
 # select(-total_points)

cfb_leverage <- read_csv("cfb_comp_per_paststicks_down_2min.csv")
cfb_conv <- read_csv("cfb_dat_conv.csv")



all_cfb_last_season <- read_csv("all_cfb_data_last_season.csv") %>% 
  mutate(cluster = "College", yoe = -1, total_points = 0) %>% 
  select(-tot_hurries, -tot_sacks, -interceptions, -dropped_ints) %>% 
  left_join(cfb_leverage, by = c("Player", "Season")) %>% 
  left_join(cfb_conv)

################################################
all_nfl_data <- read_csv("all_qb_nfl_data.csv") %>% 
  select(-total_points)
clusters <- read_csv("nfl_qb_cluster.csv") %>% 
  select(Player, Season, cluster, yoe)
#rushing <- read_csv("rushing_qb_data.csv")
nfl_leverage <- read_csv("nfl_comp_per_paststicks_down_2min.csv")
passer_points <- read_csv("total_passer_points.csv")

nfl_data_new <- read_csv("nfl_data_new.csv")
nfl_data_new_clusters <- nfl_data_new %>% 
  left_join(clusters) %>% 
  left_join(passer_points)

write_csv(nfl_data_new_clusters, "nfl_data_new.csv")

nfl <- clusters %>% 
  left_join(all_nfl_data, by = c("Season", "Player")) %>% 
  left_join(passer_points) %>% 
#  left_join(rushing) %>% 
  left_join(nfl_leverage) %>% 
  select(-c(interceptions, dropped_ints)) %>% 
  rename(avg_comp_3rd = nfl_avg_comp_3rd, avg_comp_4th = nfl_avg_comp_4th,
         avg_comp_2min = nfl_avg_comp_2min) %>% 
  select(-c(total_accuracy7, total_accuracy4, total_accuracy3,
            total_accuracy2, total_accuracy1, total_accuracy0,
            total_accuracy5, total_accuracy6, accuracy7_percentage,
            accuracy4_percentage, accuracy3_percentage, accuracy2_percentage,
            accuracy1_percentage, accuracy0_percentage, accuracy6_percentage,
            accuracy5_percentage, total_deep_passes,
            total_medium_passes, total_short_passes, on_target_perc,
            total_air_yards_2min, total_air_yards_down_1,
            total_air_yards_down_2, total_air_yards_down_3,
            total_air_yards_down_4, number_snaps_down_1,
            number_snaps_down_2, number_snaps_down_3,
            number_snaps_down_4, number_snaps_2min)) %>% 
  mutate(total_turnovers = fumbles_lost + interceptable_passes,
         turnover_rate = total_turnovers / number_pass_plays) %>% 
  select(-c(fumbles_lost, total_turnovers, number_pass_plays,
            interceptable_passes, batted_passes, ofp_ypa, ofp_ypc,
            avg_comp_4th, avg_comp_2min, fourth_down_conv, comp_per))

write_csv(nfl, "QB_NFL_DATA_GREAT.csv")




################################

nfl_college <- bind_rows(all_cfb_last_season, nfl)

nfl_college_all <- nfl_college %>% 
  select(-c(total_accuracy7, total_accuracy4, total_accuracy3,
            total_accuracy2, total_accuracy1, total_accuracy0,
            total_accuracy5, total_accuracy6, accuracy7_percentage,
            accuracy4_percentage, accuracy3_percentage, accuracy2_percentage,
            accuracy1_percentage, accuracy0_percentage, accuracy6_percentage,
            accuracy5_percentage, last_season, total_deep_passes,
            total_medium_passes, total_short_passes, on_target_perc,
            total_air_yards_2min, total_air_yards_down_1,
            total_air_yards_down_2, total_air_yards_down_3,
            total_air_yards_down_4, number_snaps_down_1,
            number_snaps_down_2, number_snaps_down_3,
            number_snaps_down_4, number_snaps_2min))





%>% 
  arrange(Player, Season) %>% 
  mutate(lag_pass_plays = ifelse(Player == lag(Player), lag(number_pass_plays), 0),
         lag_fumbles_lost = ifelse(Player == lag(Player), lag(fumbles_lost), 0),
         lag_batted_passes = ifelse(Player == lag(Player), lag(batted_passes), 0),
         lag_interceptable_passes = ifelse(Player == lag(Player), lag(interceptable_passes), 0),
         lag_catchable_pct = ifelse(Player == lag(Player), lag(catchable_pct), 0),
         lag_ontarget_deep = ifelse(Player == lag(Player), lag(on_target_deep), 0),
         lag_ontarget_medium = ifelse(Player == lag(Player), lag(on_target_medium), 0),
         lag_ontarget_short = ifelse(Player == lag(Player), lag(on_target_short), 0),
         lag_comp_per = ifelse(Player == lag(Player), lag(comp_per), 0),
         lag_h2s = ifelse(Player == lag(Player), lag(h2s), 0),
         lag_ofp_cmp = ifelse(Player == lag(Player), lag(ofp_cmp), 0),
         lag_ofp_ypc = ifelse(Player == lag(Player), lag(ofp_ypc), 0),
         lag_ofp_ypa = ifelse(Player == lag(Player), lag(ofp_ypa), 0),
         lag_conf = ifelse(Player == lag(Player), lag(OffConfName), 0),
         lag_third_down_comp_per = ifelse(Player == lag(Player), lag(avg_comp_3rd), 0),
         lag_fourth_down_comp_per = ifelse(Player == lag(Player), lag(avg_comp_4th), 0),
         lag_two_min_comp_per = ifelse(Player == lag(Player), lag(avg_comp_2min), 0),
         lag_third_down_conv_per = ifelse(Player == lag(Player), lag(third_down_conv), 0),
         lag_fourth_down_conv_per = ifelse(Player == lag(Player), lag(fourth_down_conv),0)) %>% 
  select(Player, Season, lag_conf,
         lag_pass_plays,
         lag_fumbles_lost,
         lag_batted_passes,
         lag_interceptable_passes,
         lag_catchable_pct,
         lag_ontarget_deep,
         lag_ontarget_medium, lag_ontarget_short, lag_comp_per,
         lag_h2s, lag_ofp_cmp,
         lag_ofp_ypc, lag_ofp_ypa, cluster, yoe, total_passer_points,
         lag_third_down_comp_per, lag_fourth_down_comp_per,
         lag_two_min_comp_per, lag_third_down_conv_per,
         lag_fourth_down_conv_per) %>% 
  filter(yoe > -1)

#write_csv(nfl_college_lag, "ALL_DATA_WITH_LAG.csv")
         
###############################################################
all_data <- read_csv("ALL_DATA_WITH_LAG.csv") %>% 
  mutate(lag_total_turnovers = lag_fumbles_lost + lag_interceptable_passes,
         lag_turnover_rate = lag_total_turnovers / lag_pass_plays) %>% 
  select(-c(lag_fumbles_lost, lag_interceptable_passes, lag_total_turnovers,
            lag_batted_passes, lag_pass_plays,
         lag_ofp_ypc, lag_ofp_ypa, lag_two_min_comp_per,
         lag_third_down_conv_per, lag_comp_per, lag_fourth_down_conv_per,
         lag_fourth_down_comp_per))


########################
all_data <- read_csv("nfl_data_new2.csv") %>% 
  select(-UniversalPlayerId, -pass_att, -plays, -catchable_pct,
         -on_target_med) %>% 
  mutate(tackles_missed_rate = tackles_missed_rate / 100)

clusters <- read_csv("nfl_qb_cluster.csv") %>% 
  select(Player, Season, cluster, yoe)

passer_points <- read_csv("total_passer_points.csv")

all_data <- all_data %>% 
  left_join(clusters) %>% 
  left_join(passer_points)


ggplot(all_data, aes(y = total_passer_points, x = to_rate)) +
  geom_point()
  

# west coast
wc <- all_data %>% 
  dplyr::filter(cluster == "West Coast") %>%
  dplyr::select(-c(Player, Season, cluster, yoe))

set.seed(200)
index <- createDataPartition(wc$total_passer_points, p = 0.5, list = FALSE)
train_data <- wc[index, ]
test_data <- wc[-index, ]



# spread
spread <- all_data %>% 
  filter(cluster == "Spread") %>% 
  select(-c(Player, Season, cluster, yoe))

set.seed(123)
index <- createDataPartition(spread$total_passer_points, p = 0.5, list = FALSE)
train_data <- spread[index, ]
test_data <- spread[-index, ]



# bully ball
bb <- all_data %>% 
  filter(cluster == "Bully Ball") %>% 
  na.omit() %>% 
  select(-c(Player, Season, cluster, yoe))

set.seed(123)
index <- createDataPartition(bb$total_passer_points, p = 0.5, list = FALSE)
train_data <- bb[index, ]
test_data <- bb[-index, ]




model_y0 <- lm(total_passer_points ~ ., data = wc)
summary(model_y0)

predictions <- predict(model_y0, newdata = test_data %>% select(-total_passer_points))

rmse <- postResample(predictions, test_data$total_passer_points)
rmse


library(car)
vif(model_y0)



# Define parameter grid
param_grid <- expand.grid(
  nrounds = c(500, 5000), # Number of boosting rounds
  max_depth = c(2, 3, 4), # Maximum depth of trees
  eta = c(0.1, 0.3, 0.5),     # Learning rate
  gamma = c(0.1, 0.3, 0.5), # Minimum loss reduction required to make a further partition
  colsample_bytree = 1, # Subsample ratio of columns when constructing each tree
  min_child_weight = c(1, 2, 3), # Minimum sum of instance weight (hessian) needed in a child
  subsample = 1
)

ctrl <- trainControl(
  method = "cv",           # Cross-validation
  number = 5,              # Number of folds
  verboseIter = TRUE,      # Print progress
  allowParallel = TRUE     # Allow parallel processing
)

xgb_grid <- train(
  x = wc %>% select(-total_passer_points),   # Features
  y = wc$total_passer_points,   # Target variable
  method = "xgbTree",          # XGBoost
  trControl = ctrl,            # Control parameters
  tuneGrid = param_grid        # Parameter grid
)

# Best model
wc_model <- xgb_grid$finalModel

# save the model
saveRDS(wc_model, file = "xgb_wc_model3.rds")

print("wc done")


xgb_grid <- train(
  x = spread %>% select(-total_passer_points),   # Features
  y = spread$total_passer_points,   # Target variable
  method = "xgbTree",          # XGBoost
  trControl = ctrl,            # Control parameters
  tuneGrid = param_grid        # Parameter grid
)

# Best model
spread_model <- xgb_grid$finalModel

# save the model
saveRDS(spread_model, file = "xgb_spread_model3.rds")

print("spread done")



xgb_grid <- train(
  x = bb %>% select(-total_passer_points),   # Features
  y = bb$total_passer_points,   # Target variable
  method = "xgbTree",          # XGBoost
  trControl = ctrl,            # Control parameters
  tuneGrid = param_grid        # Parameter grid
)

# Best model
bb_model <- xgb_grid$finalModel

# save the model
saveRDS(bb_model, file = "xgb_bb_model3.rds")

# Make predictions on the test set
predictions <- predict(xgb_grid, newdata = bb %>% select(-total_passer_points))

rmse <- postResample(predictions, bb$total_passer_points)
rmse

pred_acutal <- data.frame(predictions, spread$total_passer_points)


xgb_model <- xgb_grid$finalModel

importance_bb0 <- xgb.importance(feature_names = colnames(bb %>% 
                                                        select(-total_passer_points)), 
                             model = bb_model)
importance_spread0 <- xgb.importance(feature_names = colnames(spread %>% 
                                                           select(-total_passer_points)), 
                                model = spread_model)
importance_wc0 <- xgb.importance(feature_names = colnames(wc %>% 
                                                           select(-total_passer_points)), 
                                model = wc_model)

xgb.plot.importance(importance_matrix = importance_wc0)




importance_bb <- as.data.frame(cbind("Var" = importance_bb0$Feature, 
                                     "BullyBall" = importance_bb0$Gain)) %>% 
  arrange(Var)

importance_spread <- as.data.frame(cbind("Var" = importance_spread0$Feature, 
                                     "Spread" = importance_spread0$Gain)) %>% 
  arrange(Var)

importance_wc <- as.data.frame(cbind("Var" = importance_wc0$Feature, 
                                     "WestCoast" = importance_wc0$Gain)) %>% 
  arrange(Var)

all_importance <- importance_bb %>% 
  left_join(importance_spread) %>% 
  left_join(importance_wc) %>% 
  mutate_at(vars(`BullyBall`, `Spread`, `WestCoast`), as.numeric) %>% 
  mutate(group = case_when(
    Var == "on_target_deep" | Var == "on_target_short" ~ "accuracy",
    Var == "to_rate" | Var == "third_fourth_comp_pct" ~ "decision_making",
    Var == "hurry_sack_ratio" | Var == "tackles_missed_rate" ~ "qb_mobility",
    TRUE ~ "improv"
  )) %>% 
  select(-Var)

new_importance <- all_importance %>% 
  group_by(group) %>% 
  summarise()





# Convert the data from wide to long format
data_long <- tidyr::gather(all_importance, key = "Variable", value = "Value", -group) %>% 
  group_by(group, Variable) %>% 
  summarise(total_imp = sum(Value))

custom_colors <- c("BullyBall" = "blue", "WestCoast" = "black", "Spread" = "green")
custom_labels <- c("Accuracy", "Decision Making", "Improvisation", "QB Mobility")

west <- data_long %>% 
  filter(Variable == "WestCoast")

spread <- data_long %>% 
  filter(Variable == "Spread")

bully_ball <- data_long %>% 
  filter(Variable == "BullyBall")


# Create the bar plot using ggplot2
ggplot(bully_ball, aes(x = group, y = total_imp, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black",
           show.legend = FALSE, width = 0.5) +
  labs(title = "Importance in West Coast Model", x = "Variable", y = "Importance", fill = "Variable") +
  scale_fill_manual(values = "blue") +
  scale_x_discrete(labels = custom_labels) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24)) +
  labs(title = NULL,
       fill = NULL) +
  coord_flip()
ggsave("bully_ball_importance_graph.png", width = 12.5, height = 10)


# make a plot of passer points distribution
ggplot(data = all_data) +
  geom_histogram(aes(x = total_passer_points, y = ..density..), fill = "white", color = "black", bins = 30) +
  geom_density(aes(x = total_passer_points), color = "red", fill = "red", alpha = 0.25) +
  labs(title = "Distribution of Total Passer Points",
       x = "Total Passer Points",
       y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggsave("total_passer_points.png", width = 15, height = 10)



# Best model
wc_model <- xgb_grid$finalModel

# save the model
saveRDS(spread_model, file = "xgb_spread_model2.rds")

rookies <- read_csv("rookies2.csv")

# read in the saved model
bb_model <- read_rds("xgb_bb_model3.rds")
spread_model <- read_rds("xgb_spread_model3.rds")
wc_model <- read_rds("xgb_wc_model3.rds")


rookies2 <- rookies %>% 
  filter(yoe >= 0) %>% 
  select(-c(UniversalPlayerId, Season, plays, pass_att, level))

rookies_pred <- rookies2 %>% 
  select(-c(Player, yoe, rush_att))

bb_matrix <- xgb.DMatrix(data = as.matrix(rookies_pred))

bb_predictions <- predict(bb_model, newdata = bb_matrix)

bb_pred_frame <- as.data.frame(cbind(rookies2$Player, rookies2$yoe, bb_predictions))
bb_pred_frame$bb_predictions <- as.numeric(bb_pred_frame$bb_predictions)

#write_csv(bb_pred_frame, "bb_pred_frame3.csv")

spread_matrix <- xgb.DMatrix(data = as.matrix(rookies_pred))

spread_predictions <- predict(spread_model, newdata = spread_matrix)

spread_pred_frame <- as.data.frame(cbind(rookies2$Player, rookies2$yoe, spread_predictions))
spread_pred_frame$spread_predictions <- as.numeric(spread_pred_frame$spread_predictions)

#write_csv(spread_pred_frame, "spread_pred_frame3.csv")

wc_matrix <- xgb.DMatrix(data = as.matrix(rookies_pred))

wc_predictions <- predict(wc_model, newdata = wc_matrix)

wc_pred_frame <- as.data.frame(cbind(rookies2$Player, rookies2$yoe, wc_predictions))
wc_pred_frame$wc_predictions <- as.numeric(wc_pred_frame$wc_predictions)

#write_csv(wc_pred_frame, "wc_pred_frame3.csv")


# 69.7402 + 2.7295 * sqrt(abs(total_passer_points)) * sign(total_passer_points))
wc_pred_frame$QBR <- 69.7402 + 2.7295 * sqrt(abs(wc_pred_frame$wc_predictions)) *
                                              sign(wc_pred_frame$wc_predictions)

bb_pred_frame$QBR <- 69.7402 + 2.7295 * sqrt(abs(bb_pred_frame$bb_predictions)) *
  sign(bb_pred_frame$bb_predictions)

spread_pred_frame$QBR <- 69.7402 + 2.7295 * sqrt(abs(spread_pred_frame$spread_predictions)) *
  sign(spread_pred_frame$spread_predictions)

###############
wc_pred_frame <- read_csv("wc_pred_frame3.csv") %>% 
  select("player" = V1, "Year" = V2, "Total_Passer_Points" = wc_predictions) %>% 
  mutate(Year = Year + 1)


wc_player_order <- c("Jayden Daniels", "Bo Nix", "J.J. McCarthy",
                     "Drake Maye", "Caleb Williams", "Michael Penix Jr.")

# Convert Player_Name to a factor with the desired order
wc_pred_frame$player <- factor(wc_pred_frame$player, levels = wc_player_order)


# Create separate line plots for each player
ggplot(wc_pred_frame, aes(x = Year, y = Total_Passer_Points, group = player, color = player)) +
  geom_line(size = 2) +
  labs(title = "Total Passer Points Over Years in West Coast Scheme", x = "Year", y = "Total Passer Points") +
  scale_x_continuous(breaks = seq(min(wc_pred_frame$Year), max(wc_pred_frame$Year), by = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values = player_colors) +
  theme_bw() +
  theme(legend.title = element_text(size = 24),  # Adjust legend title size here
        legend.text = element_text(size = 20),
        legend.key.size = unit(7, "lines"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  labs(color = "Player Ranking",
       title = NULL)
ggsave("wc_projection_over_time.png", width = 15, height = 10)


spread_pred_frame <- read_csv("spread_pred_frame3.csv") %>% 
  select("player" = V1, "Year" = V2, "Total_Passer_Points" = spread_predictions) %>% 
  mutate(Year = Year + 1)

spread_player_order <- c("J.J. McCarthy", "Caleb Williams", "Bo Nix",
                     "Jayden Daniels", "Drake Maye", "Michael Penix Jr.")

# Convert Player_Name to a factor with the desired order
spread_pred_frame$player <- factor(spread_pred_frame$player, levels = spread_player_order)

# Create separate line plots for each player
ggplot(spread_pred_frame, aes(x = Year, y = Total_Passer_Points, group = player, color = player)) +
  geom_line(size = 2) +
  labs(title = "Total Passer Points Over Years in Spread Scheme", x = "Year", y = "Total Passer Points") +
  scale_x_continuous(breaks = seq(min(spread_pred_frame$Year), max(spread_pred_frame$Year), by = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values = player_colors) +
  theme_bw() +
  theme(legend.title = element_text(size = 24),  # Adjust legend title size here
        legend.text = element_text(size = 20),
        legend.key.size = unit(7, "lines"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  labs(color = "Player Ranking",
       title = NULL)
ggsave("spread_projection_over_time.png", width = 15, height = 10)

bb_pred_frame <- read_csv("bb_pred_frame3.csv") %>% 
  select("player" = V1, "Year" = V2, "Total_Passer_Points" = bb_predictions) %>% 
  mutate(Year = Year + 1)

bb_player_order <- c("Bo Nix", "Jayden Daniels", "J.J. McCarthy",
                         "Michael Penix Jr.", "Caleb Williams", "Drake Maye")

# Convert Player_Name to a factor with the desired order
bb_pred_frame$player <- factor(bb_pred_frame$player, levels = bb_player_order)

player_colors <- c("Caleb Williams" = "#9D2235", "J.J. McCarthy" = "#00274C", "Bo Nix" = "forestgreen",
                   "Jayden Daniels" = "#FDD023", "Michael Penix Jr." = "darkviolet", "Drake Maye" = "#7BAFD4")

# Create separate line plots for each player
ggplot(bb_pred_frame, aes(x = Year, y = Total_Passer_Points, group = player, color = player)) +
  geom_line(size = 2) +
  labs(title = "Total Passer Points Over Years in Bully Ball Scheme", x = "Year", y = "Total Passer Points") +
  scale_x_continuous(breaks = seq(min(bb_pred_frame$Year), max(bb_pred_frame$Year), by = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  scale_color_manual(values = player_colors) +
  theme_bw() +
  theme(legend.title = element_text(size = 24),  # Adjust legend title size here
        legend.text = element_text(size = 20),
        legend.key.size = unit(7, "lines"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  labs(color = "Player Ranking",
       title = NULL)
  
ggsave("bb_projection_over_time.png", width = 15, height = 10)

############################################

# Initialize an empty dataframe to store predictions
all_predictions <- as.data.frame(test_data$total_passer_points)

# Run the code in a loop 10 times
for (i in 1:6) {
  xgb_grid <- train(
    x = train_data %>% select(-total_passer_points),   # Features
    y = train_data$total_passer_points,               # Target variable
    method = "xgbTree",                               # XGBoost
    trControl = ctrl,                                  # Control parameters
    tuneGrid = param_grid                              # Parameter grid
  )
  
  # Make predictions on the test set
  predictions <- predict(xgb_grid, newdata = test_data %>% select(-total_passer_points))
  
  # Assign iteration number to predictions
  predictions <- data.frame(Iteration = i, Predictions = predictions)
  
  # Append predictions to all_predictions dataframe
  all_predictions <- cbind(all_predictions, predictions)
}

names(all_predictions) <- c("test_data", "it1", "pred1",
                            "it2", "pred2", "it3", "pred3",
                            "it4", "pred4", "it5", "pred5",
                            "it6", "pred6")

all_predictions2 <- all_predictions %>% 
  mutate(avg_pred = (pred1 + pred2 + pred3 + pred4 + pred5 + pred6) / 6)

write_csv(all_predictions2, "test_predictions.csv")

library(ModelMetrics)
rmse(all_predictions2$test_data, all_predictions2$avg_pred)
#####################################################################
