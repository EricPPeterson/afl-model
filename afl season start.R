library(fitzRoy)
library(dplyr)
library(lookup)
library(ggplot2)
library(stringr)
library(randomForest)
library(caret)
library(lookup)

#fetch historical odds
afl_historical_odds <- read.csv("~/GitHub/afl model/afl_historical_odds.csv")
afl_historical_odds$season <- as.Date(afl_historical_odds$Date)
afl_historical_odds$season <- format(afl_historical_odds$season, format = '%Y')
afl_historical_odds_join <- afl_historical_odds %>% filter(season == 2022)
afl_historical_odds_join <- afl_historical_odds_join %>% select(-c(season))
afl_historical_odds_join <- afl_historical_odds_join %>% slice(9:207)

#change Home.Team/Away.Team colnames to home_team/away_team
colnames(afl_historical_odds_join)[3] <- 'home_team'
colnames(afl_historical_odds_join)[4] <- 'away_team'
colnames(afl_historical_odds)[3] <- 'home_team'
colnames(afl_historical_odds)[4] <- 'away_team'


afl_historical_odds$Date <- as.Date(afl_historical_odds$Date)

#pull game statistics for five seasons
fry_stats <- get_fryzigg_stats(start = 2016, end = 2021)
fry_stats <- fry_stats %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_stats$season <- format(fry_stats$season, format = '%Y')

fry_stats$opposition <- NA

#create a column for who the opposition was in every game
#will allow me to see what teams give up lots of shots
for(i in 1: nrow(fry_stats)) {
  if(fry_stats$player_team[i] == fry_stats$match_home_team[i]){
    fry_stats$opposition[i] <- fry_stats$match_away_team[i]
  } else {
    fry_stats$opposition[i] <- fry_stats$match_home_team[i]
  }
}


stats_grouped <- fry_stats %>% group_by(match_id, player_team) %>%
  summarise(game_shots = sum(shots_at_goal),
            game_ins = sum(inside_fifties),
            game_kicks = sum(kicks),
            game_marks = sum(marks),
            game_handballs = sum(handballs),
            game_eff_disposals = sum(effective_disposals),
            game_disposals = sum(disposals),
            game_clearances = sum(clearances),
            game_FK_for = sum(free_kicks_for),
            game_FK_against = sum(free_kicks_against),
            game_uncontested = sum(uncontested_possessions),
            game_cont_marks = sum(contested_marks),
            game_marks_50 = sum(marks_inside_fifty),
            game_hitouts = sum(hitouts),
            game_tackles = sum(tackles),
            game_tack_ins_fifty = sum(tackles_inside_fifty),
            game_rebounds = sum(rebounds),
            game_cont_possessions = sum(contested_possessions),
            
            )


#check for NANs
which(is.na(stats_grouped))
colnames(stats_grouped)[colSums(is.na(stats_grouped)) > 0]


#impute
stats_grouped_impute <- mean(na.omit(stats_grouped$game_eff_disposals))

for(i in 1:nrow(stats_grouped)){
  if(is.na(stats_grouped$game_eff_disposals[i])){
    stats_grouped$game_eff_disposals[i] = stats_grouped_impute
  }
}

#feature engineering
stats_grouped$pct_mks_in_50 <- stats_grouped$game_marks_50 / stats_grouped$game_marks
stats_grouped$pct_cont_marks <- stats_grouped$game_cont_marks / stats_grouped$game_marks
stats_grouped$ins_times_50s <- stats_grouped$game_ins * stats_grouped$game_marks_50
stats_grouped$rebound_ins_comb <- stats_grouped$game_rebounds * stats_grouped$game_ins

#attach weather and stadium
stats_grouped$weather <- lookup(stats_grouped$match_id, fry_stats$match_id, fry_stats$match_weather_type)
stats_grouped$temp <- lookup(stats_grouped$match_id, fry_stats$match_id, fry_stats$match_weather_temp_c)
stats_grouped$stadium <- lookup(stats_grouped$match_id, fry_stats$match_id, fry_stats$venue_name)

#remove match_id and player_team (grouping IDs)
stats_grouped <- stats_grouped %>% ungroup()
stats_grouped <- stats_grouped %>% select(-c(match_id, player_team))


#check histograms of variables to see if any are skewed
p_shots <- ggplot(data = stats_grouped, aes(x = game_shots)) + geom_histogram()
p_ins <- ggplot(data = stats_grouped, aes(x = game_ins)) + geom_histogram()
p_kicks <- ggplot(data = stats_grouped, aes(x = game_kicks)) + geom_histogram()
p_marks <- ggplot(data = stats_grouped, aes(x = game_marks)) + geom_histogram()

#build random forest for game shots
#test and train data
rf_data <- stats_grouped
rf_data$random <- runif(n = nrow(rf_data))
rf_train <- rf_data %>% filter(random <= 0.8)
rf_test <- rf_data %>% filter(random > 0.8)
rf_train <- rf_train %>% select(-c(random))
rf_test <- rf_test %>% select(-c(random))

#create rf
rf_shots <- randomForest(game_shots~., data = rf_train, na.action = na.omit)
rf_imp <- as.data.frame(rf_shots$importance)
rf_imp <- cbind(variables = rownames(rf_imp), rf_imp)
rf_imp <- arrange(rf_imp, desc(IncNodePurity))


p <- ggplot(data = rf_imp, aes(x=variables, y=IncNodePurity)) + geom_bar(stat = 'identity') + 
  coord_flip()
p

#predict values
y_hats <- predict(object = rf_shots, newdata = rf_test[,-1])

#check RMSE
root_error <- sqrt((sum(y_hats-rf_test$game_shots)^2)/length(y_hats))
cat('RMSE on testing data: ', round(root_error, 4), ' shots',  sep='')
#RMSE on testing data: 1.7833 shots

#build prediction for 2022
fry_test <- get_fryzigg_stats(start = 2022, end = 2022)
fry_test <- fry_test %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_test$season <- format(fry_test$season, format = '%Y')

fry_test$opposition <- NA

#create a column for who the opposition was in every game
#will allow me to see what teams give up lots of shots
for(i in 1: nrow(fry_test)) {
  if(fry_test$player_team[i] == fry_test$match_home_team[i]){
    fry_test$opposition[i] <- fry_test$match_away_team[i]
  } else {
    fry_test$opposition[i] <- fry_test$match_home_team[i]
  }
}

#function to create team stats
test_grouped <- function(df){
  out <- df %>% group_by(match_id, player_team) %>%
  summarise(game_ins = sum(inside_fifties),
            game_kicks = sum(kicks),
            game_marks = sum(marks),
            game_handballs = sum(handballs),
            game_eff_disposals = sum(effective_disposals),
            game_disposals = sum(disposals),
            game_clearances = sum(clearances),
            game_FK_for = sum(free_kicks_for),
            game_FK_against = sum(free_kicks_against),
            game_uncontested = sum(uncontested_possessions),
            game_cont_marks = sum(contested_marks),
            game_marks_50 = sum(marks_inside_fifty),
            game_hitouts = sum(hitouts),
            game_tackles = sum(tackles),
            game_tack_ins_fifty = sum(tackles_inside_fifty),
            game_rebounds = sum(rebounds),
            game_cont_possessions = sum(contested_possessions),
            
  )
  
  stats_grouped_impute <- mean(na.omit(out$game_eff_disposals))
  
  #impute
  for(i in 1:nrow(out)){
    if(is.na(out$game_eff_disposals[i])){
      out$game_eff_disposals[i] = stats_grouped_impute
      out$game_eff_disposals[i] = stats_grouped_impute
    }
  }
  
  #feature engineering
  out$pct_mks_in_50 <- test_grouped$game_marks_50 / test_grouped$game_marks
  out$pct_cont_marks <- test_grouped$game_cont_marks / test_grouped$game_marks
  out$ins_times_50s <- test_grouped$game_ins * test_grouped$game_marks_50
  out$rebound_ins_comb <- test_grouped$game_rebounds * test_grouped$game_ins
  #attach weather and stadium
  out$weather <- lookup(out$match_id, dft$match_id, df$match_weather_type)
  out$temp <- lookup(out$match_id, df$match_id, df$match_weather_temp_c)
  out$stadium <- lookup(out$match_id, df$match_id, df$venue_name)
  
  
  return(out)
}

#function to create opposition stats  
opposition_grouped <- function(df){
  out <- df %>% group_by(match_id, opposition) %>%
  summarise(game_ins = sum(inside_fifties),
            game_kicks = sum(kicks),
            game_marks = sum(marks),
            game_handballs = sum(handballs),
            game_eff_disposals = sum(effective_disposals),
            game_disposals = sum(disposals),
            game_clearances = sum(clearances),
            game_FK_for = sum(free_kicks_for),
            game_FK_against = sum(free_kicks_against),
            game_uncontested = sum(uncontested_possessions),
            game_cont_marks = sum(contested_marks),
            game_marks_50 = sum(marks_inside_fifty),
            game_hitouts = sum(hitouts),
            game_tackles = sum(tackles),
            game_tack_ins_fifty = sum(tackles_inside_fifty),
            game_rebounds = sum(rebounds),
            game_cont_possessions = sum(contested_possessions),
            
  )
  
  stats_grouped_impute <- mean(na.omit(out$game_eff_disposals))
  
  #impute
  for(i in 1:nrow(out)){
    if(is.na(out$game_eff_disposals[i])){
      out$game_eff_disposals[i] = stats_grouped_impute
      out$game_eff_disposals[i] = stats_grouped_impute
    }
  }
  
  #feature engineering
  out$pct_mks_in_50 <- out$game_marks_50 / opposition_grouped$game_marks
  out$pct_cont_marks <- out$game_cont_marks / opposition_grouped$game_marks
  out$ins_times_50s <- out$game_ins * opposition_grouped$game_marks_50
  out$rebound_ins_comb <- out$game_rebounds * opposition_grouped$game_ins
  #attach weather and stadium
  out$weather <- lookup(out$match_id, fry_test$match_id, df$match_weather_type)
  out$temp <- lookup(out$match_id, fry_test$match_id, df$match_weather_temp_c)
  out$stadium <- lookup(out$match_id, fry_test$match_id, df$venue_name)
  
  return(out)
}


#predict shots
rf_shots <- randomForest(game_shots~., data = stats_grouped[,-1])
test_grouped$game_shots <- predict(object = rf_shots, newdata = test_grouped[-c(1:2)])
opposition_grouped$game_shots <- predict(object = rf_shots, newdata = opposition_grouped[-c(1:2)])

#mean predicted shots 
shots_prior <- test_grouped %>% group_by(player_team) %>%
  mutate(mean_shots = mean(game_shots),
         var_shots = var(game_shots)) %>%
  select(player_team, mean_shots, var_shots)
opposition_prior <- opposition_grouped %>% group_by(opposition) %>%
  mutate(mean_shots_opposition = mean(game_shots),
         var_shots_opposition = var(game_shots)) %>%
  select(opposition, mean_shots_opposition, var_shots_opposition)
colnames(opposition_prior)[1] <- 'player_team'

#combine 2 prior dfs
shots_prior <- left_join(shots_prior, opposition_prior, by = c('player_team'))
shots_prior <- unique(shots_prior)

#check points / shot
pts_per_kick_weather <- fry_stats %>% dplyr :: group_by(match_weather_type) %>%
  dplyr :: summarise(pts_per_shot = ((6*sum(goals) + sum(behinds)) / sum(shots_at_goal)),
                     total_games = n())
pts_per_kick_venue <- fry_stats %>% dplyr :: group_by(venue_name) %>%
  dplyr :: summarise(pts_per_shot = ((6*sum(goals) + sum(behinds)) / sum(shots_at_goal)),
                     total_games = n())
pts_per_kick_temp <- fry_stats %>% dplyr :: group_by(match_weather_temp_c) %>%
  dplyr :: summarise(pts_per_shot = ((6*sum(goals) + sum(behinds)) / sum(shots_at_goal)),
                     total_games = n())

point_plot <- ggplot(data = pts_per_kick_temp, aes(x = match_weather_temp_c, y = pts_per_shot)) + geom_line()
point_weather <- ggplot(data = pts_per_kick_weather, aes(x = match_weather_type, y = pts_per_shot)) + 
  geom_point() + coord_flip()


#do random forest for pts_per_shot
pts_per_shot_forest <- fry_stats %>% dplyr :: group_by(match_id) %>%
  dplyr :: summarise(pts_per_shot = ((6*sum(goals) + sum(behinds)) / sum(shots_at_goal))) 
pts_per_shot_forest$weather <- lookup(pts_per_shot_forest$match_id, fry_stats$match_id, fry_stats$match_weather_type)
pts_per_shot_forest$venue <- lookup(pts_per_shot_forest$match_id, fry_stats$match_id, fry_stats$venue_name)
pts_per_shot_forest$temp <- lookup(pts_per_shot_forest$match_id, fry_stats$match_id, fry_stats$match_weather_temp_c)

#create test and training sets
pts_per_shot_forest$random <- runif(n = nrow(pts_per_shot_forest))
pts_per_shot_test <- pts_per_shot_forest %>% filter(random > 0.80)
pts_per_shot_train <- pts_per_shot_forest %>% filter(random <= 0.80)
pts_per_shot_test <- pts_per_shot_test %>% select(-c(random))
pts_per_shot_train <- pts_per_shot_train %>% select(-c(random))


rf_pts_per <- randomForest(pts_per_shot~., data = pts_per_shot_train[,-1])
pts_per_imp <- as.data.frame(rf_pts_per$importance)
pts_per_imp <- cbind(variables = rownames(pts_per_imp), pts_per_imp)
pts_per_imp <- arrange(pts_per_imp, desc(IncNodePurity))

#predict values
shots_hats <- predict(object = rf_pts_per, newdata = pts_per_shot_test[,-c(1:2)])

#check RMSE
root_error_pts <- sqrt((sum(shots_hats-pts_per_shot_test$pts_per_shot)^2)/length(shots_hats))
cat('RMSE on testing data: ', round(root_error_pts, 4), ' pts per shot',  sep='')
#RMSE on testing data: 0.2264 shots

var_pts_per_shot <- var(pts_per_shot_forest$pts_per_shot)

#covariance matrix for shots
cov_df <- fry_stats
cov_df$row_odd <- seq_len(nrow(cov_df)) %% 2 
cov_zero <- cov_df %>% filter(row_odd == 0)
colnames(cov_zero)[77] <- 'shots_at_goal_zero'
cov_zero_sum <- cov_zero %>% group_by(match_id) %>%
  summarise(sum_goals_zero = sum(shots_at_goal_zero))

cov_one <- cov_df %>% filter(row_odd == 1)
colnames(cov_one)[77] <- 'shots_at_goal_one'
cov_one_sum <- cov_one %>% group_by(match_id) %>%
  summarise(sum_goals_one = sum(shots_at_goal_one))
cov_comb <- left_join(cov_zero_sum, cov_one_sum, on = match_id) %>%
  select(c(match_id, sum_goals_zero, sum_goals_one)) %>%
  select(-c(match_id))

cov_shots_matrix <- cov(cov_comb)

#create game sims, both with independent pull and from mvn
season <- get_fryzigg_stats(start = 2022, end = 2022)
games_2022 <- fetch_fixture(season = 2022) %>% filter(round.roundNumber <= 23)


run_season <- function(season_df, sched, shot_func, opp_func){
  season_df <- season_df %>% filter(!match_round %in% c('Semi Finals', 'Preliminary Finals', 'Grand Final', 'Finals Week 1'))
  season_df$match_round <- as.numeric(season_df$match_round)
  

  for(i in 1:max(season_df$match_round)){
    season_week <- sched %>% filter(round.roundNumber == i)
    
    for(j in 1:nrow(season_week)){
      home_team <- season_week$home.team.name[j]
      away_team <- season_week$away.team.name[j]
      
      
      
      
    }
  }
  

}
