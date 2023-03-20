library(fitzRoy)
library(dplyr)
library(lookup)
library(ggplot2)
library(stringr)


#fetch fixtures
#get games to work on data from 2014 / 2020 (these are the years we have totals data for)
seasons <- c(2014:2020)
fixtures_afl <- data.frame()
for(i in 1:length(seasons)){
  current_season <- fetch_fixture(season = seasons[i])
  fixtures_afl <- bind_rows(fixtures_afl, current_season)
}

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

#pull historical odds
afl_historical_odds$Date <- as.Date(afl_historical_odds$Date)
afl_historical_odds$home_team[afl_historical_odds$home_team == 'Geelong'] <- 'Geelong Cats'
afl_historical_odds$home_team[afl_historical_odds$home_team == 'West Coast'] <- 'West Coast Eagles'
afl_historical_odds$home_team[afl_historical_odds$home_team == 'Adelaide'] <- 'Adelaide Crows'
afl_historical_odds$home_team[afl_historical_odds$home_team == 'Brisbane'] <- 'Brisbane Lions'

#create a totals column for historical odds
afl_historical_odds <- afl_historical_odds %>% 
  mutate(Total.Points = Home.Score + Away.Score)

#split out date to crop out years before 2014
afl_historical_odds[c('Year', 'Month', 'Day')] <- str_split_fixed(afl_historical_odds$Date, '-', 3)
afl_historical_odds <- afl_historical_odds %>% select(-c(Notes, Month, Day))

#count the total games that landed over and under the open/close/max/min values
afl_historical_odds$Open.Total <- ifelse(afl_historical_odds$Total.Points - afl_historical_odds$Total.Score.Open >= 0, 'Over', 'Under')
afl_historical_odds$Close.Total <- ifelse(afl_historical_odds$Total.Points - afl_historical_odds$Total.Score.Close >= 0, 'Over' , 'Under')
afl_historical_odds$Max.Total <- ifelse(afl_historical_odds$Total.Points - afl_historical_odds$Total.Score.Max >= 0, 'Over' , 'Under')
afl_historical_odds$Min.Total <- ifelse(afl_historical_odds$Total.Points - afl_historical_odds$Total.Score.Min >= 0, 'Over' , 'Under')

#calc the error between open/close/max/min
afl_historical_odds$Open.Error <- afl_historical_odds$Total.Score.Open - afl_historical_odds$Total.Points
afl_historical_odds$Close.Error <- afl_historical_odds$Total.Score.Close - afl_historical_odds$Total.Points
afl_historical_odds$Max.Error.Over <- afl_historical_odds$Total.Score.Max - afl_historical_odds$Total.Points
afl_historical_odds$Max.Error.Under <- afl_historical_odds$Total.Score.Min - afl_historical_odds$Total.Points

#count of overs and unders
open_table <- table(afl_historical_odds$Open.Total)
close_table <- table(afl_historical_odds$Close.Total)
max_table <- table(afl_historical_odds$Max.Total)
min_table <- table(afl_historical_odds$Min.Total)

#pct tables over and unders
open_table_pct <- round(open_table[1]/(open_table[1] + open_table[2]), 4)
close_table_pct <- round(close_table[1]/(close_table[1] + close_table[2]), 4)
max_table_pct <- round(max_table[1]/(close_table[1] + close_table[2]), 4)
min_table_pct <- round(min_table[1]/(min_table[1] + min_table[2]), 4)

#printout of how often games finish over and under certain numbers
print(paste0('The open total closes over ', open_table_pct, ' of the time.'))
print(paste0('The open total closes under ', 1-open_table_pct, ' of the time.'))
print(paste0('The close total closes over ', close_table_pct, ' of the time.'))
print(paste0('The close total closes under ', 1-close_table_pct, ' of the time.'))
print(paste0('The max total closes over max ', max_table_pct, ' of the time.'))
print(paste0('The max total closes under max ', 1-max_table_pct, ' of the time.'))
print(paste0('The min total closes over min ', min_table_pct, ' of the time.'))
print(paste0('The min total closes under min ', 1-min_table_pct, ' of the time.'))

#check by year
by_year_open <- table(afl_historical_odds$season, afl_historical_odds$Open.Total) 
by_year_close <- table(afl_historical_odds$season, afl_historical_odds$Close.Total)
by_year_open
by_year_close

#homefield advantage
afl_historical_odds <- afl_historical_odds %>% mutate(HFA = Home.Score - Away.Score)
HFA_by_year <- afl_historical_odds %>% group_by(season) %>%
  summarize(home_field_mean = mean(HFA),
            home_field_median = median(HFA))
HFA_by_year

#pull game statistics for five seasons
fry_stats <- get_fryzigg_stats(start = 2016, end = 2020)
fry_stats <- fry_stats %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_stats$season <- format(fry_stats$season, format = '%Y')
fry_stats <- fry_stats %>% filter(match_round != c('Elimination Final', 'Finals Week 1', 'Grand Final', 
                                                   'Preliminary Final', 'Preliminary Finals', 'Qualifying Finals',
                                                   'Semi Final', 'Semi Finals'))
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

#create stat for total shots taken by team per game
total_shots <- fry_stats %>% dplyr :: group_by(season, match_id, player_team) %>%
  dplyr :: summarise(total_shots_by_team = sum(shots_at_goal))

#mean shots by team per game
mean_shots <- fry_stats %>% dplyr :: group_by(season, player_team) %>% 
  dplyr :: summarise(mean_shots = sum(shots_at_goal)/ 22)

#mean shots conceded by team per game
mean_shots_opposition <- fry_stats %>% dplyr :: group_by(season, opposition) %>%
  dplyr :: summarise(mean_shots_by_opposition = sum(shots_at_goal) / 22)
colnames(mean_shots_opposition)[2] <- 'player_team'

total_shots <- left_join(total_shots, mean_shots, by = c('season', 'player_team'))
total_shots <- left_join(total_shots, mean_shots_opposition, by = c('season', 'player_team'))

#Points per shot by year
pts_per_shot <- fry_stats %>% dplyr :: group_by(season, player_team) %>%
  dplyr :: summarise(points_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal),
                     avg_shots = sum(shots_at_goal) / 22)

ggplot(data = pts_per_shot, aes(y = points_per_shot, x = season)) + geom_point(aes(colour = player_team))
ggplot(data = pts_per_shot, aes(y = avg_shots, x = season)) + geom_point(aes(colour = player_team))

#average pts per shot by season
avg_pts_season <- fry_stats %>% dplyr :: group_by(season) %>%
  dplyr :: summarise(points_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal),
                     total_shots = sum(shots_at_goal))

#build mixed effects model
fixtures_test <- fetch_fixture(season = 2021)

#pull game statistics for a decade
fry_test <- get_fryzigg_stats(start = 2021, end = 2021)
fry_test <- fry_test %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_test$season <- format(fry_test$season, format = '%Y')
fry_test <- subset(fry_test, match_round != 'Semi Finals')
fry_test <- subset(fry_test, match_round != 'Preliminary Finals')
fry_test <- subset(fry_test, match_round != 'Grand Final')
fry_test <- subset(fry_test, match_round != 'Finals Week 1')
fry_test$match_round <- as.numeric(fry_test$match_round)
fry_test$opposition <- NA
fry_test <- fry_test %>%
  mutate(pts_per_shot = (6*goals + behinds) / shots_at_goal)

#create column for opposition
for(i in 1: nrow(fry_test)) {
  if(fry_test$player_team[i] == fry_test$match_home_team[i]){
    fry_test$opposition[i] <- fry_test$match_away_team[i]
  } else {
    fry_test$opposition[i] <- fry_test$match_home_team[i]
  }
}

max_round <- max(fry_test$match_round)
shots_test <- fry_test %>% dplyr :: group_by(season, player_team) %>%
  dplyr :: summarise(total_shots_by_team = sum(shots_at_goal)/max_round,
                     var_shots_by_team = var(shots_at_goal))

shots_test_opposition <- fry_test %>% dplyr :: group_by(season, opposition) %>%
  dplyr :: summarise(mean_shots_by_opposition = sum(shots_at_goal) / max_round,
                     var_shots_by_opposition = var(shots_at_goal))
colnames(shots_test_opposition)[2] <- 'player_team'

model_2022 <- left_join(shots_test, shots_test_opposition, by = c('season', 'player_team'))
model_2022 <- model_2022 %>% 
  mutate(shots_differential = total_shots_by_team - mean_shots_by_opposition,
         shots_over_average = total_shots_by_team / mean(total_shots_by_team),
         def_over_average = mean_shots_by_opposition / mean(total_shots_by_team))
model_2022[7,2] <- 'Geelong Cats'
model_2022[9,2] <- 'GWS Giants'
model_2022[1,2] <- 'Adelaide Crows'
model_2022[17,2] <- 'West Coast Eagles'
model_2022[16,2] <- 'Sydney Swans'
model_2022[8,2] <- 'Gold Coast Suns'

#points per shot
pts_per_shot_2022 <- fry_test %>% dplyr :: group_by(season) %>%
  dplyr :: summarise(points_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal))

var_2022 <- fry_test %>% dplyr :: group_by(match_id) %>%
  dplyr :: mutate(pts_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal)) %>%
  dplyr :: select(match_id, pts_per_shot)
var_2022 <- unique(var_2022)

pts_per_shot_2022$var_pts <- var(var_2022$pts_per_shot)

#schedule 2022
games_2022 <- fetch_fixture(season = 2022)

#weeks of games
week_1 <- games_2022 %>% filter(round.abbreviation == 'Rd 1')
week_2 <- games_2022 %>% filter(round.abbreviation == 'Rd 2')
week_3 <- games_2022 %>% filter(round.abbreviation == 'Rd 3')
week_4 <- games_2022 %>% filter(round.abbreviation == 'Rd 4')
week_5 <- games_2022 %>% filter(round.abbreviation == 'Rd 5')
week_6 <- games_2022 %>% filter(round.abbreviation == 'Rd 6')
week_7 <- games_2022 %>% filter(round.abbreviation == 'Rd 7')
week_8 <- games_2022 %>% filter(round.abbreviation == 'Rd 8')
week_9 <- games_2022 %>% filter(round.abbreviation == 'Rd 9')
week_10 <- games_2022 %>% filter(round.abbreviation == 'Rd 10')
week_11 <- games_2022 %>% filter(round.abbreviation == 'Rd 11')
week_12 <- games_2022 %>% filter(round.abbreviation == 'Rd 12')
week_13 <- games_2022 %>% filter(round.abbreviation == 'Rd 13')
week_14 <- games_2022 %>% filter(round.abbreviation == 'Rd 14')
week_15 <- games_2022 %>% filter(round.abbreviation == 'Rd 15')
week_16 <- games_2022 %>% filter(round.abbreviation == 'Rd 16')
week_17 <- games_2022 %>% filter(round.abbreviation == 'Rd 17')
week_18 <- games_2022 %>% filter(round.abbreviation == 'Rd 18')
week_19 <- games_2022 %>% filter(round.abbreviation == 'Rd 19')
week_20 <- games_2022 %>% filter(round.abbreviation == 'Rd 20')
week_21 <- games_2022 %>% filter(round.abbreviation == 'Rd 21')
week_22 <- games_2022 %>% filter(round.abbreviation == 'Rd 22')
week_23 <- games_2022 %>% filter(round.abbreviation == 'Rd 23')

#change week column names
colnames(week_23)[3] <- 'Date' 
week_23$Date <- as.Date(week_23$Date)


#create function to process week of games
run_week <- function(wk, model, pts_per, trials){
  final_frame <- data.frame()

  for(i in 1: nrow(wk)){
    home_team <- wk$home.team.name[i]
    away_team <- wk$away.team.name[i]
    
    stats_game <- model %>% filter(player_team == home_team | player_team == away_team)
    
    home_correction <- stats_game[1,8] * stats_game[2,9]
    away_correction <- stats_game[1,9] * stats_game[2,8]
    
    home_shots_mean <- stats_game[1,3] * home_correction
    away_shots_mean <- stats_game[2,3] * away_correction
    
    #run the numbers 100k times
    home_pts_per_shot <- rnorm(n = trials, mean = as.numeric(pts_per[2]), sd = as.numeric(pts_per[3]^(1/2)))
    away_pts_per_shot <- rnorm(n = trials, mean = as.numeric(pts_per[2], sd = pts_per[3]^(1/2)))
    home_total_shots <- rnorm(n = trials, mean = as.numeric(home_shots_mean), sd = as.numeric(stats_game[1,4]^(1/2)))
    away_total_shots <- rnorm(n = trials, mean = as.numeric(away_shots_mean), sd = as.numeric(stats_game[2,4]^(1/2)))
    home_score <- home_total_shots * home_pts_per_shot
    away_score <- away_total_shots * away_pts_per_shot
    totals <- home_score + away_score
    pt_diff <- home_score - away_score
    
    #create data frame to get totals, spread, and money line
    #mean_total <- mean(game_final_scores$totals)
    mean_total <- mean(totals)
    mean_side <- mean(pt_diff)
    totals_quantile <- quantile(totals, probs = c(0.45, 0.55), na.rm = TRUE)
    home_score_mean <- mean(home_score)
    away_score_mean <- mean(away_score)
    shots <- c(mean(home_total_shots), mean(away_total_shots))
    
    #add to data frame
    x <- cbind(home_team, away_team, totals_quantile, home_score_mean, away_score_mean, shots)
    final_frame <- data.frame(rbind(final_frame, x))
    
  }
  return(final_frame)
}

#create run for week of games
trial_run <- run_week(week_23, model_posterior, pts_per_shot_posterior, 100000)
trial_run$Date <- lookup(trial_run$home_team, week_23$home.team.name, week_23$Date)
trial_run$Date <- as.Date(trial_run$Date)
trial_run$season <- trial_run$Date
trial_run$season <- format(trial_run$Date, '%Y')

trial_run$home_team[trial_run$home_team == 'Gold Coast Suns'] <- 'Gold Coast'
trial_run$away_team[trial_run$away_team == 'Gold Coast Suns'] <- 'Gold Coast'
trial_run$home_team[trial_run$home_team == 'Sydney Swans'] <- 'Sydney'
trial_run$away_team[trial_run$away_team == 'Sydney Swans'] <- 'Sydney'
trial_run$home_team[trial_run$home_team == 'Brisbane Lions'] <- 'Brisbane'
trial_run$away_team[trial_run$away_team == 'Brisbane Lions'] <- 'Brisbane'
trial_run$home_team[trial_run$home_team == 'Geelong Cats'] <- 'Geelong'
trial_run$away_team[trial_run$away_team == 'Geelong Cats'] <- 'Geelong'
trial_run$home_team[trial_run$home_team == 'Adelaide Crows'] <- 'Adelaide'
trial_run$away_team[trial_run$away_team == 'Adelaide Crows'] <- 'Adelaide'
trial_run$home_team[trial_run$home_team == 'West Coast Eagles'] <- 'West Coast'
trial_run$away_team[trial_run$away_team == 'West Coast Eagles'] <- 'West Coast'



#join the afl_odds to the week of games to grade outcomes
week_plus_odds <- left_join(trial_run, afl_historical_odds_join, by = c('home_team', 'away_team'))
week_plus_odds <- week_plus_odds %>%
  mutate(totals = Home.Score + Away.Score)
colnames(week_plus_odds)[6] <- 'Date'
week_plus_odds <- week_plus_odds %>% select(-c(Date.y))

#season stats
#season_stats <- data.frame()
season_stats <- dplyr:: bind_rows(season_stats, week_plus_odds)

#save stats
setwd("/Users/ericp/OneDrive/Documents/GitHub/afl model")
write.csv(season_stats, 'season_stats.csv', row.names = FALSE)

#bayesian update

#download results from 2022 to start updating 
#actual_results <- get_fryzigg_stats(start = 2022, end = 2022)

fry_update <- actual_results %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_update$season <- format(fry_update$season, format = '%Y')
fry_update$match_round <- as.numeric(fry_update$match_round)
fry_update <- fry_update %>% filter(match_round < 23)
fry_update$opposition <- NA
fry_update <- fry_update %>%
  mutate(pts_per_shot = (6*goals + behinds) / shots_at_goal)

#update opposition column
for(i in 1: nrow(fry_update)) {
  if(fry_update$player_team[i] == fry_update$match_home_team[i]){
    fry_update$opposition[i] <- fry_update$match_away_team[i]
  } else {
    fry_update$opposition[i] <- fry_update$match_home_team[i]
  }
}

#update the mean shots by team and mean shots by opposition
max_round_update <- max(fry_update$match_round)
shots_update <- fry_update %>% dplyr :: group_by(season, player_team) %>%
  dplyr :: summarise(total_shots_by_team = sum(shots_at_goal)/max_round_update,
                     var_shots_by_team = var(shots_at_goal))

shots_update_opposition <- fry_update %>% dplyr :: group_by(season, opposition) %>%
  dplyr :: summarise(update_shots_by_opposition = sum(shots_at_goal)/max_round_update,
                     var_shots_update_opposition = var(shots_at_goal))
colnames(shots_update_opposition)[2] <- 'player_team'

#model update
model_update <- left_join(shots_update, shots_update_opposition, by = c('season', 'player_team'))
model_update <- model_update %>% 
  mutate(shots_differential = total_shots_by_team - update_shots_by_opposition,
         shots_over_average = total_shots_by_team / mean(total_shots_by_team),
         def_over_average = update_shots_by_opposition / mean(total_shots_by_team))

#make sds into variance to update
model_2022_update <- model_2022

#update column names on model_update
model_update[7,2] <- 'Geelong Cats'
model_update[9,2] <- 'GWS Giants'
model_update[1,2] <- 'Adelaide Crows'
model_update[17,2] <- 'West Coast Eagles'
model_update[16,2] <- 'Sydney Swans'
model_update[8,2] <- 'Gold Coast Suns'

#points per shot
pts_per_shot_update <- fry_update %>% dplyr :: group_by(season) %>%
  dplyr :: summarise(points_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal))

var_update <- fry_update %>% dplyr :: group_by(match_id) %>%
  dplyr :: mutate(pts_per_shot = ((sum(goals)*6) + sum(behinds)) / sum(shots_at_goal)) %>%
  dplyr :: select(match_id, pts_per_shot)
var_update <- unique(var_update)

pts_per_shot_update$var_pts <- var(var_update$pts_per_shot)

#update pts per shot
n0 <- 22 - max_round_update
n1 <- max_round_update
n_posterior <- n0 + n1
mu0 <- as.numeric(pts_per_shot_2022$points_per_shot)
x_bar <- as.numeric(pts_per_shot_update$points_per_shot)

#mu posterior
mu_posterior_shots <- (n0 * mu0 + (n1 * x_bar)) / n_posterior

#sigma posterior
sigma_data <- as.numeric(pts_per_shot_update$var_pts)
sigma_prior <- as.numeric(pts_per_shot_2022$var_pts)
df_start <- as.numeric(n0-1)
df_end <- as.numeric(n_posterior - 1)
sigma_posterior_shots <- (((n_posterior - 1) * sigma_data)  + (df_start * sigma_prior) + 
  ((x_bar - mu0)^2 * (n0*n1/n_posterior)))/(df_end)

#update sigmas and #mu
pts_per_shot_posterior <- data.frame(cbind(pts_per_shot_update$season, mu_posterior_shots, sigma_posterior_shots))
colnames(pts_per_shot_posterior) <- c('season', 'points_per_shot', 'var_pts')
pts_per_shot_posterior$pts_per_shot <- as.numeric(pts_per_shot_posterior$points_per_shot)
pts_per_shot_posterior$var_pts <- as.numeric(pts_per_shot_posterior$var_pts)
pts_per_shot_posterior$var_pts <- as.numeric(pts_per_shot_posterior$var_pts)

#update model stats
model_posterior <- data.frame(cbind(model_update$season, model_update$player_team))
colnames(model_posterior)[1:2] <- c('season', 'player_team')

#update mu / sigma model stats
for (i in 1:nrow(model_posterior)){
  model_posterior$total_shots_by_team[i] <- (n0 * model_2022$total_shots_by_team[i] + 
                                               (n1 * model_update$total_shots_by_team[i])) / n_posterior
  
  model_posterior$var_shots_by_team[i] <- (((n_posterior - 1) * model_update$var_shots_by_team[i]) + 
                                          (df_start * model_2022$var_shots_by_team[i]) +
                                          ((model_update$total_shots_by_team[i] - model_2022$total_shots_by_team[i])^2 *
                                           (n0*n1/n_posterior)))/df_end  
  
  model_posterior$update_shots_by_opposition[i] <- (n0 * model_2022$mean_shots_by_opposition[i] + 
                                               (n1 * model_update$update_shots_by_opposition[i])) / n_posterior
  
  model_posterior$var_shots_update_opposition[i] <- (((n_posterior - 1) * model_update$var_shots_update_opposition[i]) + 
                                            (df_start * model_2022$var_shots_by_opposition[i]) +
                                            ((model_update$update_shots_by_opposition[i] - model_2022$mean_shots_by_opposition[i])^2 *
                                               (n0*n1/n_posterior)))/df_end  
}


#create columns
model_posterior <- model_posterior %>% 
  mutate(shots_differential = total_shots_by_team - update_shots_by_opposition,
         shots_over_average = total_shots_by_team / mean(total_shots_by_team),
         def_over_average = update_shots_by_opposition / mean(update_shots_by_opposition))
colnames(model_posterior) <- colnames(model_2022)

#check win percentage

season_stats_check <- season_stats %>% select(c(home_team, away_team, totals_quantile, home_score_mean, away_score_mean,
                                          Home.Line.Open, Away.Line.Open, Total.Score.Open, totals))
season_stats_check$row_odd <- seq_len(nrow(season_stats_check)) %% 2 
season_stats_over <- season_stats_check %>% filter(row_odd == 1)
colnames(season_stats_over)[3] <- c('totals_quantile_low')
season_stats_under <- season_stats_check %>% filter(row_odd == 0)
colnames(season_stats_under)[3] <- c('totals_quantile_high')

season_stats_final <- left_join(season_stats_over, season_stats_under, by = c('home_team', 'away_team', 'Home.Line.Open', 'Total.Score.Open',
                                                                              'home_score_mean', 'away_score_mean', 'totals')) %>%
  select(c(home_team, away_team, totals_quantile_low, totals_quantile_high, home_score_mean, away_score_mean, Home.Line.Open, 
           Total.Score.Open, totals))


#check win % on totals
season_stats_final$over_under <- NA

for(i in 1:nrow(season_stats_final)){
  if(season_stats_final$Total.Score.Open[i] > season_stats_final$totals_quantile_high[i]) {
    season_stats_final$over_under[i] = 'under'
    } else if(season_stats_final$Total.Score.Open[i] < season_stats_final$totals_quantile_low[i]) {
          season_stats_final$over_under[i] = 'over'
    } else {
          season_stats_final$over_under[i] = 'no bet'
    }
  print(i)
}

season_stats_final$totals_result <- season_stats_final$totals - season_stats_final$Total.Score.Open
season_stats_final$totals_win_lose <- NA

for(j in 1:nrow(season_stats_final)){
  if(season_stats_final$totals_result[j] > 0 & season_stats_final$over_under[j] == 'over') {
    season_stats_final$totals_win_lose[j] = 'win'
  } else if (season_stats_final$totals_result[j] > 0 & season_stats_final$over_under[j] == 'under') {
      season_stats_final$totals_win_lose[j] = 'lose'
    } else if (season_stats_final$totals_result[j] < 0 & season_stats_final$over_under[j] == 'under'){
        season_stats_final$totals_win_lose[j] = 'win'
      } else if(season_stats_final$totals_result[j] > 0 & season_stats_final$over_under[j] == 'under') {
          season_stats_final$totals_win_lose = 'lose'
        } else {
          season_stats_final_totals_win_lose = 'push'
        }
}


#check win % on sides
season_stats_final <- season_stats_final %>%
  mutate(home_pt_diff = as.numeric(away_score_mean) - as.numeric(home_score_mean))

season_stats_final$sides_bet <- 

for(x in 1:nrow(season_stats_final)){
  if(season_stats_final$home_pt_diff< 0 & season_stats_final$Home.Line.Open[j] < season_stats_final$home_pt_diff){
    
  }
}
