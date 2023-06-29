#########################################################################################################
#install required libraries
#########################################################################################################
setwd("/Users/ericp/OneDrive/Documents/GitHub/afl-model")
library(fitzRoy)
library(dplyr)
library(lookup)
library(ggplot2)
library(stringr)
library(randomForest)
library(caret)
library(lookup)
library(bayestestR)
library(xgboost)
library(gbm)
library(lubridate)
library(jsonlite)
library(tidyr)

####################################################################################################
#ideas for 2024
####################################################################################################
#boost / weight stats by position
#player level data to adjust for injuries
#determine lambda 3 for bivariate model correlation
#weight recent performance over past?
#age curve for teams. 
#add variance to pts-per-shot by making that a RV.

#################################################################################################################
#Odds API
#################################################################################################################
theODDS_base <- 'https://api.the-odds-api.com/v4/sports/'
theODDS_sport  <- 'aussierules_afl/'
theODDS_key <- Sys.getenv('theODDS_key')
theODDS_region <- 'regions=au&'
#theODDS_markets <- '&markets=spreads,h2h,totals&oddsFormat=decimal&'
start_date <- as.Date('2023-03-10') 
theODDS_date <- paste0('date=',start_date,'T00:00:00Z')

#pull API data
odds_df <- data.frame()
y <- 1

while(start_date <= as.Date('2023-06-14')){
  theODDS_fullAPI <- paste0(theODDS_base, theODDS_sport, theODDS_key, theODDS_region, theODDS_date)
  
  pull_API <- fromJSON(theODDS_fullAPI)
  afl_odds <- pull_API$data
  odds_df <- rbind(odds_df, afl_odds)
  start_date <- start_date + 1
  theODDS_date <- paste0('date=',start_date,'T00:00:00Z')
  print(y)
  y <- y+1
}

odds_df$commence_time <- as.Date(odds_df$commence_time, format = '%Y-%m-%d')
odds_df <- odds_df %>% select(-c(1:3))
output_odds <- data.frame()

for(i in 1:nrow(odds_df)){
  teams <- odds_df[c(i:i),] %>% select(c(commence_time, home_team, away_team))
  first_level <- odds_df$bookmakers[[i]]
  
  over <- data.frame()
  under <- data.frame()
  hm_side <- data.frame()
  aw_side <- data.frame()
  
  for(j in 1:nrow(first_level)){
    bookies <- first_level$markets[[j]]
    bookies_spread <- bookies %>% filter(key == 'spreads')
    if(nrow(bookies_spread) == 0){next}
    bookies_spread_home <- bookies_spread$outcomes[[1]] %>% filter(name == teams$home_team)
    bookies_spread_away <- bookies_spread$outcomes[[1]] %>% filter(name == teams$away_team)
    bookies_total <- bookies %>% filter(key == 'totals')
    if(nrow(bookies_total) == 0){next}
    bookies_total_over <- bookies_total$outcomes[[1]] %>% filter(name == 'Over')
    bookies_total_under <- bookies_total$outcomes[[1]] %>% filter(name == 'Under')
    hm_side <- rbind(hm_side, bookies_spread_home)
    aw_side <- rbind(aw_side, bookies_spread_away)
    over <- rbind(over, bookies_total_over)
    under <- rbind(under, bookies_total_under)
  }
  if(is.infinite(teams$over|teams$under) == TRUE){next}
  teams$over <- min(over$point)
  teams$under <- max(under$point)
  teams$home_line <- ifelse(hm_side$point[1] < 0, min(hm_side$point),max(hm_side$point))
  teams$away_line <- ifelse(aw_side$point[1] < 0 , min(aw_side$point), max(aw_side$point))
  
  output_odds <- rbind(output_odds, teams)
}

colnames(odds_df)[1] <- 'date'
write.csv(output_odds, 'odds_df.csv', row.names = FALSE)
odds_df <- read.csv("~/GitHub/afl-model/odds_df.csv")

##########################################################################################################
#fetch historical odds to backtest
##########################################################################################################
afl_historical_odds <- read.csv("~/GitHub/afl-model/afl_historical_odds.csv")
afl_historical_odds$Date <- as.Date(dmy(afl_historical_odds$Date))
afl_historical_odds$season <- as.Date(afl_historical_odds$Date)
afl_historical_odds$season <- format(afl_historical_odds$season, format = '%Y')
afl_historical_odds_join <- afl_historical_odds %>% filter(season > 2021)
afl_historical_odds_join <- afl_historical_odds_join %>% select(-c(season))

#change Home.Team/Away.Team colnames to home_team/away_team
colnames(afl_historical_odds_join)[1] <- 'date'
colnames(afl_historical_odds_join)[3] <- 'home_team'
colnames(afl_historical_odds_join)[4] <- 'away_team'
colnames(afl_historical_odds)[3] <- 'home_team'
colnames(afl_historical_odds)[4] <- 'away_team'

#change team names where required
afl_historical_odds_join$home_team <- ifelse(afl_historical_odds_join$home_team == 'GWS Giants', 'Greater Western Sydney', afl_historical_odds_join$home_team)
afl_historical_odds_join$home_team <- ifelse(afl_historical_odds_join$home_team == 'Brisbane', 'Brisbane Lions', afl_historical_odds_join$home_team)
afl_historical_odds_join$away_team <- ifelse(afl_historical_odds_join$away_team == 'GWS Giants', 'Greater Western Sydney', afl_historical_odds_join$away_team)
afl_historical_odds_join$away_team <- ifelse(afl_historical_odds_join$away_team == 'Brisbane', 'Brisbane Lions', afl_historical_odds_join$away_team)

##########################################################################################################
#pull game statistics for five seasons
##########################################################################################################
season_pull <- function(x,y){
  df_season <- data.frame()
  if(!is.numeric(x)|!is.numeric(y)){return('x or y is not numeric.')}
  rng <- seq(x,y,1)
  for(i in 1:length(rng)){
    fry_stats <- fetch_player_stats_fryzigg(season = rng[i])
    df_season <- bind_rows(df_season, fry_stats)
    print(i)
  }
  df_season <- df_season %>% mutate(season = as.POSIXct(match_date, format = '%Y-%m-%d'))
  df_season$season <- format(df_season$season, format = '%Y')

  #create a column for who the opposition was in every game
  df_season$opposition <- NA
  #will allow me to see what teams give up lots of shots
  for(i in 1: nrow(df_season)) {
    if(df_season$player_team[i] == df_season$match_home_team[i]){
      df_season$opposition[i] <- df_season$match_away_team[i]
    } else {
      df_season$opposition[i] <- df_season$match_home_team[i]
    }
  }
  
  return(df_season)
}

#pull stats from 2016-2020 to build initial model
fry_stats <- season_pull(2016,2020)

########################################################################################################
#create group stats
########################################################################################################
#create function to group offensive stats
stats_create <- function(df) {
  df2 <- df %>% group_by(match_id, player_team) %>%
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
              game_cont_possessions = sum(contested_possessions)
          
            )
  imp <- colnames(df2)[colSums(is.na(df2)) > 0]
  
  #impute NANs into mean of data
  mean_imp <- lapply(df2[,imp], mean, na.rm = TRUE)
  df2[,imp][is.na(df2[,imp])] <- as.integer(mean_imp)
  
  #feature engineering
  df2$pct_mks_in_50 <- df2$game_marks_50 / df2$game_marks
  df2$pct_cont_marks <- df2$game_cont_marks / df2$game_marks
  df2$ins_times_50s <- df2$game_ins * df2$game_marks_50
  df2$rebound_ins_comb <- df2$game_rebounds * df2$game_ins
  
  
  return(df2)
}

#create stats_grouped for 2016-2020  
stats_grouped <- stats_create(fry_stats)

#check for NANs
which(is.na(stats_grouped))

#########################################################################################################
#check histograms of variables to see if any are skewed
#########################################################################################################
p_shots <- ggplot(data = stats_grouped, aes(x = game_shots)) + geom_histogram(binwidth = 5)
p_ins <- ggplot(data = stats_grouped, aes(x = game_ins)) + geom_histogram(binwidth = 5)
p_kicks <- ggplot(data = stats_grouped, aes(x = game_kicks)) + geom_histogram(binwidth = 5)
p_marks <- ggplot(data = stats_grouped, aes(x = game_marks)) + geom_histogram(binwidth = 5)

p_shots
p_ins
p_kicks
p_marks

#########################################################################################################
#build random forest for game shots
#test and train data
#########################################################################################################
rf_data <- stats_grouped %>% ungroup() %>%
  select(-c(1,2))
rf_data$random <- runif(n = nrow(rf_data))
rf_train <- rf_data %>% filter(random <= 0.8)
rf_test <- rf_data %>% filter(random > 0.8)
rf_train <- rf_train %>% select(-c(random))
rf_test <- rf_test %>% select(-c(random))

#create rf
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(50)
tunegrid <- expand.grid(.mtry=c(13))
metric <- 'RMSE'
rf_gridsearch <- train(game_shots~., data = rf_train, method = "rf", metric = metric, tuneGrid = tunegrid, 
                       trControl = control)

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 13.

#predict values
y_rf <- predict(object = rf_gridsearch, newdata = rf_test[,-1])

#check RMSE
root_error_rf <- sqrt((sum(y_rf-rf_test$game_shots)^2)/length(y_rf))
cat('RMSE on testing data: ', round(root_error_rf, 4), ' shots',  sep='')

###########################################################################################################
#predict shots gbm
###########################################################################################################
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10
                           )

gbmGrid <-  expand.grid(interaction.depth = c(1), 
                        n.trees = (10)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm_model <- train(game_shots ~ ., data = rf_train, 
                 method = 'gbm', 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = 'RMSE')

gbm_model$bestTune
#> gbm_model$bestTune
#n.trees interaction.depth shrinkage n.minobsinnode
#1     500                 1       0.1             20

#predict shots per game
gbm_test <- rf_test[,-1]
gbm_preds <- predict(gbm_model, newdata = gbm_test)

#check RMSE
root_error_gbm <- sqrt((sum(gbm_preds-rf_test$game_shots)^2)/length(gbm_preds))
cat('RMSE on testing data: ', round(root_error_gbm, 4), ' shots',  sep='')

#########################################################################################################
#predict shots xgboost
#########################################################################################################
set.seed(50)
# Customzing the tuning grid
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

# training a XGboost Regression tree model while tuning parameters
xboost_model <- train(game_shots~., data = rf_train, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)

#summarize results
print(xboost_model)

#make preds xgboost
xboost_test <- rf_test[,-1]
pred_xboost <- predict(xboost_model, xboost_test)

#check RMSE
root_error_xboost <- sqrt((sum(pred_xboost-rf_test$game_shots)^2)/length(pred_xboost))
cat('RMSE on testing data: ', round(root_error_xboost, 4), ' shots',  sep='')

##########################################################################################################
#ensemble model
##########################################################################################################
train_preds_rf <- data.frame(predict(rf_gridsearch, newdata = rf_train[,-1]))
colnames(train_preds_rf) <- 'preds_rf'
train_preds_gbm <- data.frame(predict(gbm_model, newdata = rf_train[,-1]))
colnames(train_preds_gbm) <- 'preds_gbm'
train_preds_xboost <- data.frame(predict(xboost_model, newdata = rf_train[,-1]))
colnames(train_preds_xboost) <- 'preds_xboost'

#lm ensemble
ensemble_df <- bind_cols(rf_train[,1], train_preds_rf, train_preds_gbm, train_preds_xboost)
ensemble_model <- lm(game_shots~., data = ensemble_df)

#ensemble preds
ensemble_test <- bind_cols(y_rf, gbm_preds, pred_xboost)
colnames(ensemble_test) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
ensemble_preds <- predict(ensemble_model, newdata = ensemble_test)

#check RMSE
root_error_ensemble <- sqrt((sum(ensemble_preds-rf_test$game_shots)^2)/length(ensemble_preds))
cat('RMSE on testing data: ', round(root_error_ensemble, 4), ' shots',  sep='')

########################################################################################################
#ensemble performed well#
########################################################################################################

########################################################################################################
#Models to isolate travel, defensive and home field#
########################################################################################################
library(rvest)
library(DAAG)
library(geosphere)
library(tibble)
aus_cities <- data.frame(aulatlong)
aus_cities <- rownames_to_column(aus_cities)
colnames(aus_cities) <- c('City','Lat','Long')
aus_cities$City <- ifelse(aus_cities$City == 'Alice','Alice Springs', aus_cities$City)
url <- 'https://en.wikipedia.org/wiki/Australian_Football_League'
html <- read_html(url)


#change cities to states to match home ground and home stats dfs. 
aus_cities$City <- ifelse(aus_cities$City == 'Adelaide', 'South Australia', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Alice Springs'| aus_cities$City =='Darwin', 'Northern Territories', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Sydney', 'New South Wales', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Melbourne', 'Victoria', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Perth', 'Western Australia', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Brisbane', 'Queensland', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Hobart', 'Tasmania', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Canberra', 'Australian Capital Territory', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Broome', 'Western Australia', aus_cities$City)
aus_cities$City <- ifelse(aus_cities$City == 'Cairns', 'Queensland', aus_cities$City)


#distance function
#distm is the distance function (lat1,lon1,lat2,lon2)

home_ground <- html %>% 
  html_element("table.wikitable.sortable") %>% 
  html_table()
home_ground <- home_ground[c(-1,-20),]
home_ground <- home_ground %>% select(-c(2,3,6:12))
colnames(home_ground) <- c('team', 'city', 'home_ground')


###################################################################################################################
#HFA adjustments
###################################################################################################################
home_stats <- fry_stats %>%
  group_by(match_id, player_team) %>%
  summarise(total_shots = sum(shots_at_goal))
home_stats$season <- lookup(home_stats$match_id, fry_stats$match_id, fry_stats$season)
home_stats$home_team <- lookup(home_stats$match_id, fry_stats$match_id, fry_stats$match_home_team)
home_stats$away_team <- lookup(home_stats$match_id, fry_stats$match_id, fry_stats$match_away_team)
home_stats$venue <- lookup(home_stats$match_id, fry_stats$match_id, fry_stats$venue_name)
home_stats$home_away <- ifelse (home_stats$player_team == home_stats$home_team, 'home', 'away')
home_stats$home_city <- lookup(home_stats$home_team, home_ground$team, home_ground$city)
home_stats$away_city <- lookup(home_stats$away_team, home_ground$team, home_ground$city)
home_stats$away_ground <-lookup(home_stats$away_team, home_ground$team, home_ground$home_ground)

#get rid of ^ in NSW column
home_stats$home_city <- ifelse(home_stats$home_city == 'New South Wales^', 'New South Wales', home_stats$home_city)
home_stats$away_city <- ifelse(home_stats$away_city == 'New South Wales^', 'New South Wales', home_stats$away_city)

#calc travel distance for away teams

home_stats$home_lat <- lookup(home_stats$home_city,aus_cities$City, aus_cities$Lat)
home_stats$home_lon <- lookup(home_stats$home_city,aus_cities$City, aus_cities$Long)
home_stats$away_lat <- lookup(home_stats$away_city,aus_cities$City, aus_cities$Lat)
home_stats$away_lon <- lookup(home_stats$away_city,aus_cities$City, aus_cities$Long)

#away travel
home_stats$travel <- NA
for(i in 1:nrow(home_stats)){
  home_stats$travel[i] <- distm(c(home_stats$home_lat[i],home_stats$home_lon[i]),c(home_stats$away_lat[i], home_stats$away_lon[i]),
        fun = distHaversine)/1000
  }

home_stats$travel <- ifelse(home_stats$home_away == 'home',0, home_stats$travel)

#basic stats
shots_home_away <- home_stats %>% group_by(home_away) %>% summarise(mean_shots = mean(total_shots))
#home team takes approx 1.9 more shots
travel_shots <- home_stats %>% group_by(home_away,travel) %>% summarise(mean_shots = mean(total_shots))
#home team shot diff increases as travel increases
shots_home_away_year <- home_stats %>% group_by(home_away, season) %>% summarise(mean_shots = mean(total_shots))
#more to look at. doesn't look like a trend there.
shots_venue <- home_stats %>% group_by(home_away, venue) %>% summarise(mean_shots = mean(total_shots))
#does look like some venues create greater discrepancies. Also, some venues probably only had very few games there
#so making some crazy looking stats.

#########################################################################################################
#home / away splits 
#########################################################################################################
HFA <- home_stats %>% group_by(home_away, venue) %>% 
  summarise(mean_shots = mean(total_shots),
            total_games = n()) %>%
  filter(total_games > 9)
HFA <- HFA %>% mutate(overall_mean = 0.5 * (sum(fry_stats$shots_at_goal)/n_distinct(fry_stats$match_id)),
                      pct_of_mean = mean_shots / overall_mean,
                      shot_diff = mean_shots - overall_mean)

#########################################################################################################
#effect of travel
#########################################################################################################
travel_effect <- home_stats %>% filter(home_away == 'away') %>%
  group_by(travel) %>%
  summarise(mean_shots = mean(total_shots),
            total_games = n())

lm_travel <- lm(mean_shots~travel, data = travel_effect)
summary(lm_travel)
travel_int <- lm_travel$coefficients[1]
travel_coeff <- lm_travel$coefficients[2]
#get home mean shots for HFA calc
home_base <- home_stats %>% filter(home_away == 'home') %>% 
  group_by(home_away) %>% 
  summarise(mean_shots = mean(total_shots))

##########################################################################################################
#defensive adjustment
##########################################################################################################
fry_stats_2021 <- fetch_player_stats_fryzigg(season = 2021)
fry_stats_2022 <- fetch_player_stats_fryzigg(season = 2022)
fry_stats_2021 <- fry_stats_2021 %>% filter(!(match_round %in% c('Semi Finals', 'Preliminary Finals', 'Grand Final', 'Finals Week 1')))
fry_stats_2022 <- fry_stats_2022 %>% filter(!(match_round %in% c('Semi Finals', 'Preliminary Finals', 'Grand Final', 'Finals Week 1')))
fry_stats_2021$match_round <- is.numeric(fry_stats_2021$match_round)
fry_stats_2022$match_round <- is.numeric(fry_stats_2022$match_round)
fry_stats_2021$opposition <- fry_stats_2022$opposition <- NA

#create a column for who the opposition was in every game
#will allow me to see what teams give up lots of shots
for(i in 1: nrow(fry_stats_2021)) {
  if(fry_stats_2021$player_team[i] == fry_stats_2021$match_home_team[i]){
    fry_stats_2021$opposition[i] <- fry_stats_2021$match_away_team[i]
  } else {
    fry_stats_2021$opposition[i] <- fry_stats_2021$match_home_team[i]
  }
}

#create a column for who the opposition was in every game for 2022
#will allow me to see what teams give up lots of shots in 2022
for(i in 1: nrow(fry_stats_2022)) {
  if(fry_stats_2022$player_team[i] == fry_stats_2022$match_home_team[i]){
    fry_stats_2022$opposition[i] <- fry_stats_2022$match_away_team[i]
  } else {
    fry_stats_2022$opposition[i] <- fry_stats_2022$match_home_team[i]
  }
}

#function to create defensive stats
defense_func <- function(df){
  df2 <- df %>% group_by(match_id, opposition) %>%
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
  return(df2)
}

#2021 defensive stats
defense_grouped_2021 <- defense_func(fry_stats_2021)
defense_grouped_2022 <- defense_func(fry_stats_2022)
#check for NANs
which(is.na(defense_grouped_2021))
which(is.na(defense_grouped_2022))
#none

##########################################################################################################
#ensemble defense correction
##########################################################################################################
defensive_correction <- function(df, df2){
  df3 <- df %>% group_by(opposition) %>%
    mutate(mean_game_shots = mean(game_shots)) %>%
    select(c(opposition, mean_game_shots)) %>% unique()
  df3$league_mean <- sum(df2$shots_at_goal/n_distinct(df2$match_id))/2
  df3 <- df3 %>%
    mutate(def_diff = mean_game_shots - league_mean,
           def_pct = mean_game_shots / league_mean)
  colnames(df3)[1] <- 'player_team'
  return(df3)
}

def_stats_2021 <- defensive_correction(defense_grouped_2021, fry_stats_2021)
def_stats_2022 <- defensive_correction(defense_grouped_2022, fry_stats_2022)

###########################################################################################################
#initial offensive stats from ensemble model
###########################################################################################################
########################################################################################################
#create group stats
########################################################################################################
stats_create_2021 <- function(df){
  df2 <- df %>% group_by(match_id, player_team) %>%
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

  #feature engineering
  df2$pct_mks_in_50 <- df2$game_marks_50 / df2$game_marks
  df2$pct_cont_marks <- df2$game_cont_marks / df2$game_marks
  df2$ins_times_50s <- df2$game_ins * df2$game_marks_50
  df2$rebound_ins_comb <- df2$game_rebounds * df2$game_ins
  
  return(df2)
}

stats_grouped_2021 <- stats_create_2021(fry_stats_2021)
stats_grouped_2022 <- stats_create_2021(fry_stats_2022)
#check for NANs
which(is.na(stats_grouped_2021))
colnames(stats_grouped_2021)[colSums(is.na(stats_grouped_2021)) > 0]
which(is.na(stats_grouped_2022))
colnames(stats_grouped_2022)[colSums(is.na(stats_grouped_2021)) > 0]

#########################################################################################################
#run models on 2021 data
#########################################################################################################
train_preds_rf_2021 <- data.frame(predict(rf_gridsearch, newdata = stats_grouped_2021[,-c(1:3)]))
train_preds_rf_2022 <- data.frame(predict(rf_gridsearch, newdata = stats_grouped_2022[,-c(1:3)]))
colnames(train_preds_rf_2021) <- 'preds_rf'
colnames(train_preds_rf_2022) <- 'preds_rf'
train_preds_gbm_2021 <- data.frame(predict(gbm_model, newdata = stats_grouped_2021[,-c(1:3)]))
train_preds_gbm_2022 <- data.frame(predict(gbm_model, newdata = stats_grouped_2022[,-c(1:3)]))
colnames(train_preds_gbm_2021) <- 'preds_gbm'
colnames(train_preds_gbm_2022) <- 'preds_gbm'
train_preds_xboost_2021 <- data.frame(predict(xboost_model, newdata = stats_grouped_2021[,-c(1:3)]))
train_preds_xboost_2022 <- data.frame(predict(xboost_model, newdata = stats_grouped_2022[,-c(1:3)]))
colnames(train_preds_xboost_2021) <- 'preds_xboost'
colnames(train_preds_xboost_2022) <- 'preds_xboost'

#lm ensemble
ensemble_df_2021 <- bind_cols(stats_grouped_2021[c(1:3)],train_preds_rf_2021, train_preds_gbm_2021, train_preds_xboost_2021)
ensemble_df_2021$ensemble_preds <- predict(ensemble_model, newdata = ensemble_df_2021[,-c(1:3)])
ensemble_df_2022 <- bind_cols(stats_grouped_2022[c(1:3)],train_preds_rf_2022, train_preds_gbm_2022, train_preds_xboost_2022)
ensemble_df_2022$ensemble_preds <- predict(ensemble_model, newdata = ensemble_df_2022[,-c(1:3)])

############################################################################################################
#initial lambda per team 2022
############################################################################################################
lambda_initial_2022 <- ensemble_df_2021 %>% 
  dplyr :: group_by(player_team) %>%
  dplyr :: summarise(mean_shots = mean(ensemble_preds))
lambda_initial_2023 <- ensemble_df_2022 %>%
  dplyr :: group_by(player_team) %>%
  dplyr :: summarise(mean_shots = mean(ensemble_preds))

############################################################################################################
#combine above work to get complete view of team shots / HFA / defense
#DFs needed <- HFA, HFA_home, HFA_away, def_stats_2021, travel_coeff
############################################################################################################

############################################################################################################
#pts per shot by venue and weather
############################################################################################################
pts_per_shot <- fry_stats %>% group_by(match_id) %>%
  summarize(total_shots = sum(shots_at_goal),
            total_points = sum(goals * 6 + behinds),
            points_per_kick = total_points / total_shots)

#assign lat lon for each game
library(tidyr)
pts_per_shot$home_team <- lookup(pts_per_shot$match_id, fry_stats$match_id, fry_stats$match_home_team)
pts_per_shot$city <- lookup(pts_per_shot$home_team, home_ground$team, home_ground$city)
pts_per_shot$city <- ifelse(pts_per_shot$city == 'New South Wales^', 'New South Wales', pts_per_shot$city)
pts_per_shot$lat <- lookup(pts_per_shot$city, aus_cities$City, aus_cities$Lat)
pts_per_shot$long <- lookup(pts_per_shot$city, aus_cities$City, aus_cities$Long)
pts_per_shot$date <- lookup(pts_per_shot$match_id, fry_stats$match_id, fry_stats$match_date)
pts_per_shot$start_time <- lookup(pts_per_shot$match_id, fry_stats$match_id, fry_stats$match_local_time)
pts_per_shot$home_ground <- lookup(pts_per_shot$home_team, home_ground$team, home_ground$home_ground)
time_separated <- pts_per_shot %>% 
  separate(start_time,c('hour', 'minute', 'second'), sep = ':') %>% 
  select(-c(minute, second))
pts_per_shot$hour <- lookup(pts_per_shot$match_id, time_separated$match_id, time_separated$hour)

#############################################################################################################
#create function to pull weather data
#############################################################################################################
library(jsonlite)
weather_api_base <- 'https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/'
API_KEY <- Sys.getenv('API_KEY')

weather_function <- function(df, base, key){
  out <- data.frame()
  df2 <- df %>% select(c(city, lat, long, date, hour))
  df2 <- df2 %>% distinct()
  for(i in 1:nrow(df2)){
    #create city to locate using lat/lon
    city <- paste0(df2[i,3],',',df2[i,2],'/')
    #create date to pull from df
    api_date <- df2[i,4]
    #create time from df
    api_time <- paste0('T',df2[i,5],':00:00')
    #make full api link
    full_api <- paste0(base,city,api_date,api_time,key)
    #turn JSON into list
    weather_data <- fromJSON(full_api)
    #turn list into dataframe
    x <- data.frame(weather_data$days)
    #pull hour by hour by hour data out of last column (last column is in itself a list)
    x_hour <- x$hours[[1]]
    #pull out hours
    x_hour <- x_hour %>% separate(datetime,c('hour', 'minute', 'second'), sep = ':') %>% select(-c(minute, second))
    
    final_weather <- x_hour %>% select(c(hour, temp, humidity, dew, precip, precipprob, windspeed, winddir, conditions))
    final_weather <- cbind(df2$date[i], df2$lat[i], df2$long[i], final_weather)
    out <- rbind(out,final_weather)
    print(i)
  }
  
  return(out)
}

##############################################################################################################
#create weather df
##############################################################################################################
weather_output <- weather_function(pts_per_shot, weather_api_base, API_KEY)
colnames(weather_output)[1:4] <- c('date','lat', 'long','hour')
pts_per_shot <- left_join(pts_per_shot, weather_output, by = c('date', 'hour', 'lat', 'long'), relationship = 'many-to-many') %>%
  select(-c(total_shots, total_points, home_team, city, lat, long, date, start_time, hour)) %>% distinct()

##############################################################################################################
#attach weather to points per kick df
##############################################################################################################
docklands <- function(df){
  df$winddir <- ifelse(df$home_ground == 'Docklands Stadium' & df$precip > 0,0,df$winddir)
  df$windspeed <- ifelse(df$home_ground == 'Docklands Stadium' & df$precip > 0,0,df$windspeed)
  df$precip <- ifelse(df$home_ground == 'Docklands Stadium' & df$precip > 0,0,df$precip)
  df$precipprob <- ifelse(df$home_ground == 'Docklands Stadium' & df$precipprob > 0,0,df$precipprob)
  df$temp <- ifelse(df$home_ground == 'Docklands Stadium' & df$temp < 70, 70, df$temp)
  return(df)
}
pts_per_shot <- docklands(pts_per_shot)
write.csv(pts_per_shot, 'pts_per_shot.csv', row.names = FALSE)
pts_per_shot <- read.csv("~/GitHub/afl-model/pts_per_shot.csv")
############################################################################################################
#build ensemble models to predict points per kick
#start with random forest
############################################################################################################
rf_pts_per_shot <- pts_per_shot %>% select(-match_id)
rf_pts_per_shot$random <- runif(n = nrow(rf_pts_per_shot))
rf_train_pts <- rf_pts_per_shot %>% filter(random <= 0.8)
rf_test_pts <- rf_pts_per_shot %>% filter(random > 0.8)
rf_train_pts <- rf_train_pts %>% select(-c(random))
rf_test_pts <- rf_test_pts %>% select(-c(random))

#create RF
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(50)
tunegrid <- expand.grid(.mtry=c(13))
metric <- 'RMSE'
#check for NANs
which(is.na(rf_train_pts))
colnames(rf_train_pts)[colSums(is.na(rf_train_pts)) > 0]
#remove 3 rows of NAs
rf_train_pts <- rf_train_pts %>% na.omit()
rf_test_pts <- rf_test_pts %>% na.omit()
#run RF model
rf_gridsearch_pts <- train(points_per_kick~., data = rf_train_pts, method = "rf", metric = metric, tuneGrid = tunegrid, 
                       trControl = control)

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 13.

#predict values
y_rf_pts <- predict(object = rf_gridsearch_pts, newdata = rf_test_pts[,-1])

#check RMSE
root_error_pts <- sqrt((sum(y_rf_pts-rf_test_pts$points_per_kick)^2)/length(y_rf_pts))
cat('RMSE on testing data: ', round(root_error_pts, 4), ' pts_per_shot',  sep='')

###########################################################################################################
#predict pts_per_shot gbm
###########################################################################################################
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10
)

gbmGrid <-  expand.grid(interaction.depth = c(1), 
                        n.trees = (10)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm_model_pts <- train(points_per_kick~ ., data = rf_train_pts, 
                   method = 'gbm', 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = gbmGrid,
                   ## Specify which metric to optimize
                   metric = 'RMSE')

gbm_model_pts$bestTune
#> gbm_model$bestTune
#n.trees interaction.depth shrinkage n.minobsinnode
#1     500                 1       0.1             20

#predict shots per game
gbm_test_pts <- rf_test_pts[,-1]
#remove NAs
gbm_test_pts <- gbm_test_pts %>% na.omit()
#make preds
gbm_preds_pts <- predict(gbm_model_pts, newdata = gbm_test_pts)

#check RMSE
root_error_gbm_pts <- sqrt((sum(gbm_preds_pts-rf_test_pts$points_per_kick)^2)/length(gbm_preds_pts))
cat('RMSE on testing data: ', round(root_error_gbm_pts, 4), 'pts_per_shot',  sep='')

#########################################################################################################
#predict pts_per_shot xgboost
#########################################################################################################
set.seed(50)
# Customzing the tuning grid
gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

# training a XGboost Regression tree model while tuning parameters
xboost_model_pts <- train(points_per_kick~., data = rf_train_pts, method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)

#summarize results
print(xboost_model)

#make preds xgboost
xboost_test_pts <- rf_test_pts[,-1]
pred_xboost_pts <- predict(xboost_model_pts, xboost_test_pts)

#check RMSE
root_error_xboost_pts <- sqrt((sum(pred_xboost_pts-rf_test_pts$points_per_kick)^2)/length(pred_xboost_pts))
cat('RMSE on testing data: ', round(root_error_xboost_pts, 4), ' shots',  sep='')

##########################################################################################################
#ensemble model points_per_shot
##########################################################################################################
train_preds_rf <- data.frame(predict(rf_gridsearch_pts, newdata = rf_train_pts[,-1]))
colnames(train_preds_rf) <- 'preds_rf'
train_preds_gbm <- data.frame(predict(gbm_model_pts, newdata = rf_train_pts[,-1]))
colnames(train_preds_gbm) <- 'preds_gbm'
train_preds_xboost <- data.frame(predict(xboost_model_pts, newdata = rf_train_pts[,-1]))
colnames(train_preds_xboost) <- 'preds_xboost'

#lm ensemble
ensemble_df <- bind_cols(rf_train_pts[,1], train_preds_rf, train_preds_gbm, train_preds_xboost)
colnames(ensemble_df)[1] <- 'points_per_kick'
ensemble_model <- lm(points_per_kick~., data = ensemble_df)

#ensemble preds
ensemble_test <- bind_cols(y_rf_pts, gbm_preds_pts, pred_xboost_pts)
colnames(ensemble_test) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
ensemble_preds <- predict(ensemble_model, newdata = ensemble_test)

#check RMSE
root_error_ensemble <- sqrt((sum(ensemble_preds-rf_test_pts$points_per_kick)^2)/length(ensemble_preds))
cat('RMSE on testing data: ', round(root_error_ensemble, 4), ' shots',  sep='')

###############################################################################################################
#ensemble model worked very well. Off by ~0.021 pts per kick
###############################################################################################################

###############################################################################################################
#build out back test here
###############################################################################################################
#needed data
lambda_initial_2022
def_stats_2021

#pull schedule for 2022
sched_2022 <- fetch_fixture(season = 2022) %>% 
  select(c(compSeason.year, utcStartTime, round.roundNumber, home.team.name, home.team.abbreviation, home.team.nickname, away.team.name, away.team.abbreviation, away.team.nickname)) %>%
  separate(utcStartTime, c('Date', 'Time'), 'T') %>%
  separate(Time, c('Hour', 'Minute', 'Second'), ':') %>%
  select(-c('Second')) %>% filter(round.roundNumber < 24)
sched_2022$Hour <- as.integer(sched_2022$Hour)

#align home team names
sched_team <- function(df){
  df$home.team.name <- ifelse(df$home.team.name == 'Geelong Cats', 'Geelong', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'GWS Giants', 'Greater Western Sydney', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'Adelaide Crows', 'Adelaide', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'GWS Giants', 'Greater Western Sydney', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'West Coast Eagles', 'West Coast', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'Sydney Swans', 'Sydney', df$home.team.name)
  df$home.team.name <- ifelse(df$home.team.name == 'Gold Coast Suns', 'Gold Coast', df$home.team.name)
  #align away team names 
  df$away.team.name <- ifelse(df$away.team.name == 'Geelong Cats', 'Geelong', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'GWS Giants', 'Greater Western Sydney', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'Adelaide Crows', 'Adelaide', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'GWS Giants', 'Greater Western Sydney', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'West Coast Eagles', 'West Coast', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'Sydney Swans', 'Sydney', df$away.team.name)
  df$away.team.name <- ifelse(df$away.team.name == 'Gold Coast Suns', 'Gold Coast', df$away.team.name)
  
  return(df)  
}

sched_2022 <- sched_team(sched_2022)

#add match_id
match_id_2022 <- fetch_player_stats_fryzigg(season = 2022)
colnames(match_id_2022)[3:5] <- c('home.team.name', 'away.team.name', 'Date')
match_id_2022 <- match_id_2022 %>% select(match_id, home.team.name, away.team.name, Date) %>% unique()
#attach match_id to sched_2022
sched_2022 <- left_join(sched_2022, match_id_2022, by = c('Date', 'home.team.name', 'away.team.name'))

#pull home ground data
sched_2022$home_ground <- lookup(sched_2022$home.team.name, home_ground$team, home_ground$home_ground)
#pull away ground data to add travel
sched_2022$away_ground <- lookup(sched_2022$away.team.name, home_ground$team, home_ground$home_ground)
#pull home city and lat / long
sched_2022$home_city <- lookup(sched_2022$home.team.name, home_ground$team, home_ground$city)
sched_2022$home_city <- ifelse(sched_2022$home_city == 'New South Wales^', 'New South Wales', sched_2022$home_city)
sched_2022$home_lat <- lookup(sched_2022$home_city, aus_cities$City, aus_cities$Lat)
sched_2022$home_lon <- lookup(sched_2022$home_city, aus_cities$City, aus_cities$Long)
#pull away city and lat / long
sched_2022$away_city <- lookup(sched_2022$away.team.name, home_ground$team, home_ground$city)
sched_2022$away_city <- ifelse(sched_2022$away_city == 'New South Wales^', 'New South Wales', sched_2022$away_city)
sched_2022$away_lat <- lookup(sched_2022$away_city, aus_cities$City, aus_cities$Lat)
sched_2022$away_lon <- lookup(sched_2022$away_city, aus_cities$City, aus_cities$Long)
pts_per_shot$hour <- lookup(pts_per_shot$match_id, time_separated$match_id, time_separated$hour)

#pull weather data
##############################################################################################################
#create weather df
##############################################################################################################
colnames(sched_2022)[c(2:3,15:17)] <- c('date', 'hour', 'city', 'lat', 'long')
weather_output_2022 <- weather_function(sched_2022, weather_api_base, API_KEY)
colnames(weather_output_2022)[1:4] <- c('date', 'lat', 'long', 'hour')
weather_output_2022$hour <- as.integer(weather_output_2022$hour)

write.csv(weather_output_2022, 'weather_output_2022.csv', row.names = FALSE)
weather_output_2022 <- read.csv("~/GitHub/afl-model/weather_output_2022.csv")

##############################################################################################################
#attach weather to points per kick df
##############################################################################################################
sched_2022_weather <- left_join(sched_2022, weather_output_2022, by = c('date', 'hour', 'lat', 'long'), relationship = 'many-to-many') %>%
  distinct()
weather_output_2022 <- docklands(sched_2022_weather)

##############################################################################################################
#add travel column and add distance traveled by away team
##############################################################################################################
#calc travel distance for awa#away travel
sched_2022_weather$travel <- NA
for(i in 1:nrow(sched_2022_weather)){
  sched_2022_weather$travel[i] <- distm(c(sched_2022_weather$lat[i],sched_2022_weather$long[i]),c(sched_2022_weather$away_lat[i], sched_2022_weather$away_lon[i]),
                                fun = distHaversine)/1000
  print(i)
}

###############################################################################################################
#loop through the schedule
###############################################################################################################
library(extraDistr)
update_df_final <- data.frame(sched_2022) %>% select(home.team.name) %>% unique()
colnames(update_df_final) <- 'player_team'
lambda_loop <- lambda_initial_2022
lambda_loop <- lambda_loop %>% 
  mutate(alpha = mean_shots * 6,
         beta = 6)
def_corr_update <- def_stats_2021
updated_stats <- season_pull(2022,2022)
output_df <- data.frame()
defense_df <- data.frame()
combined_defense <- def_stats_2021
colnames(combined_defense) <- c('player_team', 'mean_game_shots_0', 'league_mean_0', 'def_diff_0', 'def_pct_0')

#set initial n
n = 1

while(n < 23){
  final_df <- data.frame()
  #filter to just current week
  new_week <- sched_2022_weather %>% filter(round.roundNumber == n)
  
  for(k in 1:nrow(new_week)){
    #lookup mean shots for home and away team
    home_lambda <- lookup(new_week$home.team.name[k], lambda_loop$player_team, lambda_loop$mean_shots)
    away_lambda <- lookup(new_week$away.team.name[k], lambda_loop$player_team, lambda_loop$mean_shots)
    #lookup adjustment for defense
    home_def <- lookup(new_week$home.team.name[k], def_corr_update$player_team, def_corr_update$def_pct)
    away_def <- lookup(new_week$away.team.name[k], def_corr_update$player_team, def_corr_update$def_pct)
    #travel_adj 
    travel_adj <- travel_coeff * sched_2022_weather$travel[k]
    #HFA
    hfa_adj <- home_base[2] - travel_int  - (sched_2022_weather$travel[k] * travel_coeff)
    #adjusted lambdas
    home_adj <- (home_lambda * away_def)
    away_adj <- (away_lambda * home_def) - hfa_adj
    #pts_per_kick
    df_pts <- new_week %>% .[k:k,] %>% select(c(home_ground, temp, humidity, dew, precip, precipprob, windspeed, winddir, conditions))
    pts_rf <- predict(rf_gridsearch_pts, newdata = df_pts)
    pts_xboost <- predict(gbm_model_pts, newdata = df_pts)
    pts_gbm <- predict(xboost_model_pts, newdata = df_pts)
    lm_df <- data.frame(pts_rf, pts_xboost, pts_gbm)
    colnames(lm_df) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
    pts_per_kick <- predict(ensemble_model, lm_df)
    
    #simulate 100,000 games with home_adj and away_adj as lambdas
    biv_pois <- as.data.frame(rbvpois(100000, home_adj, away_adj[1,1],0) * pts_per_kick)
    colnames(biv_pois) <- c('home', 'away')
    home_mean <- mean(biv_pois$home)
    away_mean <- mean(biv_pois$away)
    total <- biv_pois$home + biv_pois$away
    total_quant <- c(mean(total), quantile(total, probs = c(0.30,0.70)))
    output <- c(new_week$date[k], new_week$home.team.name[k], new_week$away.team.name[k], round(home_mean,2), round(away_mean,2), round(home_mean - away_mean,2), round(total_quant,2))
    final_df <- rbind(final_df, output)
    colnames(final_df) <- c('date', 'home_team', 'away_team', 'home_mean_score', 'away_mean_score', 'side', 'total', 'total_low_quantile', 'total_high_quantile')
    
    }
  
  #attach betting odds to final_df
  final_df$date <- as.Date(final_df$date)
  final_df <- left_join(final_df, afl_historical_odds_join, by = (c('date', 'home_team', 'away_team')))
  final_df <- data.frame(final_df)
  #update lambdas / pts per shot / defense
  updated_stats_week <- updated_stats %>% filter(match_round == n)
  stats_grouped_update <- stats_create_2021(updated_stats_week)
  
  #update defense stats
  defense_grouped_update <- defense_func(updated_stats_week)
  def_weeks_update <- defensive_correction(defense_grouped_update, updated_stats_week)
  colnames(def_weeks_update) <- c('player_team', paste0('mean_game_shots_',n), paste0('league_mean_',n), paste0('def_diff_',n), paste0('def_pct_',n))
  combined_defense <- left_join(combined_defense, def_weeks_update, by = 'player_team')
  mean_weeks_update <- combined_defense %>% select(c('player_team', matches('mean_game_'))) 
  mean_weeks_update <- mean_weeks_update %>%
      mutate(data_sum = rowSums(across(where(is.numeric)), na.rm = TRUE) - mean_game_shots_0,
             update_alpha = (mean_game_shots_0 * 2) + data_sum,
             update_beta = 2 + n,
             update_lambda = update_alpha / update_beta)
  mean_league_update <- combined_defense %>% select(c('player_team', matches('league_mean_'))) %>%
    mutate(data_sum = rowSums(across(where(is.numeric)), na.rm=TRUE) - league_mean_0,
           update_alpha = (league_mean_0 * 2) + data_sum,
           update_beta = 2 + n,
           update_lambda = update_alpha / update_beta)
  def_corr_update <- data.frame(mean_league_update$player_team)
  colnames(def_corr_update) <- 'player_team'
  def_corr_update$league_mean <- lookup(def_corr_update$player_team, mean_league_update$player_team, mean_league_update$update_lambda)
  def_corr_update$mean_game_shots <- lookup(def_corr_update$player_team, mean_weeks_update$player_team, mean_weeks_update$update_lambda)
  def_corr_update <- def_corr_update %>%
    mutate(def_diff = mean_game_shots - league_mean,
           def_pct = mean_game_shots / league_mean)

  #update lambda
  update_lambda_rf <- predict(rf_gridsearch, newdata = stats_grouped_update[,-c(1:3)])
  update_lambda_gbm <- predict(gbm_model, newdata = stats_grouped_update[,-c(1:3)])
  update_lambda_xboost <- predict(xboost_model, newdata = stats_grouped_update[,-c(1:3)])
  #create ensemble        
  update_ensemble_df <- data.frame(cbind(update_lambda_rf, update_lambda_xboost, update_lambda_gbm))
  colnames(update_ensemble_df) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
  update_ensemble_preds <- predict(ensemble_model, newdata = update_ensemble_df)
  #update preds df
  update_lambda_df <- data.frame(cbind(stats_grouped_update$player_team, as.numeric(round(update_ensemble_preds,2))))
  colnames(update_lambda_df) <- c('player_team', paste0('week_', n))

  #posterior lambdas
  update_df_final <- left_join(update_df_final, update_lambda_df, by = 'player_team')
  update_df_final[,c(2:(n+1))] <- as.numeric(unlist(update_df_final[,c(2:(n+1))])) 
  update_df_final <- update_df_final %>% 
    mutate(data_sum = rowSums(across(where(is.numeric)), na.rm=TRUE))
  update_df_final <- left_join(update_df_final, lambda_loop, by = 'player_team')
  #create new alpha/beta
  update_df_final <- update_df_final %>%
    mutate(alpha_update = alpha + data_sum,
           beta_update = beta + n,
           mean_shots_update = alpha_update / beta_update)
  #update dfs for next run through
  lambda_loop <- lambda_loop %>% select(c(player_team))
  lambda_loop$mean_shots <- lookup(lambda_loop$player_team, update_df_final$player_team, update_df_final$mean_shots_update)
  lambda_loop$alpha <- lookup(lambda_loop$player_team, update_df_final$player_team, update_df_final$alpha_update)
  lambda_loop$beta <- lookup(lambda_loop$player_team, update_df_final$player_team, update_df_final$beta_update)
  #update_df_final
  update_df_final <- update_df_final %>% select(c(1:(n+1)))
  #bind final_df to output
  output_df <- as.data.frame(rbind(as.matrix(output_df), final_df))
  #add to n
  print(n)
  n <- n+1
}

################################################################################################################
#test vs markets to see performance
################################################################################################################
#totals
output_df <- output_df %>% select(-c(Kick.Off..local., Venue))
output_df <- output_df %>% mutate_at(c(4:56), as.numeric)
output_df$over_bet <- ifelse(output_df$total_high_quantile < output_df$Total.Score.Open, 'under','no bet')
output_df$over_bet <- ifelse(output_df$total_low_quantile > output_df$Total.Score.Open, 'over',output_df$over_bet)
output_df$over_act <- ifelse((as.numeric(output_df$Home.Score) + as.numeric(output_df$Away.Score)) > output_df$Total.Score.Open, 'over', 'under')
output_df$over_outcome <- ifelse(output_df$over_bet == output_df$over_act,'win','loss')
output_df$over_outcome <- ifelse(output_df$over_bet == 'no bet', 'no bet', output_df$over_outcome)
table(output_df$over_outcome)

#sides
output_df$side_bet <- ifelse(output_df$side > 0 & output_df$Home.Line.Open < 0 & output_df$side > abs(output_df$Home.Line.Open),'home','away')
output_df$side_bet <- ifelse(output_df$side > 0 & output_df$Home.Line.Open >= 0, 'home', output_df$side_bet)
output_df$side_bet <- ifelse(output_df$side < 0 & output_df$Home.Line.Open < 0,  'away', output_df$side_bet)
output_df$side_bet <- ifelse(output_df$side < 0 & output_df$Home.Line.Open > 0 & abs(output_df$side) < output_df$Home.Line.Open, 'home', output_df$side_bet)
output_df$side_act <- ifelse(output_df$Home.Line.Open <= 0 & (output_df$Home.Score - output_df$Away.Score) > abs(output_df$Home.Line.Open), 'home', 'away')
output_df$side_act <- ifelse(output_df$Home.Line.Open > 0 & (output_df$Away.Score - output_df$Home.Score) > output_df$Home.Line.Open, 'away', output_df$side_act)
output_df$side_outcome <- ifelse(output_df$side_act == output_df$side_bet, 'win', 'loss')
table(output_df$side_outcome)

#######################################################################################################################
#2023 season
#######################################################################################################################
#pull schedule for 2023
sched_2023_all <- fetch_fixture(season = 2023) %>% 
  select(c(compSeason.year, utcStartTime, round.roundNumber, home.team.name, home.team.abbreviation, home.team.nickname, away.team.name, away.team.abbreviation, away.team.nickname)) %>%
  separate(utcStartTime, c('Date', 'Time'), 'T') %>%
  separate(Time, c('Hour', 'Minute', 'Second'), ':') %>%
  select(-c('Second'))

#align home team names
sched_2023_all <- sched_team(sched_2023_all)

#add match_id
match_id_2023 <- fetch_player_stats_fryzigg(season = 2023)
colnames(match_id_2023)[3:5] <- c('home.team.name', 'away.team.name', 'Date')
match_id_2023 <- match_id_2023 %>% select(match_id, home.team.name, away.team.name, Date) %>% unique()
#attach match_id to sched_2023
sched_2023_all <- left_join(match_id_2023, sched_2023_all, by = c('Date', 'home.team.name', 'away.team.name'))

#pull home ground data
sched_2023_all$home_ground <- lookup(sched_2023_all$home.team.name, home_ground$team, home_ground$home_ground)
#pull away ground data to add travel
sched_2023_all$away_ground <- lookup(sched_2023_all$away.team.name, home_ground$team, home_ground$home_ground)
#pull home city and lat / long
sched_2023_all$home_city <- lookup(sched_2023_all$home.team.name, home_ground$team, home_ground$city)
sched_2023_all$home_city <- ifelse(sched_2023_all$home_city == 'New South Wales^', 'New South Wales', sched_2023_all$home_city)
sched_2023_all$home_lat <- lookup(sched_2023_all$home_city, aus_cities$City, aus_cities$Lat)
sched_2023_all$home_lon <- lookup(sched_2023_all$home_city, aus_cities$City, aus_cities$Long)
#pull away city and lat / long
sched_2023_all$away_city <- lookup(sched_2023_all$away.team.name, home_ground$team, home_ground$city)
sched_2023_all$away_city <- ifelse(sched_2023_all$away_city == 'New South Wales^', 'New South Wales', sched_2023_all$away_city)
sched_2023_all$away_lat <- lookup(sched_2023_all$away_city, aus_cities$City, aus_cities$Lat)
sched_2023_all$away_lon <- lookup(sched_2023_all$away_city, aus_cities$City, aus_cities$Long)

#filter games from 2023 that have already happened
sched_2023 <- sched_2023_all %>% filter(round.roundNumber <= 14)
sched_2023$Date <- as.character(sched_2023$Date)
sched_2023$Hour <- as.integer(sched_2023$Hour)
colnames(sched_2023)[c(4,6,15:17)] <- c('date', 'hour', 'city', 'lat', 'long')

#pull weather data 2023 
##############################################################################################################
#create weather df 2023
##############################################################################################################
weather_output_2023 <- weather_function(sched_2023, weather_api_base, API_KEY)
colnames(weather_output_2023)[1:4] <- c('date', 'lat', 'long','hour')
weather_output_2023$hour <- as.integer(weather_output_2023$hour)
weather_output_2023$date <- as.character(weather_output_2023$date)

write.csv(weather_output_2023, 'weather_output_2023.csv', row.names = FALSE)
weather_output_2023 <- read.csv("~/GitHub/afl-model/weather_output_2023.csv")


##############################################################################################################
#attach weather to points per kick df 2023
##############################################################################################################
sched_2023_weather <- left_join(sched_2023, weather_output_2023, by = c('date', 'hour', 'lat', 'long')) %>%
  distinct()
sched_2023_weather <- docklands(sched_2023_weather)

##############################################################################################################
#add travel column and add distance traveled by away team 2023
##############################################################################################################
#calc travel distance for awa#away travel
sched_2023_weather$travel <- NA
for(i in 1:nrow(sched_2023_weather)){
  sched_2023_weather$travel[i] <- distm(c(sched_2023_weather$lat[i],sched_2023_weather$long[i]),c(sched_2023_weather$away_lat[i], sched_2023_weather$away_lon[i]),
                                        fun = distHaversine)/1000
  print(i)
}

###############################################################################################################
#loop through the schedule
###############################################################################################################
update_df_final_2023 <- data.frame(sched_2023) %>% select(home.team.name) %>% unique()
colnames(update_df_final_2023) <- 'player_team'
lambda_loop_2023 <- lambda_initial_2023
lambda_loop_2023 <- lambda_loop_2023 %>% 
  mutate(alpha = mean_shots * 6,
         beta = 6)
def_corr_update_2023 <- def_stats_2022
updated_stats_2023 <- season_pull(2023,2023)
output_df_2023 <- data.frame()
defense_df_2023 <- data.frame()
df_2023_final <- data.frame()
combined_defense_2023 <- def_stats_2022
colnames(combined_defense_2023) <- c('player_team', 'mean_game_shots_0', 'league_mean_0', 'def_diff_0', 'def_pct_0')

#set initial n
n = 1

while(n < 15){
  final_df_2023 <- data.frame()
  #filter to just current week
  new_week_2023 <- sched_2023_weather %>% filter(round.roundNumber == n)

  for(k in 1:nrow(new_week_2023)){
    #lookup mean shots for home and away team
    home_lambda_2023 <- lookup(new_week_2023$home.team.name[k], lambda_loop_2023$player_team, lambda_loop_2023$mean_shots)
    away_lambda_2023 <- lookup(new_week_2023$away.team.name[k], lambda_loop_2023$player_team, lambda_loop_2023$mean_shots)
    #lookup adjustment for defense
    home_def_2023 <- lookup(new_week_2023$home.team.name[k], def_corr_update_2023$player_team, def_corr_update_2023$def_pct)
    away_def_2023 <- lookup(new_week_2023$away.team.name[k], def_corr_update_2023$player_team, def_corr_update_2023$def_pct)
    #travel_adj 
    travel_adj_2023 <- travel_coeff * sched_2023_weather$travel[k]
    #HFA
    hfa_adj_2023 <- home_base[2] - travel_int  - (sched_2023_weather$travel[k] * travel_coeff)
    #adjusted lambdas
    home_adj_2023 <- (home_lambda_2023 * away_def_2023)
    away_adj_2023 <- (away_lambda_2023 * home_def_2023) - hfa_adj_2023
    #pts_per_kick
    df_pts_2023 <- new_week_2023 %>% .[k:k,] %>% 
      select(c(home_ground, temp, humidity, dew, precip, precipprob, windspeed, winddir, conditions))
    pts_rf_2023 <- predict(rf_gridsearch_pts, newdata = df_pts_2023)
    pts_xboost_2023 <- predict(gbm_model_pts, newdata = df_pts_2023)
    pts_gbm_2023 <- predict(xboost_model_pts, newdata = df_pts_2023)
    lm_df_2023 <- data.frame(pts_rf_2023, pts_xboost_2023, pts_gbm_2023)
    colnames(lm_df_2023) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
    pts_per_kick_2023 <- predict(ensemble_model, lm_df_2023)
    
    #simulate 100,000 games with home_adj and away_adj as lambdas
    biv_pois_2023 <- as.data.frame(rbvpois(100000, home_adj_2023, away_adj_2023[1,1],0) * pts_per_kick_2023)
    colnames(biv_pois_2023) <- c('home', 'away')
    home_mean_2023 <- mean(biv_pois_2023$home)
    away_mean_2023 <- mean(biv_pois_2023$away)
    total_2023 <- biv_pois_2023$home + biv_pois_2023$away
    total_quant_2023 <- c(mean(total_2023), quantile(total_2023, probs = c(0.30,0.70)))
    output_2023 <- data.frame(new_week_2023$date[k], new_week_2023$home.team.name[k], new_week_2023$away.team.name[k], round(home_mean_2023,2), round(away_mean_2023,2), round(home_mean_2023 - away_mean_2023,2), round(total_quant_2023[1],2), round(total_quant_2023[2],2), round(total_quant_2023[3],2))
    final_df_2023 <- rbind(final_df_2023, output_2023)
    
  }
  
  #attach betting odds to final_df
  colnames(final_df_2023) <- c('date', 'home_team', 'away_team', 'home_mean_score', 'away_mean_score', 'side', 'total', 'total_low_quantile', 'total_high_quantile')
  final_df_2023$date <- as.Date(final_df_2023$date)
  final_df_2023 <- left_join(final_df_2023, afl_historical_odds_join, by = (c('date', 'home_team', 'away_team')))
  final_df_2023 <- data.frame(final_df_2023)
  #update lambdas / pts per shot / defense
  updated_stats_week_2023 <- updated_stats_2023 %>% filter(match_round == n)
  stats_grouped_update_2023 <- stats_create_2021(updated_stats_week_2023)
  
  #update defense stats
  defense_grouped_update_2023 <- defense_func(updated_stats_week_2023)
  def_weeks_update_2023 <- defensive_correction(defense_grouped_update_2023, updated_stats_week_2023)
  colnames(def_weeks_update_2023) <- c('player_team', paste0('mean_game_shots_',n), paste0('league_mean_',n), paste0('def_diff_',n), paste0('def_pct_',n))
  combined_defense_2023 <- left_join(combined_defense_2023, def_weeks_update_2023, by = 'player_team')
  mean_weeks_update_2023 <- combined_defense_2023 %>% select(c('player_team', matches('mean_game_'))) 
  mean_weeks_update_2023 <- mean_weeks_update_2023 %>%
    mutate(data_sum = rowSums(across(where(is.numeric)), na.rm = TRUE) - mean_game_shots_0,
           update_alpha = (mean_game_shots_0 * 2) + data_sum,
           update_beta = 2 + n,
           update_lambda = update_alpha / update_beta)
  mean_league_update_2023 <- combined_defense_2023 %>% select(c('player_team', matches('league_mean_'))) %>%
    mutate(data_sum = rowSums(across(where(is.numeric)), na.rm=TRUE) - league_mean_0,
           update_alpha = (league_mean_0 * 2) + data_sum,
           update_beta = 2 + n,
           update_lambda = update_alpha / update_beta)
  def_corr_update_2023 <- data.frame(mean_league_update_2023$player_team)
  colnames(def_corr_update_2023) <- 'player_team'
  def_corr_update_2023$league_mean <- lookup(def_corr_update_2023$player_team, mean_league_update_2023$player_team, mean_league_update_2023$update_lambda)
  def_corr_update_2023$mean_game_shots <- lookup(def_corr_update_2023$player_team, mean_weeks_update_2023$player_team, mean_weeks_update_2023$update_lambda)
  def_corr_update_2023 <- def_corr_update_2023 %>%
    mutate(def_diff = mean_game_shots - league_mean,
           def_pct = mean_game_shots / league_mean)
  
  #update lambda
  update_lambda_rf_2023 <- predict(rf_gridsearch, newdata = stats_grouped_update_2023[,-c(1:3)])
  update_lambda_gbm_2023 <- predict(gbm_model, newdata = stats_grouped_update_2023[,-c(1:3)])
  update_lambda_xboost_2023 <- predict(xboost_model, newdata = stats_grouped_update_2023[,-c(1:3)])
  #create ensemble        
  update_ensemble_df_2023 <- data.frame(cbind(update_lambda_rf_2023, update_lambda_xboost_2023, update_lambda_gbm_2023))
  colnames(update_ensemble_df_2023) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
  update_ensemble_preds_2023 <- predict(ensemble_model, newdata = update_ensemble_df_2023)
  #update preds df
  update_lambda_df_2023 <- data.frame(cbind(stats_grouped_update_2023$player_team, as.numeric(round(update_ensemble_preds_2023,2))))
  colnames(update_lambda_df_2023) <- c('player_team', paste0('week_', n))
  
  #posterior lambdas
  update_df_final_2023 <- left_join(update_df_final_2023, update_lambda_df_2023, by = 'player_team')
  update_df_final_2023[,c(2:(n+1))] <- as.numeric(unlist(update_df_final_2023[,c(2:(n+1))])) 
  update_df_final_2023 <- update_df_final_2023 %>% 
    mutate(data_sum = rowSums(across(where(is.numeric)), na.rm=TRUE))
  update_df_final_2023 <- left_join(update_df_final_2023, lambda_loop_2023, by = 'player_team')
  #create new alpha/beta
  update_df_final_2023 <- update_df_final_2023 %>%
    mutate(alpha_update = alpha + data_sum,
           beta_update = beta + n,
           mean_shots_update = alpha_update / beta_update)
  #update dfs for next run through
  lambda_loop_2023 <- lambda_loop_2023 %>% select(c(player_team))
  lambda_loop_2023$mean_shots <- lookup(lambda_loop_2023$player_team, update_df_final_2023$player_team, update_df_final_2023$mean_shots_update)
  lambda_loop_2023$alpha <- lookup(lambda_loop_2023$player_team, update_df_final_2023$player_team, update_df_final_2023$alpha_update)
  lambda_loop_2023$beta <- lookup(lambda_loop_2023$player_team, update_df_final_2023$player_team, update_df_final_2023$beta_update)
  #update_df_final
  update_df_final_2023 <- update_df_final_2023 %>% select(c(1:(n+1)))
  #bind final_df to output
  output_df_2023 <- as.data.frame(rbind(as.matrix(output_df_2023), final_df_2023))
  #output_df_2023 <- rbind(output_df_2023, final_df_2023)
  print(n)
  n <- n+1
}

################################################################################################################
#test vs markets to see performance
################################################################################################################
#totals
output_df_2023 <- output_df_2023 %>% select(-c(10,11,14))
output_df_2023 <- output_df_2023 %>% mutate_at(c(4:55), as.numeric)
output_df_2023$over_bet <- ifelse(output_df_2023$total_high_quantile < output_df_2023$Total.Score.Open, 'under','no bet')
output_df_2023$over_bet <- ifelse(output_df_2023$total_low_quantile > output_df_2023$Total.Score.Open, 'over',output_df_2023$over_bet)
output_df_2023$over_act <- ifelse((as.numeric(output_df_2023$Home.Score) + as.numeric(output_df_2023$Away.Score)) > output_df_2023$Total.Score.Open, 'over', 'under')
output_df_2023$over_outcome <- ifelse(output_df_2023$over_bet == output_df_2023$over_act,'win','loss')
output_df_2023$over_outcome <- ifelse(output_df_2023$over_bet == 'no bet', 'no bet', output_df_2023$over_outcome)
table(output_df_2023$over_outcome)

#sides
output_df_2023$side_bet <- ifelse(output_df_2023$side > 0 & output_df_2023$Home.Line.Open < 0 & output_df_2023$side > abs(output_df_2023$Home.Line.Open),'home','away')
output_df_2023$side_bet <- ifelse(output_df_2023$side > 0 & output_df_2023$Home.Line.Open >= 0, 'home', output_df_2023$side_bet)
output_df_2023$side_bet <- ifelse(output_df_2023$side < 0 & output_df_2023$Home.Line.Open < 0,  'away', output_df_2023$side_bet)
output_df_2023$side_bet <- ifelse(output_df_2023$side < 0 & output_df_2023$Home.Line.Open > 0 & abs(output_df_2023$side) < output_df_2023$Home.Line.Open, 'home', output_df_2023$side_bet)
output_df_2023$side_act <- ifelse(output_df_2023$Home.Line.Open <= 0 & (output_df_2023$Home.Score - output_df_2023$Away.Score) > abs(output_df_2023$Home.Line.Open), 'home', 'away')
output_df_2023$side_act <- ifelse(output_df_2023$Home.Line.Open > 0 & (output_df_2023$Away.Score - output_df_2023$Home.Score) > output_df_2023$Home.Line.Open, 'away', output_df_2023$side_act)
output_df_2023$side_outcome <- ifelse(output_df_2023$side_act == output_df_2023$side_bet, 'win', 'loss')
table(output_df_2023$side_outcome)

################################################################################################################
#predictions for new week
################################################################################################################
#pull schedule for 2023
current_week <- fetch_fixture(season = 2023) %>% 
  select(c(compSeason.year, utcStartTime, round.roundNumber, home.team.name, home.team.abbreviation, home.team.nickname, away.team.name, away.team.abbreviation, away.team.nickname)) %>%
  separate(utcStartTime, c('Date', 'Time'), 'T') %>%
  separate(Time, c('Hour', 'Minute', 'Second'), ':') %>%
  select(-c('Second')) %>% filter(round.roundNumber == 17)

#align home team names
current_week <- sched_team(current_week)

#pull home ground data
current_week$home_ground <- lookup(current_week$home.team.name, home_ground$team, home_ground$home_ground)
#pull away ground data to add travel
current_week$away_ground <- lookup(current_week$away.team.name, home_ground$team, home_ground$home_ground)
#pull home city and lat / long
current_week$home_city <- lookup(current_week$home.team.name, home_ground$team, home_ground$city)
current_week$home_city <- ifelse(current_week$home_city == 'New South Wales^', 'New South Wales', current_week$home_city)
current_week$home_lat <- lookup(current_week$home_city, aus_cities$City, aus_cities$Lat)
current_week$home_lon <- lookup(current_week$home_city, aus_cities$City, aus_cities$Long)
#pull away city and lat / long
current_week$away_city <- lookup(current_week$away.team.name, home_ground$team, home_ground$city)
current_week$away_city <- ifelse(current_week$away_city == 'New South Wales^', 'New South Wales', current_week$away_city)
current_week$away_lat <- lookup(current_week$away_city, aus_cities$City, aus_cities$Lat)
current_week$away_lon <- lookup(current_week$away_city, aus_cities$City, aus_cities$Long)

#pull weather data 2023 
##############################################################################################################
#create weather df 2023
##############################################################################################################
colnames(current_week)[c(2,3,14:16)] <- c('date', 'hour', 'city','lat','long') 
weather_output_new <- weather_function(current_week, weather_api_base, API_KEY)
colnames(weather_output_new)[1:3] <- c('date', 'lat', 'long')

##############################################################################################################
#add travel column and add distance traveled by away team 2023
##############################################################################################################
#calc travel distance for awa#away travel
current_week$travel <- NA
for(i in 1:nrow(current_week)){
  current_week$travel[i] <- distm(c(current_week$lat[i],current_week$long[i]),c(current_week$away_lat[i], current_week$away_lon[i]),
                                        fun = distHaversine)/1000
  print(i)
}

##############################################################################################################
#new week predictions
##############################################################################################################
current_week <- left_join(current_week, weather_output_new, by = c('date', 'hour', 'lat', 'long'), relationship = 'many-to-many') %>%
  distinct()
current_week <- docklands(current_week)
final_df_new <- data.frame()
spread_list <- list()
total_list <- list()
pts_new <- data.frame()

for(k in 1:nrow(current_week)){
  #lookup mean shots for home and away team
  home_lambda_new <- lookup(current_week$home.team.name[k], lambda_initial_2023$player_team, lambda_initial_2023$mean_shots)
  away_lambda_new <- lookup(current_week$away.team.name[k], lambda_initial_2023$player_team, lambda_initial_2023$mean_shots)
  #lookup adjustment for defense
  home_def_new <- lookup(current_week$home.team.name[k], def_corr_update_2023$player_team, def_corr_update_2023$def_pct)
  away_def_new <- lookup(current_week$away.team.name[k], def_corr_update_2023$player_team, def_corr_update_2023$def_pct)
  #travel_adj 
  travel_adj_new <- travel_coeff * current_week$travel[k]
  #HFA
  hfa_adj_new <- home_base[2] - travel_int  - (current_week$travel[k] * travel_coeff)
  #adjusted lambdas
  home_adj_new <- (home_lambda_new * away_def_new)
  away_adj_new <- (away_lambda_new * home_def_new) - hfa_adj_new
  #pts_per_kick
  
  df_pts_new <- current_week %>% .[k:k,] %>% 
    select(c(home_ground, temp, humidity, dew, precip, precipprob, windspeed, winddir, conditions))
  pts_rf_new <- predict(rf_gridsearch_pts, newdata = df_pts_new)
  pts_xboost_new <- predict(gbm_model_pts, newdata = df_pts_new)
  pts_gbm_new <- predict(xboost_model_pts, newdata = df_pts_new)
  lm_df_new <- data.frame(pts_rf_new, pts_xboost_new, pts_gbm_new)
  colnames(lm_df_new) <- c('preds_rf', 'preds_gbm', 'preds_xboost')
  pts_per_kick_new <- predict(ensemble_model, lm_df_new)
  
  #simulate 100,000 games with home_adj and away_adj as lambdas
  biv_pois_new <- as.data.frame(rbvpois(100000, home_adj_new, away_adj_new[1,1],0) * pts_per_kick_new)
  colnames(biv_pois_new) <- c('home', 'away')
  home_mean_new <- mean(biv_pois_new$home)
  away_mean_new <- mean(biv_pois_new$away)
  moneyline <- as.data.frame(cbind(biv_pois_new$home, biv_pois_new$away))
  colnames(moneyline) <- c('home', 'away')
  moneyline$winner <- ifelse(moneyline$home > moneyline$away, 1,0)
  home_moneyline <- sum(moneyline$winner) / nrow(moneyline)
  away_moneyline <- 1-home_moneyline
  total_new <- biv_pois_new$home + biv_pois_new$away
  total_quant_new <- c(mean(total_new), quantile(total_new, probs = c(0.35,0.65)))
  output_new <- c(current_week$date[k], current_week$home.team.name[k], current_week$away.team.name[k], round(home_mean_new,2), round(away_mean_new,2), round(home_mean_new - away_mean_new,2), round(total_quant_new,2), round(home_moneyline,2), round(away_moneyline,2))
  final_df_new <- rbind(final_df_new, output_new)
  colnames(final_df_new) <- c('date', 'home_team', 'away_team', 'home_mean_score', 'away_mean_score', 'side', 'total', 'total_low_quantile', 'total_high_quantile', 'home_moneyline', 'away_moneyline')
  
  #histogram
  pt_diff <- data.frame(biv_pois_new$home - biv_pois_new$away)
  colnames(pt_diff) <- 'spread'
  
  d2 <- pt_diff %>%
    summarize(lower = quantile(spread, probs = 0.35),
              upper = quantile(spread, probs = 0.65))
  p <- ggplot(pt_diff, aes(x = spread)) +
        geom_density(aes(fill = 'red')) +
        geom_vline(data = d2, aes(xintercept = lower)) +
        annotate('text', label = round(d2$lower, 2), x = d2$lower-20, y = .010) + 
        geom_vline(data = d2, aes(xintercept = upper)) +
        annotate('text', label = round(d2$upper,2), x = d2$upper+20, y = .010) +
        ggtitle(paste0(current_week$home.team.name[k],' vs. ', current_week$away.team.name[k]))

  spread_list[[k]] <- p
  
  total <- data.frame(biv_pois_new$home + biv_pois_new$away)
  colnames(total) <- 'overunder'

  d3 <- total %>%
    summarize(lower = quantile(overunder, probs = 0.44),
              upper = quantile(overunder, probs = 0.56))
  p2 <- ggplot(total, aes(x = overunder)) +
    geom_density(aes(fill = 'green')) +
    geom_vline(data = d3, aes(xintercept = lower)) +
    annotate('text', label = round(d3$lower, 2), x = d3$lower-20, y = .010) + 
    geom_vline(data = d3, aes(xintercept = upper)) +
    annotate('text', label = round(d3$upper,2), x = d3$upper+20, y = .010) +
    ggtitle(paste0(current_week$home.team.name[k],' vs. ', current_week$away.team.name[k]))
  
  total_list[[k]] <- p2
  #save df pts_per_week
  pts_new <- rbind(pts_new, pts_per_kick_new)
  
}

write.csv(final_df_new, 'final_df_17.csv', row.names = FALSE)

############################################################################################################
#power rankings
############################################################################################################
power_rank <- lambda_loop_2023 %>% select(c(player_team, mean_shots))
power_rank$league_mean <- mean(lambda_loop_2023$mean_shots)
power_rank$def_adj <- lookup(power_rank$player_team, def_corr_update_2023$player_team, def_corr_update_2023$def_pct)
power_rank$def_shots <- power_rank$def_adj * power_rank$league_mean
rank_pts_2023 <- updated_stats_2023 %>%
  summarise(pts = (sum(goals * 6) + sum(behinds))/sum(shots_at_goal))
power_pts <- as.numeric(rank_pts_2023[1,1])
power_rank <- power_rank %>%
  mutate(offense_rank = mean_shots * power_pts,
         defense_rank = def_shots * power_pts)
power_rank$pts_vs_avg <- power_rank$offense_rank - power_rank$defense_rank
power_rank <- power_rank %>% 
  select(c(1,6:8))

write.csv(power_rank, 'power_rank.csv', row.names = FALSE)


##############################################################################################################
#totals CLV
##############################################################################################################
totals_clv <- season_pull(2021,2023)
totals_clv_df <- totals_clv %>%
  group_by(match_id) %>%
  mutate(total = match_home_team_score + match_away_team_score) %>%
  select(match_id, total) %>%
  distinct()

write.csv(totals_clv_df, 'totals_clv.csv', row.names = FALSE)