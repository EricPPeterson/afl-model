library(fitzRoy)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(lookup)

#fetch fixtures
seasons <- c(2014:2022)
fixtures_afl <- data.frame()
for(i in 1:length(seasons)){
  current_season <- fetch_fixture(season = seasons[i])
  fixtures_afl <- bind_rows(fixtures_afl, current_season)
}

#fetch historical odds
afl_historical_odds <- read.csv("~/GitHub/afl model/afl_historical_odds.csv")
afl_historical_odds$Home.Team[afl_historical_odds$Home.Team == 'Brisbane'] <- 'Brisbane Lions'
afl_historical_odds$Away.Team[afl_historical_odds$Away.Team == 'Brisbane'] <- 'Brisbane Lions'
afl_historical_odds$Home.Team[afl_historical_odds$Home.Team == 'GWS Giants'] <- 'Greater Western Sydney'
afl_historical_odds$Away.Team[afl_historical_odds$Away.Team == 'GWS Giants'] <- 'Greater Western Sydney'


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
by_year_open <- table(afl_historical_odds$Year, afl_historical_odds$Open.Total) 
by_year_close <- table(afl_historical_odds$Year, afl_historical_odds$Close.Total)
by_year_open
by_year_close

#amount off on total by year
error_open_year <- afl_historical_odds %>%
  group_by(Year) %>%
  summarise(mean_open_error = mean(abs(Open.Error)),
         mean_close_error = mean(abs(Close.Error)),
         mean_max_error = mean(abs(Max.Error.Over)),
         mean_min_error = mean(abs(Max.Error.Under)))

#homefield advantage
afl_historical_odds <- afl_historical_odds %>% mutate(HFA = Home.Score - Away.Score)
HFA_by_year <- afl_historical_odds %>% group_by(Year) %>%
  summarize(home_field_mean = mean(HFA),
            home_field_median = median(HFA))
HFA_by_year

#fetch tables for every year except 2020
#pull 2014 data
table_squiggle_2014 <- fetch_ladder_squiggle(season = 2014)
table_squiggle_2014$season <- as.numeric(2014)
table_squiggle_2014 <- table_squiggle_2014 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2014 <- table_squiggle_2014 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2014 <- table_squiggle_2014 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2015 data
table_squiggle_2015 <- fetch_ladder_squiggle(season = 2015)
table_squiggle_2015$season <- as.numeric(2015)
table_squiggle_2015 <- table_squiggle_2015 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2015 <- table_squiggle_2015 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2015 <- table_squiggle_2015 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2016 data
table_squiggle_2016 <- fetch_ladder_squiggle(season = 2016)
table_squiggle_2016$season <- as.numeric(2016)
table_squiggle_2016 <- table_squiggle_2016 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2016 <- table_squiggle_2016 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2016 <- table_squiggle_2016 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2017 data
table_squiggle_2017 <- fetch_ladder_squiggle(season = 2017)
table_squiggle_2017$season <- as.numeric(2017)
table_squiggle_2017 <- table_squiggle_2017 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2017 <- table_squiggle_2017 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2017 <- table_squiggle_2017 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2018 data
table_squiggle_2018 <- fetch_ladder_squiggle(season = 2018)
table_squiggle_2018$season <- as.numeric(2018)
table_squiggle_2018 <- table_squiggle_2018 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2018 <- table_squiggle_2018 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2018 <- table_squiggle_2018 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2019 data
table_squiggle_2019 <- fetch_ladder_squiggle(season = 2019)
table_squiggle_2019$season <- as.numeric(2019)
table_squiggle_2019 <- table_squiggle_2019 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2019 <- table_squiggle_2019 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2019 <- table_squiggle_2019 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2021 data
table_squiggle_2021 <- fetch_ladder_squiggle(season = 2021)
table_squiggle_2021$season <- as.numeric(2021)
table_squiggle_2021 <- table_squiggle_2021 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2021 <- table_squiggle_2021 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2021 <- table_squiggle_2021 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#pull 2022 data
table_squiggle_2022 <- fetch_ladder_squiggle(season = 2022)
table_squiggle_2022$season <- as.numeric(2022)
table_squiggle_2022 <- table_squiggle_2022 %>% mutate(points_scored_rank = dense_rank(desc(for.)))
table_squiggle_2022 <- table_squiggle_2022 %>% mutate(points_allowed_rank = dense_rank(desc(-against)))
table_squiggle_2022 <- table_squiggle_2022 %>% mutate(percentage_rank = dense_rank(desc(percentage)))
#bind tables together
tables <- bind_rows(table_squiggle_2014, table_squiggle_2015, table_squiggle_2016, table_squiggle_2017, 
                    table_squiggle_2018, table_squiggle_2019, table_squiggle_2021, table_squiggle_2022)
#delete tables
rm(table_squiggle_2014, table_squiggle_2015, table_squiggle_2016, table_squiggle_2017, table_squiggle_2018,
                    table_squiggle_2019, table_squiggle_2021, table_squiggle_2022)

#pythagorean wins
tables$pythagorean_wins_pct <- 1 / (1 + ((tables$against/tables$for.)^3.87))
tables$pythagorean_wins <- tables$pythagorean_wins_pct * 22

#check team rankings and year over year changes in pts scored and allowed
library(rlist)
teams <- unique(tables$name)
list_df <- list()
for(i in 1:length(teams)){
  list_df[[i]] <- assign(paste0('df_',teams[i]), data.frame())
  tables_team <- tables %>% filter(name == teams[i])
  list_df[[i]] <- rbind(list_df[[i]], tables_team)
}

rm(df_Adelaide, 'df_Brisbane Lions', df_Carlton, df_Collingwood, df_Essendon, df_Fremantle, df_Geelong,
   'df_Gold Coast', 'df_Greater Western Sydney', df_Hawthorn, df_Melbourne, 'df_North Melbourne', 'df_Port Adelaide', df_Richmond,
   'df_St Kilda', df_Sydney, 'df_West Coast', 'df_Western Bulldogs')

#create graphs of year over year changes in points by team
library(ggplot2)
for(j in 1:length(list_df)){
  next_team <- list_df[[j]]
  colors <- c('for.' = 'blue', 'against' = 'red')
  p <- ggplot(next_team, aes(x = season)) + 
    geom_line(aes(y = for., color = 'for.')) + 
    geom_line(aes(y = against, color = 'against')) + 
    labs(y = 'Points', x = 'Year') +
    ggtitle(next_team$name[1], 'Points Scored and allowed by Year') + 
    scale_color_manual(values = colors)
  print(p)
}

#graph of pythagorean wins
for(j in 1:length(list_df)){
  next_team <- list_df[[j]]
  p <- ggplot(next_team, aes(x = season, y = pythagorean_wins)) + 
    geom_line() + 
    labs(y = 'Estimated Wins', x = 'Year') + 
    ggtitle(next_team$name[1], 'Pythagorean Wins by Year')
  print(p)
}

#graph of percentage for / against points
for(j in 1:length(list_df)){
  next_team <- list_df[[j]]
  p <- ggplot(next_team, aes(x = season, y = percentage)) + 
    geom_line() + 
    labs(y = 'Percentage', x = 'Year') + 
    ggtitle(next_team$name[1], 'For / Against Points') +
    ylim(0,150) + 
    scale_x_continuous(n.breaks = 10) + 
    scale_y_continuous(n.breaks = 10)
  print(p)
}

#check for players changing teams
current_players_by_team <- fetch_player_details()
colnames(current_players_by_team)[4] <- 'current_team'
last_year_players_by_team <- fetch_player_details(current = FALSE)
last_year_players_by_team <- last_year_players_by_team %>% filter(season == 2022)
colnames(last_year_players_by_team)[5] <- 'last_team'

#players who changed teams
player_changing_teams <- left_join(current_players_by_team, last_year_players_by_team, by = 'id') %>%
  dplyr :: select(firstName.x, surname.x, id, current_team, last_team, position.x)
colnames(player_changing_teams) <- c('firstName', 'surname', 'id', 'current_team', 'last_team', 'position')
player_changing_teams <- player_changing_teams %>% filter(current_team != last_team)

#new players (i.e. players not on a roster from last year. Consider league averages for their stats)
new_players <- current_players_by_team %>% filter(!current_players_by_team$id %in% last_year_players_by_team$id)


#stats work
#roster data
player_details_2022 <- fetch_player_details_afl(season = 2022)
player_details_2023 <- fetch_player_details_afl(season = 2023)
lineups_2021 <- fetch_lineup_afl(season = 2021)
lineups_2021$season <- 2021
lineups_2022 <- fetch_lineup_afl(season = 2022)
lineups_2022$season <- 2022

#team stats
advanced_stats_offense_2021 <- read.csv("~/GitHub/afl model/advanced_stats_offense_2021.csv")
advanced_stats_offense_2022 <- read.csv("~/GitHub/afl model/advanced_stats_offense_2022.csv")
advanced_stats_opposition_2021 <- read.csv("~/GitHub/afl model/advanced_stats_opposition_2021.csv")
advanced_stats_opposition_2022 <- read.csv("~/GitHub/afl model/advanced_stats_opposition_2022.csv")


#player game stats
#fetch player stats
stats_2021 <- fetch_player_stats(season = 2021)
stats_2021$season <- 2021
stats_2022 <- fetch_player_stats(season = 2022)
stats_2022$season <- 2022

#results stats
results_2021 <- fetch_results(season = 2021)
results_2021$season <- 2021

#match stats
match_stats <- get_afltables_stats(start_date = '2021-03-01', end_date = Sys.Date())
match_stats<- match_stats %>%
  mutate(first_quarter_points_home = (HQ1G * 6) + HQ1B,
         second_quarter_points_home = (HQ2G-HQ1G) * 6 + (HQ2B-HQ1B),
         third_quarter_points_home = (HQ3G-HQ2G) * 6 + (HQ3B-HQ2B),
         fourth_quarter_points_home = (HQ4G-HQ3G) * 6 + (HQ4B-HQ3B),
         first_quarter_points_away = (HQ1G * 6) + HQ1B,
         second_quarter_points_away = (HQ2G-HQ1G) * 6 + (HQ2B-HQ1B),
         third_quarter_points_away = (HQ3G-HQ2G) * 6 + (HQ3B - HQ2B),
         fourth_quarter_points_away = (HQ4G-HQ3G) * 6 + (HQ4B - HQ3B))

quarterly_scoring <- match_stats %>% group_by(Date, Home.team, Away.team) %>%
  select(Date, Home.team, Away.team, first_quarter_points_home, first_quarter_points_away,
         second_quarter_points_home, second_quarter_points_away, third_quarter_points_home,
         third_quarter_points_away, fourth_quarter_points_home, fourth_quarter_points_away)
quarterly_scoring <- distinct(quarterly_scoring)

#left_join quarterly scoring and historical odds
afl_historical_odds <- afl_historical_odds %>%
  mutate(Date = ymd(Date))
colnames(afl_historical_odds)[3:4] <- c('Home.team', 'Away.team')
quarterly_scoring <- left_join(quarterly_scoring, afl_historical_odds, by = c('Date', 'Home.team', 'Away.team')) %>%
  select(Date, Home.team, Away.team, first_quarter_points_home, first_quarter_points_away, second_quarter_points_home,
         second_quarter_points_away, third_quarter_points_home, third_quarter_points_away, fourth_quarter_points_home,
         fourth_quarter_points_away, Total.Score.Open, Total.Score.Close, Home.Odds.Close, Away.Odds.Close, 
         Home.Line.Open, Home.Line.Close, Away.Line.Open, Away.Line.Close) %>%
  mutate(total_pts_first_quarter = sum(first_quarter_points_home + first_quarter_points_away),
         total_pts_second_quarter = sum(second_quarter_points_home + second_quarter_points_away),
         total_pts_third_quarter = sum(third_quarter_points_home + third_quarter_points_away),
         total_pts_fourth_quarter = sum(fourth_quarter_points_home + fourth_quarter_points_away))

#average points scored by quarter
print(paste0('average 1st quarter points: ', round(mean(quarterly_scoring$total_pts_first_quarter),2)))
print(paste0('average 2nd quarter points: ', round(mean(quarterly_scoring$total_pts_second_quarter),2)))
print(paste0('average 3rd quarter points: ', round(mean(quarterly_scoring$total_pts_third_quarter),2)))
print(paste0('average 4th quarter points: ', round(mean(quarterly_scoring$total_pts_fourth_quarter),2)))


#fryzigg stats
fry_stats_2021 <- get_fryzigg_stats(start = 2021, end = 2021)
fry_stats_2021$season <- 2021
fry_stats_2022 <- get_fryzigg_stats(start = 2022, end = 2022)
fry_stats_2022$season <- 2022

#aggregate fry_stats 
agg_fry_stats_2021 <- fry_stats_2021 %>% group_by(match_id, player_team) %>%
  summarise(match_home_team_goals = mean(match_home_team_goals),
         match_home_team_behinds = mean(match_home_team_behinds),
         match_home_team_score = mean(match_home_team_score),
         match_away_team_goals = mean(match_away_team_goals),
         match_away_team_behinds = mean(match_away_team_behinds),
         match_away_team_score = mean(match_away_team_score),
         match_margin = mean(match_margin),
         total_marks = sum(marks),
         total_handballs = sum(handballs),
         total_disposals = sum(disposals),
         total_eff_disposals = sum(effective_disposals),
         disp_efficency = total_eff_disposals/total_disposals,
         total_hitouts = sum(hitouts),
         total_tackles = sum(tackles),
         total_rebounds = sum(rebounds),
         total_ins_50s = sum(inside_fifties),
         total_clearances = sum(clearances),
         total_clangers = sum(clangers),
         total_fk_for = sum(free_kicks_for),
         total_fk_against = sum(free_kicks_against),
         total_contested_poss = sum(contested_possessions),
         total_uncontested_poss = sum(uncontested_possessions),
         ratio_poss = sum(contested_possessions)/sum(uncontested_possessions),
         total_ins_50_marks = sum(marks_inside_fifty),
         total_one_perc = sum(one_percenters),
         total_bounces = sum(bounces),
         total_centre_clearances = sum(centre_clearances),
         total_stoppage_clearances = sum(stoppage_clearances),
         total_metres_gained = sum(metres_gained),
         total_turnovers = sum(turnovers),
         total_intercepts = sum(intercepts),
         total_tackles_inside_50 = sum(tackles_inside_fifty),
         total_contested_def_losses = sum(contest_def_losses),
         total_cont_def_one_on_ones = sum(contest_def_one_on_ones),
         total_cont_off_one_on_ones = sum(contest_off_one_on_ones),
         total_contested_off_wins = sum(contest_off_wins),
         total_def_half_pressure_acts = sum(def_half_pressure_acts),
         total_eff_kicks = sum(effective_kicks),
         total_f50_ground_ball_gets = sum(f50_ground_ball_gets),
         total_ground_ball_gets = sum(ground_ball_gets),
         total_hitouts_to_advantage = sum(hitouts_to_advantage),
         total_intercept_marks = sum(intercept_marks),
         total_marks_on_lead = sum(marks_on_lead),
         total_pressure_acts = sum(pressure_acts),
         total_ruck_contests = sum(ruck_contests),
         total_shots = sum(shots_at_goal),
         goal_eff = sum(goals)/sum(shots_at_goal),
         scoring_eff = (sum(goals) + sum(behinds))/sum(shots_at_goal),
         sum_spoils = sum(spoils)
         )
agg_fry_stats_2022 <- fry_stats_2022 %>% group_by(match_id, player_team) %>%
  summarise(match_home_team_goals = mean(match_home_team_goals),
            match_home_team_behinds = mean(match_home_team_behinds),
            match_home_team_score = mean(match_home_team_score),
            match_away_team_goals = mean(match_away_team_goals),
            match_away_team_behinds = mean(match_away_team_behinds),
            match_away_team_score = mean(match_away_team_score),
            match_margin = mean(match_margin),
            total_marks = sum(marks),
            total_handballs = sum(handballs),
            total_disposals = sum(disposals),
            total_eff_disposals = sum(effective_disposals),
            disp_efficency = total_eff_disposals/total_disposals,
            total_hitouts = sum(hitouts),
            total_tackles = sum(tackles),
            total_rebounds = sum(rebounds),
            total_ins_50s = sum(inside_fifties),
            total_clearances = sum(clearances),
            total_clangers = sum(clangers),
            total_fk_for = sum(free_kicks_for),
            total_fk_against = sum(free_kicks_against),
            total_contested_poss = sum(contested_possessions),
            total_uncontested_poss = sum(uncontested_possessions),
            ratio_poss = sum(contested_possessions)/sum(uncontested_possessions),
            total_ins_50_marks = sum(marks_inside_fifty),
            total_one_perc = sum(one_percenters),
            total_bounces = sum(bounces),
            total_centre_clearances = sum(centre_clearances),
            total_stoppage_clearances = sum(stoppage_clearances),
            total_metres_gained = sum(metres_gained),
            total_turnovers = sum(turnovers),
            total_intercepts = sum(intercepts),
            total_tackles_inside_50 = sum(tackles_inside_fifty),
            total_contested_def_losses = sum(contest_def_losses),
            total_cont_def_one_on_ones = sum(contest_def_one_on_ones),
            total_cont_off_one_on_ones = sum(contest_off_one_on_ones),
            total_contested_off_wins = sum(contest_off_wins),
            total_def_half_pressure_acts = sum(def_half_pressure_acts),
            total_eff_kicks = sum(effective_kicks),
            total_f50_ground_ball_gets = sum(f50_ground_ball_gets),
            total_ground_ball_gets = sum(ground_ball_gets),
            total_hitouts_to_advantage = sum(hitouts_to_advantage),
            total_intercept_marks = sum(intercept_marks),
            total_marks_on_lead = sum(marks_on_lead),
            total_pressure_acts = sum(pressure_acts),
            total_ruck_contests = sum(ruck_contests),
            total_shots = sum(shots_at_goal),
            goal_eff = sum(goals)/sum(shots_at_goal),
            scoring_eff = (sum(goals) + sum(behinds))/sum(shots_at_goal),
            sum_spoils = sum(spoils)
  )

#tag rows as either home / away to manipulate data. 
#currently all game data is on 2 rows.
rows_2021 <- nrow(agg_fry_stats_2021)
rows_2022 <- nrow(agg_fry_stats_2022)
odd_rows_2021 <- seq_len(rows_2021) %% 2
agg_fry_stats_2021$home_away <- odd_rows_2021
home_away <-  function(x) ifelse(x == 1, 'Home', 'Away')
agg_fry_stats_2021$home_away <- home_away(agg_fry_stats_2021$home_away)
odd_rows_2022 <- seq_len(rows_2022) %% 2
agg_fry_stats_2022$home_away <- odd_rows_2022
agg_fry_stats_2022$home_away <- home_away(agg_fry_stats_2022$home_away)

#split data by home and away
home_2021_data <- agg_fry_stats_2021 %>% 
  filter(home_away == 'Home')
away_2021_data <- agg_fry_stats_2021 %>%
  filter(home_away == 'Away')
home_2022_data <- agg_fry_stats_2022 %>% 
  filter(home_away == 'Home')
away_2022_data <- agg_fry_stats_2022 %>%
  filter(home_away == 'Away')

#filter unneccessary data away from each df to rebuild each game as single row
home_2021_data <- home_2021_data %>%
  select(-c(match_away_team_goals, match_away_team_behinds, match_away_team_score, home_away))
colnames(home_2021_data)[2] <- 'home_team'
colnames(home_2021_data)[7:48] <- paste(colnames(home_2021_data)[7:48],'home', sep = '_')
away_2021_data <- away_2021_data %>%
  select(-c(match_home_team_goals, match_home_team_behinds, match_home_team_score, match_margin, home_away))
colnames(away_2021_data)[2] <- 'away_team'
colnames(away_2021_data)[6:47] <- paste(colnames(away_2021_data)[6:47], 'away', sep = '_')

#recombine dfs
data_2021_combined <- left_join(home_2021_data, away_2021_data, by = 'match_id')

#add total score column
data_2021_combined$match_score <- data_2021_combined$match_home_team_score + data_2021_combined$match_away_team_score

#remove other columns related to score
data_2021_no_scores <- data_2021_combined %>%
  select(-c(match_home_team_goals, match_home_team_behinds, match_home_team_score, match_away_team_goals, 
            match_away_team_behinds, match_away_team_score, match_margin, home_team, away_team))


#combine weather variables
#data_2021_no_scores$temperature <- lookup(data_2021_no_scores$match_id, fry_stats_2021$match_id, fry_stats_2021$match_weather_temp_c)
#data_2021_no_scores$weather <- lookup(data_2021_no_scores$match_id, fry_stats_2021$match_id, fry_stats_2021$match_weather_type)

#lm fit
lm_pts <- lm(match_score~., data = data_2021_no_scores)
fwd <- step(lm_pts, direction = 'forward', steps = 1000, trace = 1)


#data to run RF
library(randomForest)
library(caret)
rf_points <- randomForest(y = data_2021_no_scores$match_score, x = data_2021_no_scores[c(2:85)], ntree = 1000)
importance_rf <- rf_points$importance
varImpPlot(rf_points)
corr_data <- cor(data_2021_no_scores[c(2:85)])


#schedules 2022/2023
sched_2022 <- get_fixture(season = 2022)
sched_2022$Date <- as.Date(sched_2022$Date)
sched_2023 <- get_fixture(season = 2023)
sched_2023$Date <- as.Date(sched_2023$Date)


#age curve
years <- c(2014:2023)

all_players_curve <- data.frame()
for (i in 1:length(years)){
  x <- fetch_player_stats(season = years[i])
  all_players_curve <- bind_rows(all_players_curve, x)
}

all_players_age <- fetch_player_details(current = FALSE)
all_players_age$start_of_season <- as.Date(paste0(all_players_age$season,'-03-01'))
all_players_age$dateOfBirth <- as.Date(all_players_age$dateOfBirth)
all_players_age$age <- as.numeric(round(((all_players_age$start_of_season - all_players_age$dateOfBirth) / 365.25),0))

all_players_curve$Date <- as.Date(all_players_curve$utcStartTime)
all_players_curve$season <- lubridate::year(all_players_curve$Date)
colnames(all_players_curve)[18] <- 'firstName'
colnames(all_players_curve)[19] <- 'surname'
colnames(all_players_curve)[96] <- 'team'

all_players_curve_age <- left_join(all_players_curve, all_players_age, by = c('firstName', 'surname', 'team', 'season')) %>%
  select(Date, firstName, surname, team, season, player.player.position, age, ratingPoints, clearances.centreClearances)

by_position_curve <- all_players_curve_age %>%
  group_by(player.player.position, age) %>%
  summarise(mean_rating_pts = mean(ratingPoints),
            median_rating_pts = median(ratingPoints),
            mean_centre_clearances = mean(clearances.centreClearances))
by_position_curve <- by_position_curve[complete.cases(by_position_curve),]


#backtest model
#check for players changing teams
players_by_team <- fetch_player_details(current = FALSE)
players_by_team$start_date <- as.Date('2021-03-01', format = '%Y-%m-%d')
players_by_team$dateOfBirth <- as.Date(players_by_team$dateOfBirth, format = '%Y-%m-%d')
players_by_team$age <- as.numeric(players_by_team$start_date - players_by_team$dateOfBirth) / 365.25
players_2021 <- players_by_team %>% filter(season == 2021)
players_2022 <- players_by_team %>% filter(season == 2022)

#players who changed teams
player_changing_teams <- left_join(current_players_by_team, last_year_players_by_team, by = 'id') %>%
  dplyr :: select(firstName.x, surname.x, id, current_team, last_team, position.x)
colnames(player_changing_teams) <- c('firstName', 'surname', 'id', 'current_team', 'last_team', 'position')
player_changing_teams <- player_changing_teams %>% filter(current_team != last_team)

#new players (i.e. players not on a roster from last year. Consider league averages for their stats)
new_players <- current_players_by_team %>% filter(!current_players_by_team$id %in% last_year_players_by_team$id)



#home grounds data
home_ground <- stats_2022 %>% select(home.team.name, venue.name)
home_ground <- data.frame(distinct(home_ground))


#################################################################################################################
#get referee data
ref_data <- get_afltables_stats(start_date = '2014-03-20', end_date = '2022-12-31')
ref_data <- ref_data %>% 
  select(c(Date, Home.team, Away.team, Umpire.1, Umpire.2, Umpire.3))
ref_data <- distinct(ref_data)
umpires <- data.frame(unique(ref_data$Umpire.1))
colnames(umpires) <- 'Umpire.1'

#combine ref data
ref_data_odds <- left_join(ref_data, afl_historical_odds, by = c('Date', 'Home.team', 'Away.team')) %>%
  select(c(Date, Home.team, Away.team, Umpire.1, Total.Points, Total.Score.Open, Total.Score.Min, Total.Score.Max, Total.Score.Close))
ref_data_odds <- ref_data_odds[complete.cases(ref_data),]
pts_by_ref <- ref_data_odds %>% group_by(Umpire.1) %>% 
  summarise(mean.pts = mean(Total.Points), 
            median.pts = median(Total.Points),
            total.games = n())
pts_by_ref <- pts_by_ref %>% filter(total.games > 10)
##################################################################################################################
