library(fitzRoy)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plyr)
library(stringr)
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

#pull game statistics for a decade
fry_stats <- get_fryzigg_stats(start = 2013, end = 2022)
fry_stats <- fry_stats %>% mutate(season = as.POSIXct(date, format = '%Y-%m-%d'))
fry_stats$season <- format(fry_stats$season, format = '%Y')
fry_stats$season <- as.integer(fry_stats$season)
x <- fry_stats %>% summarise_all(~ sum(is.na(.)))
fry_stats <- fry_stats %>% select(-c(player_height_cm, player_weight_kg, player_is_retired, 
                                     supercoach_score, afl_fantasy_score, subbed))
x <- fry_stats %>% summarise_all(~ sum(is.na(.)))
fry_stats <- fry_stats[complete.cases(fry_stats),]
colnames(fry_stats)[20] <- 'firstName'
colnames(fry_stats)[21] <- 'surname'
colnames(fry_stats)[23] <- 'jumperNumber'

#pull player information 
player_details <- fetch_player_details(current = FALSE)

#combine data with player birthdate
fry_stats <- left_join(fry_stats, player_details, by = c('firstName', 'surname'))
fry_stats <- fry_stats %>% select(-c(84:91))
fry_stats <- fry_stats %>% select(-c(season.y, data_accessed, providerId))
colnames(fry_stats)[76] <- 'season'

#kick effectiveness by year
kick_eff <- fry_stats %>% 
  dplyr :: group_by(season) %>%
  dplyr :: summarise(total_shots = sum(shots_at_goal),
            total_goals = sum(goals),
            total_behinds = sum(behinds),
            goal_eff = sum(goals) / sum(shots_at_goal),
            kick_eff = (sum(goals) + sum(behinds)) / sum(shots_at_goal)
         )

p <- ggplot(data = kick_eff, aes(season)) + geom_point(aes(y = goal_eff, color = 'goal_eff')) + 
  geom_point(aes(y = kick_eff, color = 'kick_eff')) + labs(x = 'Percentages', y = 'Season', color = 'Legend')
p

#group player stats by year
player_by_year <- fry_stats %>% 
  dplyr :: group_by(player_id, season) %>%
  dplyr :: summarise (mean_goals = mean(goals),
                      variance_goals = var(goals),
                      mean_shots = mean(shots_at_goal),
                      variance_shots = var(shots_at_goal),
                      mean_marks = mean(marks),
                      variance_marks = var(marks),
                      mean_behinds = mean(behinds),
                      variance_behinds = var(behinds),
                      mean_centre_clearances = mean(centre_clearances),
                      variance_centre_clearances = var(centre_clearances),
                      mean_time_on_ground = mean(time_on_ground_percentage),
                      variance_time_on_ground = var(time_on_ground_percentage),
                      mean_metres_gained = mean(metres_gained),
                      variance_metres_gained = var(metres_gained),
                      )
#add date of birth 
player_by_year$birth_date <- lookup(player_by_year$player_id, fry_stats$player_id, fry_stats$dateOfBirth) 


#add positions
player_by_year$player_position <- lookup(player_by_year$player_id, fry_stats$player_id, fry_stats$player_position)

#20 min quarters
#16 min quarters in 2020
player_by_year$avg_min <- NA

for(i in 1:nrow(player_by_year)){
  player_by_year$avg_min[i] <- if(player_by_year$season[i] != 2020){
  (player_by_year$mean_time_on_ground[i]/100) * 80
  } else {
    (player_by_year$mean_time_on_ground[i]/100) * 64}
}

#league averages 
lg_avg_by_year <- fry_stats %>% 
  dplyr :: group_by(season, player_position) %>%
  dplyr :: summarise (mean_goals = mean(goals),
                      mean_shots = mean(shots_at_goal),
                      mean_marks = mean(marks),
                      mean_behinds = mean(behinds),
                      mean_centre_clearances = mean(centre_clearances),
                      mean_metres_gained = mean(metres_gained),
  )

#marcel projections by year
#weight data by 3 years
years <- c(2013:2015)

year_weighting <- function(df, yrs, ID){
  df <- df %>% filter(player_id == ID) %>% filter(season >=first(yrs) & season <= last(yrs))
  if(nrow(df) == 3){weights = c(3,4,5)}
  if(nrow(df) == 2){weights = c(4,6)}
  if(nrow(df) == 1){weights = c(1)}
  
  df1 <- df %>% select(c(player_id, season, player_position, birth_date))
  df1 <- df1[1,]
  df_avg_min <- df[i,19]
  df1[1,2] <- years[3] + 1
  df2 <- df %>% select(c(1,3,5,7,9,11,13,15,19))
  df2[,2:9] <- df2[,2:9] * (weights/sum(weights))
  df3 <- as.data.frame(t(colSums(df2[,2:9])))
  df4 <- df %>% select(c(1,4,6,8,10,12,14,16,18))
  df5 <- as.data.frame(t(colSums(df4[,2:8])))
  df5 <- cbind(df1,df3,df5)
  next_season <- years[3] + 1
  df5$birth_date <- as.Date(df5$birth_date)
  age <- lubridate :: ymd(next_season, truncated = 2L) - df5$birth_date
  age <- as.numeric(age) / 365.25
  age_adj <- if (age < 29) {1+ ((29-age) * 0.003)} else {(age - 29) * 0.006}
  df5[5:11] <- df5[5:11] * age_adj
  return(df5)
  }

#create projections for a season
years <- c(2013:2015)

stats_year <- data.frame()

player_by_year_bday <- player_by_year %>% filter(is.na(birth_date) == FALSE)

for(i in 1:nrow(player_by_year_bday)){
 player_filter <- player_by_year_bday %>% filter(season >= first(years) & season <= last(years))
 id <- player_filter$player_id[i]
 player_wt <- year_weighting(player_filter, years, id)
 stats_year <- rbind(stats_year, player_wt)
 print(i)
}

stats_year <- distinct(stats_year)

IDs <- fry_stats %>% 
  filter(season == 2016) %>% 
  select(c(season, player_id, team, firstName, surname))
IDs <- distinct(IDs)

projections <- left_join(IDs, stats_year, by = c('player_id', 'season'))


#pull schedule week 1 for year
schedule <- fetch_fixture_afl(season = 2016, round_number = 1)

home_team <- projections %>% filter(team == 'Richmond')
away_team <- projections %>% filter(team == 'Carlton')

#select starters
positions <- data.frame(unique(projections$player_position)) 
colnames(positions) <- 'positions'

#positions home team
home_back <- home_team %>% filter(player_position == 'BPL' | player_position == 'BPR' | player_position == 'FB')
home_HB <- home_team %>% filter(player_position == 'HBFL' | player_position == 'CHB' | player_position == 'HBFR')
home_C <- home_team %>% filter(player_position == 'C' | player_position == 'WL' | player_position == 'WR')
home_HF <- home_team %>% filter(player_position == 'HFFL' | player_position == 'HFFR' | player_position == 'CHF')
home_F <- home_team %>% filter(player_position == 'FPL' | player_position == 'FPR' | player_position == 'FF')
home_R <- home_team %>% filter(player_position == 'R' | player_position == 'RK' | player_position == 'RR')
home_INT <- home_team %>% filter(player_position == 'INT')
home_SUB <- home_team %>% filter(player_position == 'SUB')


if(nrow(home_back) > 3) {
  home_back_starter <- home_back[order(home_back$mean_time_on_ground),]
  home_back_starter <- home_back_starter[1:3,]
  } else {
    home_back_starter <- home_back
  }

if(nrow(home_HB) > 3) {
  home_HB_starter <- home_HB[order(home_HB$mean_time_on_ground),]
  home_HB_starter <- home_HB_starter[1:3,]
  } else {
    home_HB_starter <- home_HB
  }

if(nrow(home_C) > 3) {
  home_C_starter <- home_C[order(home_C$mean_time_on_ground),]
  home_C_starter <- home_C_starter[1:3,]
  } else {
  home_C_starter <- home_C
}

if(nrow(home_HF) > 3) {
  home_HF_starter <- home_HF[order(home_HF$mean_time_on_ground),]
  home_HF_starter <- home_HF_starter[1:3,]
} else {
    home_HF_starter <- home_HF
  }

if(nrow(home_F) > 3) {
  home_F_starter <- home_F[order(home_F$mean_time_on_ground),]
  home_F_starter <- home_F_starter[1:3,]
  } else {
  home_F_starter <- home_F  
  }

if(nrow(home_R) > 3) {
  home_R_starter <- home_R[order(home_F$mean_time_on_ground),]
  home_R_starter <- home_R_starter[1:3,]
} else {
    home_R_starter <- home_R
}

if(nrow(home_INT) > 4) {
  home_INT_bench <- home_INT[order(home_INT$mean_time_on_ground),]
  home_INT_bench <- home_INT_bench[1:4,]
} else {
  home_INT_bench <- home_INT
}

if(nrow(home_SUB) > 1) {
  home_SUB_bench <- home_SUB[order(home_SUB$mean_time_on_ground),]
  home_SUB_bench <- home_SUB_bench[1,]
} else {
  home_SUB_bench <- home_SUB
}


home_starters <- rbind(home_back_starter, home_C_starter, home_F_starter, home_HB_starter, 
                       home_HF_starter, home_R_starter, home_INT_bench, home_SUB_bench)


#positions away team
away_back <- away_team %>% filter(player_position == 'BPL' | player_position == 'BPR' | player_position == 'FB')
away_HB <- away_team %>% filter(player_position == 'HBFL' | player_position == 'CHB' | player_position == 'HBFR')
away_C <- away_team %>% filter(player_position == 'C' | player_position == 'WL' | player_position == 'WR')
away_HF <- away_team %>% filter(player_position == 'HFFL' | player_position == 'HFFR' | player_position == 'CHF')
away_F <- away_team %>% filter(player_position == 'FPL' | player_position == 'FPR' | player_position == 'FF')
away_R <- away_team %>% filter(player_position == 'R' | player_position == 'RK' | player_position == 'RR')
away_INT <- away_team %>% filter(player_position == 'INT')
away_SUB <- away_team %>% filter(player_position == 'SUB')

if(nrow(away_back) > 3) {
  away_back_starter <- away_back[order(away_back$mean_time_on_ground),]
  away_back_starter <- away_back_starter[1:3,]
} else {
  away_back_starter <- away_back
}

if(nrow(away_HB) > 3) {
  away_HB_starter <- away_HB[order(away_HB$mean_time_on_ground),]
  away_HB_starter <- away_HB_starter[1:3,]
} else {
  away_HB_starter <- away_HB
}

if(nrow(away_C) > 3) {
  away_C_starter <- away_C[order(away_C$mean_time_on_ground),]
  away_C_starter <- away_C_starter[1:3,]
} else {
  away_C_starter <- away_C
}

if(nrow(away_HF) > 3) {
  away_HF_starter <- away_HF[order(away_HF$mean_time_on_ground),]
  away_HF_starter <- away_HF_starter[1:3,]
} else {
  home_HF_starter <- away_HF
}

if(nrow(away_F) > 3) {
  away_F_starter <- away_F[order(away_F$mean_time_on_ground),]
  away_F_starter <- away_F_starter[1:3,]
} else {
  away_F_starter <- away_F  
}

if(nrow(away_R) > 3) {
  away_R_starter <- away_R[order(away_R$mean_time_on_ground),]
  away_R_starter <- away_R_starter[1:3,]
} else {
  away_R_starter <- away_R
}

if(nrow(away_INT) > 4) {
  away_INT_bench <- away_INT[order(away_INT$mean_time_on_ground),]
  away_INT_bench <- away_INT_bench[1:4,]
} else {
  away_INT_bench <- away_INT
}

if(nrow(away_SUB) > 1) {
  away_SUB_bench <- away_SUB[order(away_SUB$mean_time_on_ground),]
  away_SUB_bench <- away_SUB_bench[1,]
} else {
  away_SUB_bench <- away_SUB
}


away_starters <- rbind(away_back_starter, away_C_starter, away_F_starter, away_HB_starter, 
                       away_HF_starter, away_R_starter, away_INT_bench, away_SUB_bench)

##create home / away stats
