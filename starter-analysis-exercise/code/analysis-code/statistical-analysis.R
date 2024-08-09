###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(cfbfastR)
library(caret)

#load data. 
results22 <- cfbd_game_info(2022) #gets data from 2022 season

# gets weekly stats for 2022 season
games22 <- cfbfastR::cfbd_game_team_stats(2022, week = 1)
games22$week <- 1
for (i in 2:15) {
  gamestemp <- cfbfastR::cfbd_game_team_stats(2022, week = i)
  gamestemp$week <- i
  games22 <- rbind(games22, gamestemp)
}

# Converts data to numeric
games22$total_yards <- as.numeric(games22$total_yards)
games22$sacks <- as.numeric(games22$sacks)
games22$turnovers <- as.numeric(games22$turnovers)
games22$rushing_attempts <- as.numeric(games22$rushing_attempts)
games22$rushing_yards <- as.numeric(games22$rushing_yards)
games22$sacks <- as.numeric(games22$sacks)
games22$tackles_for_loss <- as.numeric(games22$tackles_for_loss)

# Transforms data into differences
diff_data <-data.frame(matrix(ncol = 11, nrow = 0))
score_data_cols <- c("year", "week", "hTeam", "aTeam","ptDif","yrdDif", "toDif", 
                     "rustAtpDif", "rushYrdDif", "sackDif", "tflDif")
colnames(diff_data) <- score_data_cols

for (i in seq(from=1, to=nrow(games22), by=2)) {
  # Get the home team and points
  hTeam <- games22 %>% filter(game_id==games22$game_id[i] & home_away=='home') %>% 
    select(school, points)
  # Get the home team's average stats for the weeks before
  hTeamStats <- games22 %>% filter(school == hTeam$school & week < games22[i]$week) %>% 
    group_by(school) %>% summarise(total_yards = mean(total_yards), 
                                   turnovers = mean(turnovers), 
                                   rushing_attempts = mean(rushing_attempts), 
                                   rushing_yards = mean(rushing_yards),
                                   sacks = mean(sacks),
                                   tackles_for_loss = mean(tackles_for_loss))
  # Get the away team and points
  aTeam <- games22 %>% filter(game_id==games22$game_id[i] & home_away=='away') %>% select(school, points)
  # Get the away team's average stats for the weeks before
  aTeamStats <- games22 %>% filter(school == aTeam$school & week < games22[i]$week) %>% 
    group_by(school) %>% summarise(total_yards = mean(total_yards), 
                                   turnovers = mean(turnovers), 
                                   rushing_attempts = mean(rushing_attempts), 
                                   rushing_yards = mean(rushing_yards),
                                   sacks = mean(sacks),
                                   tackles_for_loss = mean(tackles_for_loss))
  # Add data points to df
  tempdata <- data.frame(2022, games22$week[i], hTeam$school[1], aTeam$school[1], 
                         hTeam$points[1]-aTeam$points[1], 
                         hTeamStats$total_yards[1]-aTeamStats$total_yards[1],
                         hTeamStats$turnovers[1]-aTeamStats$turnovers[1],
                         hTeamStats$rushing_attempts[1]-aTeamStats$rushing_attempts[1],
                         hTeamStats$rushing_yards[1]-aTeamStats$rushing_yards[1],
                         hTeamStats$sacks[1]-aTeamStats$sacks[1],
                         hTeamStats$tackles_for_loss[1]-aTeamStats$tackles_for_loss[1])
  colnames(tempdata) <- score_data_cols
  diff_data <- rbind(diff_data, tempdata)
}

# Remove NAs from data (first week of season)
diff_data <- na.omit(diff_data)

# Data exploration plots
plot(games22$points, games22$total_yards)
plot(games22$points, games22$turnovers)
plot(games22$points, games22$rushing_attempts)
plot(games22$points, games22$rushing_yards)
plot(games22$rushing_attempts,games22$rushing_yards)
plot(games22$points, games22$sacks)
plot(games22$points, games22$tackles_for_loss)

######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using points as outcome, yards and sacks as predictor
lmfit1 <- lm(ptDif ~ yrdDif + sackDif, data = diff_data)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("starter-analysis-exercise","results", "tables-files", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

# fit logistic model
logitfit <- train(x= diff_data[,(6:11)] , y=as.factor(diff_data$ptDif > 0), 
                  method = 'glmnet',
                  family = 'binomial' )


### PREPARE TEST DATA ###


#Prepare test data from 2023 season
results23 <- cfbd_game_info(2023)

# gets weekly stats for 2023 season
games23 <- cfbfastR::cfbd_game_team_stats(2023, week = 1)
games23$week <- 1
for (i in 2:15) {
  gamestemp <- cfbfastR::cfbd_game_team_stats(2023, week = i)
  gamestemp$week <- i
  games23 <- rbind(games23, gamestemp)
}

# Converts data to numeric
games23$total_yards <- as.numeric(games23$total_yards)
games23$sacks <- as.numeric(games23$sacks)
games23$turnovers <- as.numeric(games23$turnovers)
games23$rushing_attempts <- as.numeric(games23$rushing_attempts)
games23$rushing_yards <- as.numeric(games23$rushing_yards)
games23$sacks <- as.numeric(games23$sacks)
games23$tackles_for_loss <- as.numeric(games23$tackles_for_loss)

# Transforms data into differences
diff_dataTest <-data.frame(matrix(ncol = 11, nrow = 0))
score_data_cols <- c("year", "week", "hTeam", "aTeam","ptDif","yrdDif", "toDif", 
                     "rustAtpDif", "rushYrdDif", "sackDif", "tflDif")
colnames(diff_dataTest) <- score_data_cols

for (i in seq(from=1, to=nrow(games23), by=2)) {
  # Get the home team and points
  hTeam <- games23 %>% filter(game_id==games23$game_id[i] & home_away=='home') %>% 
    select(school, points)
  # Get the home team's average stats for the weeks before
  hTeamStats <- games23 %>% filter(school == hTeam$school & week < games23[i]$week) %>% 
    group_by(school) %>% summarise(total_yards = mean(total_yards), 
                                   turnovers = mean(turnovers), 
                                   rushing_attempts = mean(rushing_attempts), 
                                   rushing_yards = mean(rushing_yards),
                                   sacks = mean(sacks),
                                   tackles_for_loss = mean(tackles_for_loss))
  # Get the away team and points
  aTeam <- games23 %>% filter(game_id==games23$game_id[i] & home_away=='away') %>% select(school, points)
  # Get the away team's average stats for the weeks before
  aTeamStats <- games23 %>% filter(school == aTeam$school & week < games23[i]$week) %>% 
    group_by(school) %>% summarise(total_yards = mean(total_yards), 
                                   turnovers = mean(turnovers), 
                                   rushing_attempts = mean(rushing_attempts), 
                                   rushing_yards = mean(rushing_yards),
                                   sacks = mean(sacks),
                                   tackles_for_loss = mean(tackles_for_loss))
  # Add data points to df
  tempdata <- data.frame(2023, games23$week[i], hTeam$school[1], aTeam$school[1], 
                         hTeam$points[1]-aTeam$points[1], 
                         hTeamStats$total_yards[1]-aTeamStats$total_yards[1],
                         hTeamStats$turnovers[1]-aTeamStats$turnovers[1],
                         hTeamStats$rushing_attempts[1]-aTeamStats$rushing_attempts[1],
                         hTeamStats$rushing_yards[1]-aTeamStats$rushing_yards[1],
                         hTeamStats$sacks[1]-aTeamStats$sacks[1],
                         hTeamStats$tackles_for_loss[1]-aTeamStats$tackles_for_loss[1])
  colnames(tempdata) <- score_data_cols
  diff_dataTest <- rbind(diff_dataTest, tempdata)
}

# Remove NAs from data (first week of season)
diff_dataTest <- na.omit(diff_dataTest)

# Predictions from linear model
lmPred <- predict(lmfit1, diff_dataTest)
# Prediction from logisitic model
logitPred <- predict(logitfit, diff_dataTest[,(6:11)])

lmDF <- data.frame(response = diff_dataTest$ptDif > 0, predicted = lmPred > 0)
xtabs(~ predicted + response, data = lmDF)
table_file2 = here("starter-analysis-exercise","results", "tables-files", "resulttable2.rds")
saveRDS(xtabs(~ predicted + response, data = lmDF), file = table_file2)

logitDF <- data.frame(response = as.factor(diff_dataTest$ptDif > 0), predicted = logitPred)
xtabs(~ predicted + response, data = logitDF)
table_file3 = here("starter-analysis-exercise","results", "tables-files", "resulttable3.rds")
saveRDS(xtabs(~ predicted + response, data = logitDF), file = table_file3)