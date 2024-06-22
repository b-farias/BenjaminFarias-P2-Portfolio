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

games22$total_yards <- as.numeric(games22$total_yards)
games22$sacks <- as.numeric(games22$sacks)

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
# fit linear model using points as outcome, total_yards and sacks as predictor

lmfit1 <- lm(games22$points ~ games22$total_yards + games22$sacks)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("starter-analysis-exercise","results", "tables-files", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

