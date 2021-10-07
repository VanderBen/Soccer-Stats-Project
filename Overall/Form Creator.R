library(tidyverse)
library(readxl)

form_mean <- function(col, tmp) {
  vec <- pull(tmp, col)
  vec2 <- pull(league, col)
  df <- tibble(Stats = col, 
               `Last 3` = mean(vec[1:3], na.rm = TRUE), 
               `Last 5` = mean(vec[1:5], na.rm = TRUE), 
               Season = mean(vec, na.rm = TRUE),
               League = mean(vec2, na.rm = TRUE),
               `Last 3 Difference` = `Last 3` - League,
               `Last 5 Difference` = `Last 5` - League,
               `Season Difference` = Season - League)
  df
}


attack_creator <- function(team) {
  df <- form_mean("Goals", team)
  df2 <- form_mean("Shots", team)
  df3 <- form_mean("Shots on target", team)
  df4 <- form_mean("xG (Expected goals)", team)
  df5 <- form_mean("xG conversion", team)
  df6 <- form_mean("xG per shot", team)
  df7 <- form_mean("xG per goal", team)
  df8 <- form_mean("Corners", team)
  df9 <- form_mean("Net xG (xG - Opponent's xG)", team)
  df10 <- form_mean("Expected points", team)
  
  attack_mean_table <- bind_rows(df,df2,df3,df4,df5,df6,df7,df8,df9,df10)
  attack_mean_table
}

buildup_creator <- function(team) {
  df <- form_mean("Passes", team)
  df2 <- form_mean("Accurate passes", team)
  df3 <- form_mean("Chances", team)
  df4 <- form_mean("Chances successful", team)
  df6 <- form_mean("Key passes", team)
  df7 <- form_mean("Key passes accurate", team)
  df8 <- form_mean("Crosses", team)
  df9 <- form_mean("Crosses accurate", team)
  df10 <- form_mean("Dribbles", team)
  df11 <- form_mean("Dribbles successful", team)
  df12 <- form_mean("Lost balls", team)
  df13 <- form_mean("Lost balls in own half", team)
  df14 <- form_mean("Entrances on opponent's half", team)
  df15 <- form_mean("Entrances on final third of opponent's half", team)
  df16 <- form_mean("Entrances to the opponent's box", team)
  
  buildup_mean_table <- bind_rows(df,df2,df3,df4,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16)
}

type_creator <- function(team) {
  df <- form_mean("Attacks - left flank", team)
  df2 <- form_mean("Attacks with shots - left flank", team)
  df3 <- form_mean("Attacks - center", team)
  df4 <- form_mean("Attacks with shots - center", team)
  df5 <- form_mean("Attacks - right flank", team)
  df6 <- form_mean("Attacks with shots - right flank", team)
  df7 <- form_mean("Positional attacks", team)
  df8 <- form_mean("Positional attacks with shots", team)
  df9 <- form_mean("Counter-attacks", team)
  df10 <- form_mean("Counter-attacks with a shot", team)
  df11 <- form_mean("Set pieces attacks", team)
  df12 <- form_mean("Attacks with shots - Set pieces attacks", team)
  df13 <- form_mean("Free-kick attacks", team)
  df14 <- form_mean("Free-kick attacks with shots", team)
  df15 <- form_mean("Corner attacks", team)
  df16 <- form_mean("Corner attacks with shots", team)
  
  type_mean_table <-
    bind_rows(df,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16)
  type_mean_table
}

defense_creator <- function(team) {
  df <- form_mean("Challenges", team)
  df2 <- form_mean("Challenges won", team)
  df3 <- form_mean("Attacking challenges", team)
  df4 <- form_mean("Attacking challenges won", team)
  df5 <- form_mean("Defensive challenges", team)
  df6 <- form_mean("Defensive challenges won", team)
  df7 <- form_mean("Air challenges", team)
  df8 <- form_mean("Air challenges won", team)
  df9 <- form_mean("Tackles", team)
  df10 <- form_mean("Tackles successful", team)
  df11 <- form_mean("Fouls", team)
  df12 <- form_mean("Ball recoveries", team)
  df13 <- form_mean("Ball recoveries in opponent's half", team)
  df14 <- form_mean("Ball interceptions", team)
  df15 <- form_mean("Free ball pick ups", team)
  df16 <- form_mean("Team pressing", team)
  df17 <- form_mean("Opponent's xG", team)
  df18 <- form_mean("Opponent's xG per shot", team)
  
  defense_mean_table <- 
    bind_rows(df,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,
              df18)
  defense_mean_table
}
