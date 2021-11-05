form_mean_player <- function(playing_for, section) {
  name <- c(section$`Player Name`, NA)
  position <- c(section$Position, NA)
  team <- c(section$Team, "League")
  
  section <- select(section, -`Player Name`, -Position, -Team)
  
  section <- bind_rows(section, summarize_all(section, mean)) %>%
    mutate(Name = name, .after = `Player Number`) %>%
    mutate(Position = position, .after = Name) %>%
    mutate(Team = team, .before = `Player Number`)
  
  tmp <- filter(section, Team == playing_for)
  league_section <- filter(section, Team == "League")
  tmp <- bind_rows(tmp, league_section)
  tmp
}

forward_form <- function(section) {
  select(section, "Name", "Player Number", "InStat Index", "Matches played", "Minutes played", "Starting lineup appearances",
         "Substitutes in", "Goals",  "Shots", "Shots on target", "xG (Expected goals)", "xG conversion", "Chances", "Assists",
         "Expected assists", "Accurate passes", "Key passes", "Key passes accurate", "Ball recoveries in opponent's half",
         "Dribbles", "Attacking challenges", "Attacking challenges won", "Ball interceptions", "Fouls suffered")
}

winger_form <- function(section) {
  select(section, "Name", "Player Number", "InStat Index", "Matches played", "Minutes played", "Starting lineup appearances",
         "Substitutes in", "Goals",  "Shots", "Shots on target", "xG (Expected goals)", "xG conversion", "Chances", "Assists", 
         "Crosses", "Crosses accurate", "Expected assists", "Accurate passes", "Key passes", "Key passes accurate",
         "Ball recoveries in opponent's half", "Dribbles", "Attacking challenges", "Attacking challenges won", "Ball interceptions",
         "Fouls suffered")
}

midfielder_form <- function(section) {
  select(section, "Name", "Player Number", "InStat Index", "Matches played", "Minutes played", "Starting lineup appearances",
         "Substitutes in", "Goals",  "Shots", "xG (Expected goals)", "Chances", "Assists", "Key passes", "Key passes accurate",
         "Expected assists", "Accurate passes", "Lost balls", "Lost balls in own half", "Attacking challenges", "Attacking challenges won", "Tackles", "Tackles successful",
         "Defensive challenges", "Defensive challenges won", "Air challenges", "Air challenges won", "Ball interceptions", 
         "Ball recoveries in opponent's half", "Fouls suffered")
}

fullback_form <- function(section) {
  select(section, "Name", "Player Number", "InStat Index", "Matches played", "Minutes played", "Starting lineup appearances",
         "Substitutes in", "Assists", "Key passes", "Key passes accurate",
         "Accurate passes", "Lost balls", "Lost balls in own half", "Tackles", "Tackles successful",
         "Defensive challenges", "Defensive challenges won", "Air challenges", "Air challenges won", "Ball interceptions", 
         "Ball recoveries in opponent's half", "Fouls suffered")
}

defender_form <- function(section) {
  select(section, "Name", "Player Number", "InStat Index", "Matches played", "Minutes played", "Starting lineup appearances",
         "Substitutes in",
         "Accurate passes", "Lost balls", "Lost balls in own half", "Tackles", "Tackles successful",
         "Defensive challenges", "Defensive challenges won", "Air challenges", "Air challenges won", "Ball interceptions", 
         "Ball recoveries in opponent's half", "Fouls suffered")
}






