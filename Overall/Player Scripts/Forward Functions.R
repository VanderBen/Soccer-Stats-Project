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

forward_form <- function(team, section) {
  tmp2 <- filter(section, Team == team)
  tmp2
}