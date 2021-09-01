library(tidyverse)
library(StatsBombR)
Comp <- FreeCompetitions() %>%
  filter(competition_id==11 & season_name=="2005/2006")
Matches <- FreeMatches(Comp)
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData)


shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))


shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm =
                          TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm =
                          TRUE)/n_distinct(match_id))

head(StatsBombData)
