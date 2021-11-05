library(tidyverse)
library(readxl)

drake <- read_excel("Data/Drake.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Drake", .after = Date)
illinois_state <- read_excel("Data/Illinois State.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Illinois State", .after = Date)
indiana_state <- read_excel("Data/Indiana State.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Indiana State", .after = Date)
loyola <- read_excel("Data/Loyola.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Loyola", .after = Date)
ue <- read_excel("Data/Evansville.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "UE", .after = Date)
siu <- read_excel("Data/SIU.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "SIU", .after = Date)
uni <- read_excel("Data/UNI.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "UNI", .after = Date)
valpo <- read_excel("Data/Valpo.xlsx", na = "-") %>% 
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Valpo", .after = Date)
miss <- read_excel("Data/Miss St.xlsx", na = "-") %>%
  filter(row_number() <= n()-1) %>%
  mutate(Team = "Miss St", .after = Date)


league <- bind_rows(drake, illinois_state, indiana_state, loyola, ue, siu, uni, valpo)
league[is.na(league)] = 0