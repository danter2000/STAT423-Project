library(tidyverse)
library(tidyverse)
library(Lahman)

load("../data/kobe.RData")

awards <- read.csv("../data/AwardsPlayers.csv")

hof_players <- hall_df %>% 
  filter(category == "Player") %>% 
  group_by(playerID) %>%
  summarize(wein = "Y" %in% inducted * 1)

awards_wider <- awards %>% 
  group_by(playerID, awardID) %>% 
  summarize(n = n()) %>%
  pivot_wider(id_cols = playerID, names_from = awardID, values_from = n) %>% 
  mutate(across(1:30, ~ replace_na(.x, 0)))

write.csv(hof_players, "../data/hof_players.csv")
write.csv(awards_wider, "../data/awards_wider.csv")

save.image("../data/kobe.RData")
