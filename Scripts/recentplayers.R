library(rvest)

# Players to model on -- if they retired in the last 5 years
recent_players <- player_df %>% 
  filter(finalGame >= Sys.Date() - (365 * 5) & playerID %in% c((pitching_df %>% group_by(playerID) %>% filter(length(unique(yearID)) >= 10) %>% pull(playerID) %>% unique()), (batting_df %>% group_by(playerID) %>% filter(length(unique(yearID)) >= 10) %>% pull(playerID) %>% unique())) & !(playerID %in% (hall_df %>% pull(playerID) %>% unique())))

# Check if they are currently active or not based on if they are on a team
# Around 28% of the players in this list are still active -- based on baseball reference

recent_players_info <- data.frame(matrix(ncol = 4))

colnames(recent_players_info) <- c("player", "active", "current_team", "info")

url <- "https://www.baseball-reference.com/players/%s.shtml"

for (player in recent_players$playerID) {
  result = tryCatch({
    str_player <- paste0(str_sub(player, 1, 1), "/", player)
    
    html <- read_html(sprintf(url, str_player))
    
    status <- html %>%
      html_node("#meta") %>%
      html_node("div:nth-child(2)") %>%
      html_nodes("p:nth-child(5)")
  }, warning = function(warning_condition) {
    print('warning')
  }, error = function(error_condition) {
    recent_players_info <- rbind(recent_players_info, c(player, F, NA, "Player not found."))
  }, finally={
    status_text <- str_replace_all(html_text(status), "\\n", "")
    
    if (str_detect(html_text(status), "Born")) {
      recent_players_info <- rbind(recent_players_info, c(player, F, NA, status_text))
    } else {
      recent_players_info <- rbind(recent_players_info, c(player, T, html_text(html_node(status, "a")), status_text))
    }
  })
  
  Sys.sleep(5)
}

recent_players <- recent_players %>%
  left_join(recent_players_info , by = c("playerID" = "player")) %>%
  mutate(active = active == "TRUE", finalgame_before_2022 = finalGame <= "2022-04-07")

recent_players %>%
  select(active, finalgame_before_2022) %>%
  summarize_all(mean)

write.csv(recent_players, "recent_players.csv")