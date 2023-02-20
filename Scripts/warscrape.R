library(rvest)

war_results <- list()

url <- "https://www.baseball-reference.com/players/%s.shtml"

for (player in PitcherHOF_df$playerID) {
  result = tryCatch({
    str_player <- paste0(str_sub(player, 1, 1), "/", player)

    html <- read_html(sprintf(url, str_player))

    war <- html %>%
      html_nodes("#all_pitching_value") %>%
      html_nodes(xpath = 'comment()') %>%
      html_text() %>%
      read_html() %>%
      html_nodes(css = 'th[data-stat="player_stats_summary_explain"] ~ td[data-stat="WAR_pitch"]') %>%
      .[[1]] %>%
      html_text()

  }, warning = function(warning_condition) {
      print('warning')
  }, error = function(error_condition) {
      recent_players_info <- rbind(recent_players_info, c(player, F, NA, "Player not found."))
  }, finally={
    war_results[player] <- as.numeric(war)
  })

  Sys.sleep(2)
}

war_results <- t(data.frame(war_results))
colnames(war_results) <- c("WAR")
write.csv(war_results, "pitcherWAR.csv")

war_results <- list()

url <- "https://www.baseball-reference.com/players/%s.shtml"

for (player in PositionPlayerHOF_df$playerID) {
  result = tryCatch({
    str_player <- paste0(str_sub(player, 1, 1), "/", player)

    html <- read_html(sprintf(url, str_player))

    war <- html %>%
      html_nodes("#all_batting_value") %>%
      html_nodes(xpath = 'comment()') %>%
      html_text() %>%
      read_html() %>%
      html_nodes(css = 'th[data-stat="player_stats_summary_explain"] ~ td[data-stat="WAR"]') %>%
      .[[1]] %>%
      html_text()

  }, warning = function(warning_condition) {
      print('warning')
  }, error = function(error_condition) {
      recent_players_info <- rbind(recent_players_info, c(player, F, NA, "Player not found."))
  }, finally={
    war_results[player] <- as.numeric(war)
  })

  Sys.sleep(2)
}

war_results <- t(data.frame(war_results))
colnames(war_results) <- c("WAR")
write.csv(war_results, "posplayerWAR.csv")