library(rvest)
library(tidyverse)

hof_eligible_pos <- read.csv("data/hof_eligible_pos.csv")
hof_eligible_pitcher <- read.csv("data/hof_eligible_pitcher.csv")

results <- data.frame(matrix(ncol=3)) %>% 
  head(-1)

colnames(results) <- c("player", "war", "all_stars")

url <- "https://www.baseball-reference.com/players/%s.shtml"

for (player in hof_eligible_pitcher$playerID) {
  war <- NA
  all_star <- NA
  result <- tryCatch({
    str_player <- paste0(str_sub(player, 1, 1), "/", player)
    
    html <- read_html(sprintf(url, str_player))
    
    war <- html %>%
      html_nodes("#all_pitching_value") %>%
      html_nodes(xpath = 'comment()') %>%
      html_text() %>%
      read_html() %>%
      html_nodes(css = 'th[data-stat="player_stats_summary_explain"] ~ td[data-stat="WAR_pitch"]') %>%
      .[[1]] %>%
      html_text() %>% 
      as.numeric()
    
    all_star <- html %>% 
      html_nodes(css = 'li[class="all_star"]')
    
    if (!is_empty(all_star)) {
      all_star <- all_star %>% 
        html_node("a") %>% 
        html_text() %>% 
        str_split("x") %>% 
        .[[1]] %>% 
        .[[1]] %>% 
        as.numeric()
      
    } else {
      all_star <- 0
    }
      
  }, warning = function(warning_condition) {
    print(warning_condition)
  }, error = function(error_condition) {
    print(error_condition)
    results <- rbind(results, list('player' = player, 'war' = war, 'all_stars' = all_star))
  }, finally = {
    results <- rbind(results, list('player' = player, 'war' = war, 'all_stars' = all_star))
  })
  closeAllConnections()
  gc()
  
  Sys.sleep(2)
}

# war_results <- t(data.frame(war_results))
# colnames(results) <- c("WAR")
write.csv(results, "pitcherWARupdated.csv", row.names = F)

results <- list()

url <- "https://www.baseball-reference.com/players/%s.shtml"

for (player in PositionPlayerHOF_df$playerID) {
  war <- NA
  all_star <- NA
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
      html_text() %>% 
      as.numeric()
    
    all_star <- html %>% 
      html_nodes(css = 'li[class="all_star"]')
    
    if (!is_empty(all_star)) {
      all_star <- all_star %>% 
        html_node("a") %>% 
        html_text() %>% 
        str_split("x") %>% 
        .[[1]] %>% 
        .[[1]] %>% 
        as.numeric()
      
    } else {
      all_star <- 0
    }
    
  }, warning = function(warning_condition) {
    print(warning_condition)
  }, error = function(error_condition) {
    print(error_condition)
    results <- rbind(results, list('player' = player, 'war' = war, 'all_stars' = all_star))
  }, finally = {
    results <- rbind(results, list('player' = player, 'war' = war, 'all_stars' = all_star))
  })
  closeAllConnections()
  gc()
  
  Sys.sleep(2)
}

write.csv(results, "posplayerWARupdated.csv")