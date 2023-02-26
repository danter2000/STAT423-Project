library(tidyverse)
library(Lahman)
library(readr)
library(lubridate)
library(GGally)
library(leaps)

# Please create a variable for all datasets below along with a message of what we need it for

# To find career position player numbers
batting_df <- read_csv(file = "../data/Batting2022updated.csv")


# To find career pitcher numbers
pitching_df <- read_csv(file = "../data/PitchingUpdated2022.csv")

# For our response of boolean elected into the hall or not
hall_df <- read_csv(file = "../data/HallOfFameUpdated2022.csv")

# For the WS win column
team_df <- read_csv(file = "../data/TeamsUpdated2022.csv")

# For awards won by players
awards_df <- read_csv(file = "../data/AwardsPlayers.csv")

# For allstar game appearences
allstar_df <- read_csv(file = "../data/AllstarFull.csv") %>%
  mutate(awardID = "All-Star")

# Preleminary PED data frames
espn_ped_df <- read_csv(file = "../data/suspectedusers_espn.csv")
wiki_ped_df <-
  read_csv(file = "../data/suspendedlist_wikipedia.csv")

war_df_pos <- read_csv(file = "../data/posplayerWARupdated.csv")
colnames(war_df_pos)[1] = "playerID"

war_df_pitcher <- read_csv(file = "../data/pitcherWARupdated.csv")
colnames(war_df_pitcher)[1] = "playerID"


# Sums counting stats and removes categoricals
batting_df_continuous_counts <- batting_df %>%
  subset(select = -c(2:5)) %>%
  group_by(playerID) %>%
  summarise(across(everything(), sum))

# Find the mode of most common team and league per player
batting_df_categorical_counts <- batting_df %>%
  subset(select = c(1, 4, 5)) %>%
  mutate(across(c(1:3), as.factor)) %>%
  group_by(playerID) %>%
  mutate(PrimaryTeam = names(which.max(table(teamID)))) %>%
  mutate(PrimaryLg = as.factor(names(which.max(table(
    lgID
  ))))) %>%
  subset(select = c(1, 4, 5)) %>%
  distinct()

# Extract response y/n variable
hall_df_tidy <- hall_df %>%
  group_by(playerID) %>%
  subset(select = c(1, 3, 7, 8)) %>%
  filter(category == "Player")

# Sums counting stats for pitching
pitching_df_continuous_counts <- pitching_df %>%
  subset(select = -c(2:5, 19)) %>%
  group_by(playerID) %>%
  summarize(across(everything(), sum)) %>%
  mutate(ERA = round(ER / IPouts * 27, 2),
         WHIP = round((BB + H) / IPouts * 3, 2))

# Find the mode of most common team and league per pitcher
pitching_df_categorical_counts <- pitching_df %>%
  subset(select = c(1, 4, 5)) %>%
  mutate(across(c(1:3), as.factor)) %>%
  group_by(playerID) %>%
  mutate(PrimaryTeam = names(which.max(table(teamID)))) %>%
  mutate(PrimaryLg = as.factor(names(which.max(table(
    lgID
  ))))) %>%
  subset(select = c(1, 4, 5)) %>%
  distinct()

# Unique HOF inducted and non-inducted players
hof_players <- hall_df %>%
  filter(category == "Player") %>%
  group_by(playerID) %>%
  summarize(wein = "Y" %in% inducted * 1,
            votedBy,
            yearID,
            maxYear = max(yearID)) %>%
  filter(yearID == maxYear) %>%
  select(-c(yearID, maxYear))

# Unique award winners -- columns represent different rewards
awards_new <- awards_df %>%
  filter(!(yearID > 1933 &
             awardID == "Baseball Magazine All-Star")) %>%
  filter(awardID != "TSN All-Star") %>%
  full_join(allstar_df[, c(1, 2, 6, 9)], by = c("playerID",
                                                "yearID",
                                                "lgID",
                                                "awardID"))

awards_wider <- awards_new %>%
  mutate(award_id_clean = str_replace_all(str_replace_all(str_to_lower(awardID), " ", "_"), "-", "_")) %>%
  group_by(playerID, award_id_clean) %>%
  summarize(n = n()) %>%
  pivot_wider(id_cols = playerID,
              names_from = award_id_clean,
              values_from = n) %>%
  mutate(across(1:28, ~ replace_na(.x, 0)))

# Preleminary PED data frames
espn_ped_df <- read_csv(file = "../data/suspectedusers_espn.csv")

espn_ped_df <- espn_ped_df %>%
  subset(select = c(2, 3)) %>%
  rename(Player = name)

# For player name, debute, and playerID conversion
player_df <- read_csv(file = "../data/People2022.csv")

# Creates Player column to ease merging with other df's
player_df <- player_df %>% mutate(Player = str_c(nameFirst,
                                                 nameLast,
                                                 sep = " "))

# Main Pitcher data frame to preform analysis on
PitcherHOF_df <- inner_join(pitching_df_continuous_counts,
                            pitching_df_categorical_counts,
                            by = "playerID") %>%
  left_join(awards_wider, by = "playerID") %>%
  inner_join(hof_players, by = "playerID") %>%
  filter(G > 200) %>%
  inner_join(player_df[, c(1, 25)], by = "playerID") %>%
  mutate(IP = IPouts / 3) %>%
  relocate(Player, .after = playerID) %>%
  relocate(IP, .after = IPouts) %>%
  mutate(Steroids = ifelse(
    Player %in% espn_ped_df$Player |
      Player %in% wiki_ped_df$Player,
    1,
    0
  )) %>%
  mutate(nice_guy_awards = lou_gehrig_memorial_award +
           hutch_award +
           roberto_clemente_award +
           branch_rickey_award) %>%
  left_join(war_df_pitcher, by = "playerID") %>%
  distinct(playerID, .keep_all = TRUE) %>%
  mutate(votedBy = ifelse(
    Player == "Charles Bender",
    "Veterans",
    ifelse(
      Player == "Ed Walsh",
      "Veterans",
      ifelse(
        Player == "Eddie Plank",
        "Veterans",
        ifelse(
          Player == "Mel Harder",
          "BBWAA",
          ifelse(
            Player == "Red Ruffing",
            "BBWAA",
            ifelse(
              Player == "Rube Waddell",
              "Veterans",
              ifelse(
                Player == "Old Hoss Radbourn",
                "Veterans",
                ifelse(
                  Player == "Mordecai Brown",
                  "Veterans",
                  ifelse(
                    Player == "Nap Rucker",
                    "Veterans",
                    ifelse(Player == "Bill Donovan", "Veterans",
                           votedBy)
                  )
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  mutate(
    all_star = all_star + as.numeric(baseball_magazine_all_star),
    wein = as.factor(wein),
    votedBy = as.factor(votedBy)
  ) %>%
  filter(war != is.na(war))


# Primary position by player
primary_position <- Appearances %>%
  group_by(playerID) %>%
  summarize(
    P = sum(G_p),
    C = sum(G_c),
    `1B` = sum(G_1b),
    `2B` = sum(G_2b),
    `3B` = sum(G_3b),
    SS = sum(G_ss),
    RF = sum(G_rf),
    CF = sum(G_cf),
    LF = sum(G_lf),
    DH = sum(G_dh)
  ) %>%
  pivot_longer(cols = c(P, C, `1B`, `2B`, `3B`, SS, RF, CF, LF, DH),
               names_to = "Pos") %>%
  group_by(playerID) %>%
  summarize(Pos = Pos,
            Value = value,
            Max = max(value)) %>%
  filter(Value == Max) %>%
  select(playerID, Pos)



# Main Position Player data frame to preform analysis on
PositionPlayerHOF_df <- inner_join(batting_df_continuous_counts,
                                   batting_df_categorical_counts,
                                   by = "playerID") %>%
  left_join(awards_wider, by = "playerID") %>%
  inner_join(hof_players, by = "playerID") %>%
  subset(select = c(1:26, 28:31, 34:37, 40:42, 44, 45, 47, 49:51)) %>%
  mutate(across(c(19, 20, 29, 41), as.factor)) %>%
  filter(!(playerID %in% PitcherHOF_df$playerID)) %>%
  mutate(
    AVG = round(H / AB, 3),
    OBP = round((H + BB + HBP) / (
      AB + ifelse(!is.na(SH), SH, 0) + ifelse(!is.na(SF), SF, 0) + BB + HBP
    ), 3),
    `1B` = H - (`2B` + `3B` + HR),
    SLG = round((`1B` + `2B` * 2 + `3B` * 3 + HR * 4) / AB, 3)
  ) %>%
  inner_join(player_df[, c(1, 25)], by = "playerID") %>%
  relocate(Player, .after = playerID) %>%
  mutate(Steroids = ifelse(
    Player %in% espn_ped_df$Player |
      Player %in% wiki_ped_df$Player,
    1,
    0
  )) %>%
  mutate(
    Steroids = as.factor(Steroids),
    nice_guy_awards = lou_gehrig_memorial_award +
      hutch_award +
      roberto_clemente_award +
      branch_rickey_award
  ) %>%
  left_join(war_df_pos, by = "playerID") %>%
  distinct(playerID, .keep_all = TRUE) %>%
  mutate(votedBy = ifelse(
    Player == "Al Lopez",
    "Veterans",
    ifelse(
      Player == "Charlie Gehringer",
      "BBWAA",
      ifelse(
        Player == "Frank Chance",
        "Veterans",
        ifelse(
          Player == "Fred Clark",
          "Veterans",
          ifelse(
            Player == "Honus Wagner",
            "BBWAA",
            ifelse(
              Player == "Hugh Duffy",
              "Veterans",
              ifelse(
                Player == "Johnny Evers",
                "Veterans",
                ifelse(
                  Player == "Luke Appling",
                  "BBWAA",
                  ifelse(
                    Player == "Ray Schalk",
                    "Veterans",
                    ifelse(
                      Player == "Roger Bresnahan",
                      "Veterans",
                      ifelse(
                        Player == "Roberto Clemente",
                        "BBWAA",
                        ifelse(
                          Player == "Lou Gehrig",
                          "BBWAA",
                          ifelse(
                            Player == "Lou Criger",
                            "Veterans",
                            ifelse(
                              Player == "Shoeless Joe Jackson",
                              "Veterans",
                              ifelse(
                                Player == "Bill Killefer",
                                "Veterans",
                                ifelse(
                                  Player == "Billy Sullivan",
                                  "Veterans",
                                  ifelse(Player == "Monte Irvin", "Veterans",
                                         votedBy)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  mutate(Player = ifelse(playerID == "griffke02", "Ken Griffey Jr", Player)) %>%
  inner_join(primary_position, by = "playerID") %>%
  mutate(
    Pos = as.factor(Pos),
    votedBy = as.factor(votedBy),
    all_star = all_star + as.numeric(baseball_magazine_all_star)
  ) %>%
  mutate(wein = as.factor(wein)) %>%
  filter(Player != "Satchel Paige") %>%
  filter(Player != "Shoeless Joe Jackson") %>%
  filter(Player != "Pete Rose") %>%
  filter(war != is.na(war))

PositionPlayerHOF_df$all_star[is.na(PositionPlayerHOF_df$all_star)] <-
  0
PositionPlayerHOF_df$most_valuable_player[is.na(PositionPlayerHOF_df$most_valuable_player)] <-
  0
PositionPlayerHOF_df$gold_glove[is.na(PositionPlayerHOF_df$gold_glove)] <-
  0
PositionPlayerHOF_df$nice_guy_awards[is.na(PositionPlayerHOF_df$nice_guy_awards)] <-
  0

row.names(PositionPlayerHOF_df) <- PositionPlayerHOF_df$playerID

# update hall_df to show years with less than five percent of ballots
hall_df_update <- hall_df %>%
  mutate(fivePercent = ifelse(1.0 * votes / ballots > 0.05, 1, 0))

# update hof_players with yearsOnBallet and fivePercent
hof_players <- hall_df_update %>%
  filter(category == "Player") %>%
  group_by(playerID) %>%
  mutate(yearsOnBallet = n()) %>%
  summarize(
    wein = "Y" %in% inducted * 1,
    votedBy,
    yearID,
    maxYear = max(yearID),
    yearsOnBallet,
    fivePercent
  ) %>%
  filter(yearID == maxYear) %>%
  select(-c(yearID, maxYear))

# df with players that have played more than 10 years, have not had less than 5% on a ballot or failed to make the
# hall of fame after 10 years
# note that players who never made it on a ballot are included
PredictHOF <- player_df %>%
  filter(as.numeric(difftime(finalGame, debut, units = "days")) / 365 > 10) %>%
  left_join(hof_players, by = "playerID") %>%
  filter(wein != 1 | is.na(wein)) %>%
  filter(fivePercent != 1 | is.na(fivePercent)) %>%
  filter(yearsOnBallet < 10 | is.na(yearsOnBallet)) %>%
  mutate(votedBy = ifelse(as.Date(finalGame) +  10 * 365 < Sys.Date(),
                          "Veterans",
                          "BBWAA"))

# Prediction test set for position players

PredictHOF_pos_df <- PredictHOF[, c(1, 25, 27)] %>%
  inner_join(batting_df_continuous_counts) %>%
  inner_join(batting_df_categorical_counts) %>%
  inner_join(awards_wider) %>%
  mutate(
    AVG = round(H / AB, 3),
    OBP = round((H + BB + HBP) / (
      AB + ifelse(!is.na(SH), SH, 0) +
        ifelse(!is.na(SF), SF, 0) + BB + HBP
    ), 3),
    `1B` = H - (`2B` + `3B` + HR),
    SLG = round((`1B` + `2B` * 2 + `3B` * 3 + HR * 4) / AB, 3)
  ) %>%
  mutate(Steroids = ifelse(
    Player %in% espn_ped_df$Player |
      Player %in% wiki_ped_df$Player,
    1,
    0
  )) %>%
  mutate(
    Steroids = as.factor(Steroids),
    nice_guy_awards = lou_gehrig_memorial_award +
      hutch_award +
      roberto_clemente_award +
      branch_rickey_award
  ) %>%
  inner_join(primary_position, by = "playerID") %>%
  mutate(Pos = as.factor(Pos)) %>%
  left_join(war_df_pos, by = "playerID") %>%
  mutate(all_star = all_stars + as.numeric(baseball_magazine_all_star)) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  filter(Pos != "P")

row.names(PredictHOF_pos_df) <- PredictHOF_pos_df$playerID

# Prediction test set for pitchers

PredictHOF_pitch_df <- PredictHOF[, c(1, 25, 27)] %>%
  inner_join(pitching_df_continuous_counts,
             pitching_df_categorical_counts,
             by = "playerID") %>%
  inner_join(awards_wider, by = "playerID") %>%
  mutate(IP = IPouts / 3) %>%
  mutate(Steroids = ifelse(
    Player %in% espn_ped_df$Player |
      Player %in% wiki_ped_df$Player,
    1,
    0
  )) %>%
  mutate(nice_guy_awards = lou_gehrig_memorial_award +
           hutch_award +
           roberto_clemente_award +
           branch_rickey_award) %>%
  left_join(war_df_pitcher, by = "playerID") %>%
  mutate(all_star = all_stars + as.numeric(baseball_magazine_all_star)) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  filter(G > 10) %>%
  filter(Player != "Joe Coleman") %>%
  filter(all_star != is.na(all_star)) %>%
  mutate(votedBy = as.factor(votedBy))

row.names(PredictHOF_pitch_df) <- PredictHOF_pitch_df$playerID

# Relevel to set DH as our position reference group
PositionPlayerHOF_df$Pos <-
  relevel(as.factor(PositionPlayerHOF_df$Pos),
          ref = "DH")
# Below are mainly for time series analysis

# For player name, debut, and playerID conversion
player_df <- read_csv(file = "../Data/People2022.csv")

# Creates Player column to ease merging with other df's
player_df <- player_df %>% mutate(Player = str_c(nameFirst,
                                                 nameLast,
                                                 sep = " "))

# For updated pitching data to get accurate innings pitched totals
pitcherTimeSeries_df <- read_csv(file = "../Data/Pitching2022.csv")

# vector of playerID's who've played in 2017 or beyond
pitcherTimeSeries_df_2017onwards <- pitcherTimeSeries_df %>%
  group_by(playerID) %>%
  filter(yearID >= 2017) %>%
  pull(playerID) %>%
  unique()

# Applies vector to main data set and filters, creates IP column
pitcherTimeSeries_df <- pitcherTimeSeries_df %>%
  filter(playerID %in% pitcherTimeSeries_df_2017onwards) %>%
  mutate(IP = round(IPouts / 3, 1))

# Merges Player bio df with median innings pitched df
avg_ip_df <- pitcherTimeSeries_df %>%
  group_by(playerID) %>%
  summarise_at(vars(IP), list(MedianIP = median)) %>%
  inner_join(player_df, by = "playerID") %>%
  mutate(played2017plus = T)

# For time series pitch frequency covariates
pitch_arsenal_df <-
  rbind(
    read_csv(file = "../data/pitch_arsenals_2017.csv"),
    read_csv(file = "../data/pitch_arsenals_2018.csv"),
    read_csv(file = "../data/pitch_arsenals_2019.csv"),
    read_csv(file = "../data/pitch_arsenals_2020.csv"),
    read_csv(file = "../data/pitch_arsenals_2021.csv"),
    read_csv(file = "../data/pitch_arsenals_2022.csv")
  )


# We are making the assumption that players retain primary pitch type prior to the 2017 StatCast season.

# Create universal name variable to join with tommyJohn_df
pitch_arsenal_df <-
  pitch_arsenal_df %>% mutate(Player = str_c(first_name,
                                             last_name,
                                             sep = " ")) %>%
  group_by(Player) %>%
  summarise_at(vars(n_ff,
                    n_si,
                    n_fc,
                    n_sl,
                    n_ch,
                    n_cu,
                    n_fs), list(mean))

# For dates and player who had Tommy John surgery
tommyJohn_df <-
  read_csv(file = "../data/Tommy John Surgery List (@MLBPlayerAnalys) - TJ List.csv")

# Gets rid of spaces in col names
names(tommyJohn_df) <- str_replace_all(names(tommyJohn_df),
                                       c(" " = "_" , "," = ""))

# Merges the TJ, pitch arsenal, player bio, and IP data. Indicates censored
timeseries_df <-
  full_join(pitch_arsenal_df, tommyJohn_df, by = "Player") %>%
  mutate(TJ_Surgery_Date = mdy(TJ_Surgery_Date)) %>%
  full_join(avg_ip_df, by = "Player") %>%
  filter(played2017plus == T) %>%
  mutate(HadTJ = ifelse(is.na(TJ_Surgery_Date), 0, 1)) %>%
  mutate(Censored = ifelse(
    HadTJ == "No" & finalGame < "2022-09-01",
    "Censored",
    "Non-Censored"
  )) %>%
  mutate(
    PrimaryPitch = ifelse(
      n_ff > n_si
      & n_ff > n_fc
      & n_ff > n_sl
      & n_ff > n_ch
      & n_ff > n_cu
      & n_ff > n_fs,
      "4seamFastball",
      ifelse(
        n_si > n_ff
        & n_si > n_fc
        & n_si > n_sl
        & n_si > n_ch
        & n_si > n_cu
        & n_si > n_fs,
        "Sinker",
        ifelse(
          n_fc > n_ff
          & n_fc > n_si
          & n_fc > n_sl
          & n_fc > n_ch
          & n_fc > n_cu
          &
            n_fc > n_fs,
          "Cutter",
          ifelse(
            n_sl > n_ff
            & n_sl > n_fc
            & n_sl > n_si
            & n_sl > n_ch
            & n_sl > n_cu
            &
              n_sl > n_fs,
            "Slider",
            ifelse(
              n_ch > n_ff
              &
                n_ch > n_fc
              &
                n_ch > n_sl
              &
                n_ch > n_si
              &
                n_ch > n_cu
              &
                n_ch > n_fs,
              "Change-up",
              ifelse(
                n_cu > n_ff
                &
                  n_cu > n_fc
                &
                  n_cu > n_sl
                &
                  n_cu > n_ch
                &
                  n_cu > n_si
                &
                  n_cu > n_fs,
                "Curveball",
                "Splitter"
              )
            )
          )
        )
      )
    )
  ) %>%
  mutate(numDaysUntilTJ = ifelse(
    !is.na(TJ_Surgery_Date),
    TJ_Surgery_Date - debut,
    finalGame - debut
  )) %>%
  filter(is.na(numDaysUntilTJ) == T | numDaysUntilTJ > 0) %>%
  mutate(
    Fast_Off_Break = ifelse(
      PrimaryPitch == "4seamFastball"
      | PrimaryPitch == "Cutter"
      |
        PrimaryPitch == "Sinker",
      "Fastball",
      ifelse(
        PrimaryPitch == "Splitter"
        |
          PrimaryPitch == "Change-up",
        "Offspeed",
        "Breakingball"
      )
    )
  ) %>%
  filter(!is.na(n_ff))

# Final data frame to be used for analysis
final_time_series_df <- timeseries_df %>%
  select(1, 50, 55, 51, 67, 68, 70, 78, 80, 76, 79)

# Cooks distance for both bad and good models position players ----

row.names(PositionPlayerHOF_df) <- PositionPlayerHOF_df$Player

bad_glm_pos <- glm(
  wein ~ G + AB + R + H + HR + RBI + SB + BB +
    PrimaryLg + all_star +
    most_valuable_player + AVG + OBP + SLG +
    Steroids + nice_guy_awards + war + Pos +
    gold_glove + silver_slugger +
    hank_aaron_award + Steroids:HR +
    H:Steroids,
  data = PositionPlayerHOF_df,
  family = "binomial"
)

plot(bad_glm_pos, which = 4)

good_glm_pos <-
  glm(
    wein ~ G + AB + R + H + RBI + SB + BB + PrimaryLg +
      all_stars * war + most_valuable_player +
      AVG + OBP + SLG + HR:Steroids + war:Steroids +
      nice_guy_awards + HR * war + Pos + votedBy,
    data = PositionPlayerHOF_df,
    family = "binomial"
  )

plot(good_glm_pos, which = 4)

# Position model sans war
good_glm_pos_nowar <- step(
  glm(
    wein ~ all_star + HR:Steroids + G + Pos + HR +
      nice_guy_awards + Steroids + HR*Pos + votedBy +
      RBI + gold_glove + AVG*Steroids + most_valuable_player*all_star
    + SLG:Steroids + R + G + SB,
    data = PositionPlayerHOF_df,
    family = "binomial"
  )
)

summary(good_glm_pos_nowar)

# Cooks distance for both bad and good models pitchers ----

row.names(PositionPlayerHOF_df) <- NULL

final_pos_glm <- glm(wein ~ all_star + G + Pos + HR + nice_guy_awards + 
                       Steroids + votedBy + RBI + gold_glove + AVG +
                       most_valuable_player + SB + Pos:HR + 
                       all_star:most_valuable_player + Steroids:SLG,
                     family = "binomial",
                     data = PositionPlayerHOF_df)

# Pitcher
row.names(PitcherHOF_df) <- NULL

final_pitch_glm <- glm(wein ~ W + L + SO + SV + Steroids +
                         most_valuable_player + all_star + 
                         cy_young_award + rolaids_relief_man_award + votedBy +
                         Steroids:most_valuable_player,
                       family = "binomial",
                       data = PitcherHOF_df)
##### PLOTS
library(ggalt)
library(patchwork)
library(ggrepel)

par(mfrow=c(1,2))
plot(final_pos_glm, which = 4)
plot(final_pitch_glm, which = 4)

cookit <- function(mod, data, subtitle) {
  cooksd <- cooks.distance(mod)
  
  df <- data %>% 
    mutate(i = 1:n()) %>% 
    left_join(data.frame(i = as.numeric(names(cooksd)), cooksd = as.numeric(cooksd)), by = "i")
  
  top10 <- df %>% arrange(-cooksd) %>% head(10) %>% 
    pull(playerID)
    
  df %>% 
    mutate(over = playerID %in% top10) %>% 
    ggplot() +
    geom_bar(aes(x = i, y = cooksd, alpha = over), stat = "identity", fill = "dodgerblue4") +
    # geom_text(aes(x = ifelse(cooksd > mean(cooksd) * 1.5, i, NA), y = ifelse(cooksd > mean(cooksd) * 1.5, cooksd, NA), label = ifelse(cooksd > mean(cooksd) * 1.5, Player, NA))) +
    geom_label_repel(data = df %>% filter(playerID %in% top10), aes(x = i, y = cooksd, label = Player), fontface = "italic", nudge_y = 0.01) +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(size = 15),
      plot.title = element_text(face = "bold")
    ) +
    labs(x = "Observation Number", y = "Cook's Distance", title = "Cook's Distance", subtitle = subtitle)
}

cookit(final_pos_glm, PositionPlayerHOF_df, "Position Players")
cookit(final_pitch_glm, PositionPlayerHOF_df, "Pitchers")

od_plot <- function(mod, subtitle) {
  OD <- sum(residuals(mod,
                            type = "deviance") ^ 2) /
    mod$df.residual
  pchisq(OD, 1)
  
  ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
    stat_function(fun = pchisq, args = list(df = 1))
}

or_conf_int_plot <- function(mod, subtitle) {
  or_conf_int <-
    exp(cbind("Odds ratio" = coef(mod), confint.default(mod, level = 0.95))) %>%
    data.frame()
  
  colnames(or_conf_int) <-
    c("odds.ratio", "conf.lower", "conf.upper")
  
  or_conf_int$predictor <- row.names(or_conf_int)
  
  row.names(or_conf_int) <- NULL
  
  or_conf_int$predictor <-
    factor(or_conf_int$predictor, or_conf_int[order(or_conf_int$odds.ratio), "predictor"])
  
  p1 <- or_conf_int %>%
    filter(odds.ratio < 1) %>%
    ggplot() +
    geom_dumbbell(
      aes(x = conf.lower, xend = conf.upper, y = predictor),
      color = "#ffa600",
      alpha = 0.5
    ) +
    geom_point(aes(x = odds.ratio, y = predictor), color = "#003f5c", size = 5) +
    scale_x_log10(n.breaks = 10) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.text.y = element_text(face = "italic"),
      plot.title = element_text(face = "bold")
    ) +
    labs(
      x = "Odds Ratio (log scaled)",
      y = "",
      title = "Odds Ratios < 1 (95% CI)",
      subtitle = subtitle
    ) 
  
  p2 <- or_conf_int %>%
    filter(odds.ratio > 1) %>%
    ggplot() +
    geom_dumbbell(
      aes(x = conf.lower, xend = conf.upper, y = predictor),
      color = "#ffa600",
      alpha = 0.5
    ) +
    geom_point(aes(x = odds.ratio, y = predictor), color = "#003f5c", size = 5) +
    scale_x_log10(n.breaks = 10) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.text.y = element_text(face = "italic"),
      plot.title = element_text(face = "bold")
    ) +
    labs(
      x = "Odds Ratio (log scaled)",
      y = "",
      title = "Odds Ratios > 1 (95% CI)",
      subtitle = subtitle
    ) 
  
  return(p1 + p2)
}

or_conf_int_plot(final_pos_glm, "Position Players Model")
or_conf_int_plot(final_pitch_glm, "Pitchers Model")
# or_conf_int_plot(good_glm_pos_nowar, "Position Players Model: No WAR")

library(DHARMa) 

par(mfrow=c(1,2))
testDispersion(final_pos_glm) 
testDispersion(final_pitch_glm)

get_preds <- function(mod, data, subtitle) {
  df <- cbind(data , prob_wein = predict(mod, newdata = data, type = "response")) %>% 
    arrange(-prob_wein)
  
  df$playerID <- factor(df$playerID, levels = df$playerID[order(df$prob_wein)])
  
  p1 <- df %>% 
    head(10) %>% 
    ggplot() + 
    geom_bar(aes(x = playerID, y = prob_wein), stat="identity", fill = "#003f5c") +
    geom_text(aes(x = playerID, y = prob_wein + 0.03, label = Player)) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.text.x = element_blank(),
      plot.title = element_text(face = "bold")
    ) +
    labs(
      x = "",
      y = "Hall of Fame Probability",
      title = "Predicted Hall of Fame Probability: Top 10",
      subtitle = subtitle
    ) 
  
  df <- df %>% 
    filter(!is.na(prob_wein)) %>% 
    arrange(-prob_wein) %>% 
    tail(10)
  
  df$playerID <- factor(df$playerID, levels = df$playerID[order(df$prob_wein)])
  
  p2 <- df %>% 
    ggplot() + 
    geom_bar(aes(x = playerID, y = prob_wein), stat="identity", fill = "#003f5c") +
    geom_text(aes(x = playerID, y = prob_wein + (prob_wein / 10), label = Player)) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.text.x = element_blank(),
      plot.title = element_text(face = "bold")
    ) +
    labs(
      x = "",
      y = "Hall of Fame Probability",
      title = "Predicted Hall of Fame Probability: Bottom 10",
      subtitle = subtitle
    ) 
  
  return(p1 / p2)
}

get_preds(final_pos_glm, PredictHOF_pos_df, "Probabilities for Position Players")
get_preds(final_pitch_glm, PredictHOF_pitch_df, "Probabilities for Pitchers")

library(RColorBrewer)

get_confusion_matrix <- function(mod, data, title) {
  fitted <- fitted(mod)
  
  confusion_matrix <- data %>% 
    mutate(i = 1:n()) %>% 
    left_join(data.frame(i = as.numeric(names(fitted)), fit = fitted), by = "i") %>% 
    select(-i) %>%
    mutate(wein_pred = as.factor((fit > 0.5)*1)) %>%
    group_by(wein_pred, wein) %>%
    summarize(n = n()) %>%
    filter(!is.na(wein_pred)) %>% 
    mutate(label = paste0(n, " ",ifelse(wein_pred == 1 & wein == 1, "TP's", 
                          ifelse(wein_pred == 0 & wein == 0, "TN's", 
                                 ifelse(wein_pred == 1 & wein == 0, "FP's", "FN's")))))
  
  accuracy <- round(sum(confusion_matrix[(confusion_matrix$wein_pred == 0 & confusion_matrix$wein == 0) | (confusion_matrix$wein_pred == 1 & confusion_matrix$wein == 1), "n"]) / sum(confusion_matrix$n), 2)

  confusion_matrix %>%
    # mutate(wein_pred = factor(wein_pred, level = c(0, 1)), wein = factor(wein, level = c(0, 1))) %>% 
    mutate(wein = ifelse(wein == 1, "HoF", "No HoF"), wein_pred = ifelse(wein_pred == 1, "HoF", "No HoF")) %>%
    ggplot() +
    geom_tile(aes(x = factor(wein_pred, level = c("HoF", "No HoF")), y = factor(wein, level = c("No HoF", "HoF")), fill = n)) +
    geom_text(aes(x = wein_pred, y = wein, label = label)) +
    scale_fill_fermenter(direction = 1) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.text.x = element_text(face = "italic"),
      axis.text.y = element_text(face = "italic"),
      plot.title = element_text(face = "bold")
    ) +
    labs(x = "Predicted Label", y = "True Label", title = paste0("Confusion Matrix: ", title), subtitle = paste0("Accuracy: ", accuracy)) %>%
    return()
}

get_confusion_matrix(final_pos_glm, PositionPlayerHOF_df, "Position Players")
get_confusion_matrix(final_pitch_glm, PitcherHOF_df, "Pitchers")

