library(tidyverse)

set.seed(12)

PitcherHOF_df <- read.csv("../data/PitcherHOF_df.csv")
PositionPlayerHOF_df <- read.csv("../data/PositionPlayerHOF_df.csv")

train_test_split <- function(data, prop) {
  smp_size <- floor(prop * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  return(list(train = data[train_ind,], test = data[-train_ind,]))
}

pitcher_split <- train_test_split(PitcherHOF_df, 0.8)

PitcherHOF_df_train <- pitcher_split$train
PitcherHOF_df_test <- pitcher_split$test

position_split <- train_test_split(PositionPlayerHOF_df, 0.8)

PositionPlayerHOF_df_train <- position_split$train
PositionPlayerHOF_df_test <- position_split$test


### INITIAL
pitcher1 <- glm(
  wein ~ W + L + G + IP + ERA +
    all_star + WHIP + SO +
    gold_glove + SV +
    cy_young_award +
    most_valuable_player +
    pitching_triple_crown +
    rolaids_relief_man_award +
    nice_guy_awards,
  data = PitcherHOF_df_train,
  family = "binomial"
)

position1 <- glm(
  wein ~ G + AB + R + H + HR +
    RBI + SB + BB +
    PrimaryLg + all_star +
    most_valuable_player +
    AVG + OBP + SLG +
    nice_guy_awards +
    Pos + gold_glove +
    silver_slugger +
    hank_aaron_award,
  data = PositionPlayerHOF_df_train,
  family = "binomial"
)

### MEDIATOR
pitcher2 <- glm(
  wein ~ W + L + G + IP + ERA +
    all_star + WHIP + SO +
    gold_glove + SV +
    cy_young_award +
    Steroids +
    most_valuable_player +
    pitching_triple_crown +
    rolaids_relief_man_award +
    nice_guy_awards,
  family = "binomial",
  data = PitcherHOF_df_train
)

position2 <- glm(
  wein ~ G + AB + R + H + HR +
    RBI + SB + BB +
    PrimaryLg + all_star +
    most_valuable_player +
    AVG + OBP + SLG +
    Steroids + 
    nice_guy_awards +
    Pos + gold_glove + 
    silver_slugger +
    hank_aaron_award,
  family = "binomial",
  data = PositionPlayerHOF_df_train
)

### FINAL
final_pitch_glm <- glm(
  wein ~ W + L + SO + SV + Steroids +
    most_valuable_player + all_star + cy_young_award +
    rolaids_relief_man_award + votedBy +
    StarterReliever + Steroids:most_valuable_player,
  family = "binomial",
  data = PitcherHOF_df_train
)

final_pos_glm <-
  glm(
    wein ~ all_star + Pos + HR + nice_guy_awards + Steroids +
      votedBy + RBI + AVG + most_valuable_player + R + SB +
      Pos:HR + all_star:most_valuable_player +
      Steroids:SLG,
    family = "binomial",
    data = PositionPlayerHOF_df_train
  )

save.image(file = "modeliters.RData")

