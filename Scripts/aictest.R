library(tidymodels)

## This function tests out every combination of n predictors, and returns a list containing the formula, ordered by AIC.
# vars - these are the variables you will be testing
# n - number of variables the model will use to predict `wein`
test_models <- function(vars, n) {
  combos <- combn(vars, n)
  formulas <- list()
  
  for (i in 1:ncol(combos)) {
    fm <- paste0("wein ~ ", paste(combos[,i], collapse = "+"))
    mod <- glm(fm, family = "binomial", data = PositionPlayerHOF_df)
    aic <- extractAIC(mod)[2]
    formulas[[fm]] <- aic
  }
  
  return(formulas[order(unlist(formulas), decreasing=TRUE)])
}

test_models(c("H", "HR", "RBI", "SB", "CS", "HBP", "PrimaryLg", "gold_glove", "rookie_of_the_year", "OBP", "tsn_all_star", "tsn_player_of_the_year", "most_valuable_player", "GIDP"), 3)

