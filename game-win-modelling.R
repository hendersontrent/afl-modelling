#------------------------------------------
# This script sets out to produce a machine
# learning model of what predicts a team
# winning a game or not
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

# Define a reusable function

pull_afl_data <- function(start_date, end_date){
  tmp <- get_afltables_stats(start_date = start_date, end_date = end_date) %>%
    clean_names() %>%
    mutate(season = gsub("-.*", "\\1", date),
           season = as.numeric(season))
  
  if(nrow(tmp) == 0){
    print("Data not pulled successfully.")
  } else{
    return(tmp)
  }
}

# Pull data for the last 10 seasons

season_2011 <- pull_afl_data(start_date = "2011-01-01", end_date = "2011-12-01")
season_2012 <- pull_afl_data(start_date = "2012-01-01", end_date = "2012-12-01")
season_2013 <- pull_afl_data(start_date = "2013-01-01", end_date = "2013-12-01")
season_2014 <- pull_afl_data(start_date = "2014-01-01", end_date = "2014-12-01")
season_2015 <- pull_afl_data(start_date = "2015-01-01", end_date = "2015-12-01")
season_2016 <- pull_afl_data(start_date = "2016-01-01", end_date = "2016-12-01")
season_2017 <- pull_afl_data(start_date = "2017-01-01", end_date = "2017-12-01")
season_2018 <- pull_afl_data(start_date = "2018-01-01", end_date = "2018-12-01")
season_2019 <- pull_afl_data(start_date = "2019-01-01", end_date = "2019-12-01")
season_2020 <- pull_afl_data(start_date = "2020-01-01", end_date = "2020-12-01")

#---------------------- PRE PROCESSING -----------------------------

#----------------
# Merge and clean
#----------------

# Merge datasets

tmp1 <- bind_rows(season_2011, season_2012, season_2013, season_2014, season_2015,
                  season_2016, season_2017, season_2018, season_2019, season_2020)

# Compute winner binary variable and retain only variables of interest
# by summing over each game. Removes goals and behinds as this will obviously
# be associated with the game outcome

tmp2 <- tmp1 %>%
  mutate(winner = case_when(
         home_score > away_score ~ home_team,
         away_score > home_score ~ away_team,
         TRUE                    ~ "Remove")) %>%
  filter(winner != "Remove") %>%
  mutate(did_i_win = case_when(
         playing_for == winner ~ "Win",
         TRUE                  ~ "Lose")) %>%
  mutate(did_i_win = as.factor(did_i_win)) %>%
  group_by(season, round, did_i_win) %>%
  summarise(kicks = sum(kicks),
            marks = sum(marks),
            handballs = sum(handballs),
            hit_outs = sum(hit_outs),
            tackles = sum(tackles),
            rebounds = sum(rebounds),
            inside_50s = sum(inside_50s),
            clearances = sum(clearances),
            clangers = sum(clangers),
            frees_for = sum(frees_for),
            frees_against = sum(frees_against),
            contested_possessions = sum(contested_possessions),
            uncontested_possessions = sum(uncontested_possessions),
            contested_marks = sum(contested_marks),
            marks_inside_50 = sum(marks_inside_50),
            bounces = sum(bounces),
            goal_assists = sum(goal_assists)) %>%
  ungroup() %>%
  dplyr::select(-c(season, round))

#--------------------
# Feature engineering
#--------------------

# Split data into train and test sets

sample_size <- floor(0.8 * nrow(tmp2))
set.seed(123)
train_ind <- sample(seq_len(nrow(tmp2)), size = sample_size)
train <- tmp2[train_ind,]
test <- tmp2[-train_ind,]

# Scale features

train[-1] = round(scale(train[-1]), digits = 3) # rounding helps with plot & doesn't impact model
test[-1] = round(scale(test[-1]), digits = 3)

#---------------------- MODELLING ----------------------------------

#-----------------------
# Random forest approach
#-----------------------

# Specify formula and fit model

n <- names(train)
the_formula <- as.formula(paste("did_i_win ~", paste(n[!n %in% "did_i_win"], collapse = " + ")))

# Fit model

model_rf <- randomForest(formula = the_formula, 
                         data = train,
                         ntree = 1000,
                         importance = TRUE)

# Evaluate training performance

confusionMatrix(train$did_i_win, predict(model_rf))

# Evaluate prediction accuracy

test_pred_rf <- predict(model_rf, newdata = test)
confusionMatrix(table(test_pred_rf, test$did_i_win))

# Extract variable importance

my_importance <- as.data.frame(importance(model_rf))
my_importance <- rownames_to_column(my_importance, var = "variable") %>%
  mutate(variable = str_to_sentence(variable)) %>% # Clean up names to read nicely
  mutate(variable = gsub("_", " ", variable))

# Variable importance plot

p <- my_importance %>%
  ggplot(aes(x = reorder(variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "slategray1") +
  labs(title = "Variable importance of variables in random forest model",
       subtitle = "Model used the variables to predict whether the game was won or not.\nData was aggregated to game-level sums per team.",
       x = "Variable",
       y = "Mean Decrease in Accuracy",
       caption = "Source: CRAN package fitzRoy which pulls data from www.afltables.com\nAnalysis: Orbisant Analytics.") +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  coord_flip() +
  my_theme
print(p)

CairoPNG("output/rf-importance.png", 800, 600)
print(p)
dev.off()

# Decision tree plot

CairoPNG("output/rf-tree.png", 1500, 800)
reprtree:::plot.getTree(model_rf)
title(main = "Random forest model decision tree\nAll variables scaled prior to modelling.",
      sub = "Source: CRAN package fitzRoy which pulls data from www.afltables.com\nAnalysis: Orbisant Analytics.",
      cex.main = 2, font.main = 2)
dev.off()
