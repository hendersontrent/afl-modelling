#------------------------------------------
# This script sets out to produce a series
# of distribution plots for key variables
# to visually assess differences between
# finals and season games
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

# Aggregate the data

tmp2 <- tmp1 %>%
  mutate(finals = case_when(
         round == "EF" ~ "Finals",
         round == "SF" ~ "Finals",
         round == "QF" ~ "Finals",
         round == "PF" ~ "Finals",
         round == "GF" ~ "Finals",
         TRUE          ~ "Regular Season")) %>%
  group_by(season, round, finals, home_team) %>%
  summarise(kicks = sum(kicks),
            marks = sum(marks),
            goals = sum(goals),
            behinds = sum(behinds),
            contested_possessions = sum(contested_possessions),
            contested_marks = sum(contested_marks),
            inside_50s = sum(inside_50s),
            clearances = sum(clearances),
            handballs = sum(handballs)) %>%
  group_by(season, round, finals) %>%
  summarise(kicks = mean(kicks),
            marks = mean(marks),
            goals = mean(goals),
            behinds = mean(behinds),
            contested_possessions = mean(contested_possessions),
            contested_marks = mean(contested_marks),
            inside_50s = mean(inside_50s),
            clearances = mean(clearances),
            handballs = mean(handballs)) %>%
  ungroup()

#---------------------- MODELLING ----------------------------------

tmp_bin <- tmp2 %>%
  mutate(finals = case_when(
    finals == "Finals" ~ 1,
    TRUE               ~ 0))

n <- names(tmp_bin)
the_formula <- as.formula(paste("finals ~", paste(n[!n %in% c("finals", "season", "round")], 
                                                  collapse = " + ")))

mod <- glm(formula = the_formula,
           data = tmp_bin,
           family = "binomial")

#-------------------
# ASSUMPTION TESTING
#-------------------

# Multicollinearity

car::vif(mod)

# Re-fit model

n_clean <- names(tmp_bin)
the_formula_clean <- as.formula(paste("finals ~", paste(n[!n %in% c("finals", "season", "round", "kicks")], 
                                                  collapse = " + ")))

mod2 <- glm(formula = the_formula_clean,
           data = tmp_bin,
           family = "binomial")

# Linearity

probabilities <- predict(mod2, type = "response")

mydata <- tmp_bin %>%
  dplyr::select(c(contested_marks, clearances, inside_50s, contested_possessions, marks,
                  behinds, handballs, goals))

predictors <- colnames(mydata)

mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  facet_wrap(~predictors, scales = "free_y") +
  my_theme

#----------------
# OUTPUT PLOTTING
#----------------

CairoPNG("output/logit-outputs.png", 800, 600)
plot_model(mod2, vline.color = "slategray1", sort.est = TRUE, show.values = TRUE,
           value.offset = .3) +
  labs(title = "Logistic regression coefficient outputs",
       subtitle = "Model predicted whether a game was in finals (1) or regular season (0). CI for odds ratio > 1 indicates as the variable increases the\nodds of a game being a final increases.",
       x = "Variable",
       y = "Odds Ratios",
       caption = "Source: CRAN package fitzRoy which pulls data from www.afltables.com\nAnalysis: Orbisant Analytics.")
dev.off()

#---------------------- DATA VISUALISATION -------------------------

tmp3 <- tmp2 %>%
  gather(key = variable, value = score, 4:12) %>%
  mutate(variable = str_to_sentence(variable)) %>% # Clean up names to read nicely
  mutate(variable = gsub("_", " ", variable))

p <- tmp3 %>%
  ggplot(aes(x = score)) +
  geom_density(aes(fill = finals), alpha = 0.4) +
  labs(title = "Distributions of average AFL metrics per game for finals vs regular season",
       x = "Average per Game",
       y = "Density",
       fill = NULL,
       caption = "Source: CRAN package fitzRoy which pulls data from www.afltables.com\nAnalysis: Orbisant Analytics.") +
  scale_fill_manual(values = c("#FEB06A", "steelblue2")) +
  facet_wrap(~variable, scales = "free") +
  my_theme
print(p)

# Export

CairoPNG("output/finals-distributions.png", 800, 600)
print(p)
dev.off()
