#------------------------------------------
# This script sets out to produce run a
# Bayesian ordinal regression to predict
# Brownlow winner
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

# Compute winner binary variable

tmp2 <- tmp1 %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE                    ~ "Remove")) %>%
  filter(winner != "Remove") %>%
  mutate(did_i_win = case_when(
    playing_for == winner ~ 1,
    TRUE                  ~ 0))

#---------------------- MODELLING ----------------------------------

# Set up all the variables and build a component list

K <- unique(tmp2$brownlow_votes)
N <- nrow(tmp2)
y <- tmp2$brownlow_votes
#win <- tmp2$did_i_win
#kicks <- tmp2$kicks
#marks <- tmp2$marks
#goals <- tmp2$goals
#behinds <- tmp2$behinds
#contest_possess <- tmp2$contested_possessions
#contest_mark <- tmp2$contested_marks
#time_on_ground <- tmp2$time_on_ground
#inside_50s <- tmp2$inside_50s
#clearances <- tmp2$clearances
#handballs <- tmp2$handballs

X <- tmp2 %>%
  dplyr::select(c(did_i_win, kicks, marks, goals, behinds, contest_possess, contest_mark,
                  time_on_ground, inside_50s, clearances, handballs))

stan_data <- list(K = K,
                  N = N,
                  y = y,
                  X = X)

#stan_data <- list(K = K,
#                  N = N,
#                  y = y,
#                  win = win,
#                  kicks = kicks,
#                  marks = marks,
#                  goals = goals,
#                  behinds = behinds,
#                  contest_possess = contest_possess,
#                  contest_mark = contest_mark,
#                  time_on_ground = time_on_ground,
#                  inside_50s = inside_50s,
#                  clearances = clearances,
#                  handballs = handballs)

# Run model and track time taken

system.time({
  mod <- stan(data = stan_data, 
              file = "modelling/brownlow-ordinal-logit.stan",
              iter = 1000,
              chains = 4,
              seed = 123)
})

#---------------------- OUTPUTS ------------------------------------


