#------------------------------------------
# This script sets out to produce run a
# Bayesian ordinal regression to extract
# priors
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

# Define a reusable function
#' @param start_date start of season in format YYYY-MM-DD
#' @param end_date start of season in format YYYY-MM-DD
#' 
#' @author Trent Henderson
#' 

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

# Merge datasets

finals <- c("EF", "SF", "QF", "PF", "GF")
'%ni%' <- Negate('%in%')

tmp1 <- bind_rows(season_2011, season_2012, season_2013, season_2014, season_2015,
                  season_2016, season_2017, season_2018, season_2019, season_2020) %>%
  filter(round %ni% finals)

#---------------------- PREP DATA ----------------------------------

# Create variable for top players or not

the_best <- c("Lachie Neale", "Travis Boak", "Christian Petracca",
              "Jack Steele", "Patrick Dangerfield", "Dustin Martin",
              "Jack Macrae", "Luke Parker", "Cameron Guthrie",
              "Clayton Oliver", "Nat Fyfe", "Patrick Cripps",
              "Marcus Bontempelli")

tmp_players <- tmp1 %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  mutate(top_player = case_when(
         player_name %in% the_best ~ player_name,
         TRUE                      ~ "Not a Top Player")) %>%
  mutate(brownlow_votes = factor(brownlow_votes, 
                                 levels = c("0", "1", "2", "3", "4")))

#---------------------- PRIORS FOR TOP PLAYERS ---------------------

# Need to run a model for only the games that each top player was in
# SD = SE * sqrt(n)

# Run an ordinal regression to get some values

mod <- MASS::polr(brownlow_votes ~ top_player, 
                    data = tmp_players, 
                    Hess = TRUE)

summary(mod)

# Extract coefficients

player_priors <- as.data.frame(coef(summary(mod)))
player_priors <- rownames_to_column(player_priors, "top_player") %>%
  filter(agrepl("top_player", top_player)) %>%
  mutate(top_player = gsub("top_player", "\\1", top_player)) %>%
  dplyr::select(c(top_player, Value)) %>%
  rename()
