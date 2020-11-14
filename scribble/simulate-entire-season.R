#------------------------------------------
# This script sets out to simulate an
# entire season of AFL matches using
# joint probabilities
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

# Pull data for the last 5 seasons

season_2016 <- pull_afl_data(start_date = "2016-01-01", end_date = "2016-12-01")
season_2017 <- pull_afl_data(start_date = "2017-01-01", end_date = "2017-12-01")
season_2018 <- pull_afl_data(start_date = "2018-01-01", end_date = "2018-12-01")
season_2019 <- pull_afl_data(start_date = "2019-01-01", end_date = "2019-12-01")

# Merge datasets

tmp1 <- bind_rows(season_2016, season_2017, season_2018, season_2019)

#----------------------- PRE PROCESSING ----------------------------

# List of all teams

the_teams <- unique(tmp1$home_team)

# List of top teams

top_teams <- c("Richmond", "Port Adelaide", "Geelong", "Brisbane", "West Coast")

# Not in function

'%ni%' <- Negate('%in%')

#---------------- SIMULATE AN AFL SEASON ---------------------------

#' @param nsim number of simulated seasons to run
#' @param seed sets the seed of the machine for reproducibility
#' @param data dataset of team probabilities
#' @param ... additional parameters to pass to function
#' 
#' @author Trent Henderson
#' 

simulate_entire_season <- function(nsim = 10000, seed = 123, data = data, ...){
  
  message(paste0("Simulating an AFL season over ",nsim," iterations. This may take a few moments to calculate."))
  
  stored_probs <- list()
  for(i in the_teams){
    
    # Calculate the probability means for each scenario
    # Scenarios are:
    # * Focus team is home vs top away
    # * Focus team is home vs not top away
    # * Focus team is away vs top home
    # * Focus team is away vs not top home
    
    probs_for_current_team <- data %>%
      filter(home_team == i | away_team == i) %>%
      mutate(scenario = case_when(
        home_team == i & away_team %in% top_teams ~ "Home vs Top Team",
        home_team == i & away_team %ni% top_teams ~ "Home vs Not Top Team",
        away_team == i & home_team %in% top_teams ~ "Away vs Top Team",
        away_team == i & home_team %ni% top_teams ~ "Away vs Not Top Team")) %>%
      mutate(winner = case_when(
        home_score > away_score ~ home_team,
        away_score > home_score ~ away_team,
        TRUE                    ~ "Remove")) %>%
      mutate(current_team = case_when(
             playing_for == i ~ i,
             TRUE             ~ "Not")) %>%
      mutate(did_i_win = case_when(
        current_team == winner ~ 1,
        current_team != winner ~ 0,
        winner == "Remove"     ~ -1)) %>%
      group_by(scenario) %>%
      summarise(counter = mean(did_i_win)) %>%
      ungroup() %>%
      mutate(team = i)
    
    stored_probs[[i]] <- probs_for_current_team
  }
  
  the_probs_home <- rbindlist(stored_probs, use.names = TRUE) %>%
    rename(home_team = team) %>%
    rename(counter_home = counter)
  
  the_probs_away <- rbindlist(stored_probs, use.names = TRUE) %>%
    rename(away_team = team) %>%
    rename(counter_away = counter)
  
  # Generic list of fixtures to simulate
  
  fixtures <- data %>%
    distinct(home_team, away_team) %>%
    mutate(indicator = case_when(
           home_team == away_team ~ "Same",
           TRUE                   ~ "Different")) %>%
    filter(indicator != "Same") %>%
    mutate(fixture = row_number())
  
  # Run the actual simulation
  
  sim_list <- list()
  
  set.seed(seed)
  
  for(n in 1:nsim){
    tmp_sim <- fixtures %>%
      mutate(scenario_home = case_when(
        away_team %in% top_teams ~ "Home vs Top Team",
        away_team %ni% top_teams ~ "Home vs Not Top Team")) %>%
      mutate(scenario_away = case_when(
        home_team %in% top_teams ~ "Away vs Top Team",
        home_team %ni% top_teams ~ "Away vs Not Top Team")) %>%
      inner_join(the_probs_home, by = c("home_team" = "home_team",
                                       "scenario_home" = "scenario")) %>%
      inner_join(the_probs_away, by = c("away_team" = "away_team",
                                       "scenario_away" = "scenario")) %>%
      mutate(home_team_win = rbinom(1, 10000, counter_home),
             away_team_win = rbinom(1, 10000, counter_away)) %>%
      mutate(sim = n)
    
    sim_list[[n]] <- tmp_sim
  }
  
  the_sims <- rbindlist(sim_list, use.names = TRUE)
  
  # Final calculation of most frequent outcome for each fixture
  
  sim_results <- the_sims %>%
    mutate(winner = case_when(
           home_team_win > away_team_win ~ home_team,
           away_team_win > home_team_win ~ away_team,
           TRUE                          ~ "Draw")) %>%
    group_by(fixture, winner) %>%
    summarise(counter = n()) %>%
    group_by(fixture) %>%
    slice(which.max(counter)) %>%
    group_by(winner) %>%
    summarise(total_wins = n()) %>%
    ungroup() %>%
    arrange(desc(total_wins))
  
  return(sim_results)
  
}

#----------------------
# EXECUTE THE FUNCTION
#----------------------

outputs <- simulate_entire_season(nsim = 10000, seed = 123, data = tmp1)
