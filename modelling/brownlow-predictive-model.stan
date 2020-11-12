//
// This Stan program builds a Bayesian ordinal regression that predicts
// votes for the Brownlow medal from previous priors
//

//
// Author: Trent Henderson, 11 November 2020
//

data {
  
  // High level numbers
  
  int<lower=2> K; // number of Brownlow vote categories (0 votes, 1 vote, 2 votes, 3 votes)
  int<lower=1> N; // number of observations
  
  // Predictor variables
  
  vector[N] win;
  vector[N] kicks;
  vector[N] marks;
  vector[N] goals;
  vector[N] behinds;
  vector[N] contest_possess;
  vector[N] contest_mark;
  vector[N] time_on_ground;
  vector[N] inside_50s;
  vector[N] clearances;
  vector[N] handballs;
  
  // Response variable
  
  int<lower=1,upper=K> y[N]; // response variable (Brownlow votes)
  
  // Priors
  
  real win_mean;
  real kicks_mean;
  real marks_mean;
  real goals_mean;
  real behinds_mean;
  real contest_possess_mean;
  real contest_mark_mean;
  real time_on_ground_mean;
  real inside_50s_mean;
  real clearances_mean;
  real handballs_mean;
  
  real win_sd;
  real kicks_sd;
  real marks_sd;
  real goals_sd;
  real behinds_sd;
  real contest_possess_sd;
  real contest_mark_sd;
  real time_on_ground_sd;
  real inside_50s_sd;
  real clearances_sd;
  real handballs_sd;
}

parameters {
  real win_prior;
  real kicks_prior;
  real marks_prior;
  real goals_prior;
  real behinds_prior;
  real contest_possess_prior;
  real contest_mark_prior;
  real time_on_ground_prior;
  real inside_50s_prior;
  real clearances_prior;
  real handballs_prior;
  ordered[K-1] c; // ensures outcome doesn't exceed the Brownlow vote range
}

model {
  
  // Specify priors for predictor variables
  
  win_prior ~ normal(win_mean, win_sd);
  kicks_prior ~ normal(kicks_mean, kicks_sd);
  marks_prior ~ normal(marks_mean, marks_sd);
  goals_prior ~ normal(goals_mean, goals_sd);
  behinds_prior ~ normal(behinds_mean, behinds_sd);
  contest_possess_prior ~ normal(contest_possess_mean, contest_possess_sd);
  contest_mark_prior ~ normal(contest_mark_mean, contest_mark_sd);
  time_on_ground_prior ~ normal(time_on_ground_mean, time_on_ground_sd);
  inside_50s_prior ~ normal(inside_50s_mean, inside_50s_sd);
  clearances_prior ~ normal(clearances_mean, clearances_sd);
  handballs_prior ~ normal(handballs_mean, handballs_sd);
  
  // Compute actual model
  
  for (n in 1:N)
    y[n] ~ ordered_logistic(win_prior * win[n] + kicks_prior * kicks[n] + marks_prior * marks[n], 
    goals_prior * goals[n] + behinds_prior * behinds[n] + contest_possess_prior * contest_possess[n], 
    contest_mark_prior * contest_mark[n] + time_on_ground_prior * time_on_ground[n] + inside_50s_prior * inside_50s[n], 
    clearances_prior * clearances[n] + handballs_prior * handballs[n], c);
}

// Generate predictions for new season using the constructed model above

generated quantities {
  
}