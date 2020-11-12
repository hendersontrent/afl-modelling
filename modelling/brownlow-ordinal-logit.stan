//
// This Stan program builds a Bayesian ordinal regression that predicts
// votes for the Brownlow medal
//

//
// Author: Trent Henderson, 11 November 2020
//

data {
  int<lower=2> K; // number of Brownlow vote categories (0 votes, 1 vote, 2 votes, 3 votes)
  int<lower=1> N; // number of observations
  int<lower=1> D; // number of predictor variables
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
  int<lower=1,upper=K> y[N]; // response variable (Brownlow votes)
}

parameters {
  vector[D] beta; // regression coefficients
  ordered[K-1] c; // ensures outcome doesn't exceed the Brownlow vote range
}

model {
  for (n in 1:N)
    y[n] ~ ordered_logistic(beta[1] * win[n] + beta[2] * kicks[n] + beta[3] * marks[n], 
    beta[4] * goals[n] + beta[5] * behinds[n] + beta[6] * contest_possess[n], 
    beta[7] * contest_mark[n] + beta[8] * time_on_ground[n] + beta[9] * inside_50s[n], 
    beta[10] * clearances[n] + beta[11] * handballs[n], c);
}