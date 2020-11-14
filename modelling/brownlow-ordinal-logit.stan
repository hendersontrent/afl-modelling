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
  int<lower=0,upper=K> y[N]; // response variable (Brownlow votes)
  row_vector[D] x[N]; // predictor variables
}

parameters {
  vector[D] beta; // regression coefficients
  ordered[K-1] c; // ensures outcome doesn't exceed the Brownlow vote range
}

model {
  for (n in 1:N){
    y[n] ~ ordered_logistic(x[n] * beta, c);
  }
}