//
// This Stan program builds a Bayesian ordinal regression that predicts
// votes for the Brownlow medal
//

//
// Author: Trent Henderson, 11 November 2020
//

data {
  int<lower=2> K; // number of Bronload vote categories (0 votes, 1 vote, 2 votes, 3 votes)
  int<lower=1> N; // number of observations
  int<lower=1> D; // number of predictor variables
  matrix[N, D] X; // model matrix of predictor variables
  int<lower=1,upper=K> y[N]; // response variable (Brownlow votes)
}

parameters {
  vector[D] beta; // regression coefficients
  ordered[K-1] c; // ensures outcome doesn't exceed the Brownlow vote range
}

model {
  for (n in 1:N)
    y[n] ~ ordered_logistic(beta * X[n], c);
}

// Generate predictions for new season using the constructed model above

generated quantities {
  
}