// Normal model
//
data {
  int<lower=0> N;
  real X[N];
  real<lower=0> Sigma;
  int<lower=0> K;
}
//
parameters {
  // Define parameters to estimate
  real theta;
}
//
model {
  // Likelihood part of Bayesian inference
  theta ~ normal(75,10/sqrt(K));
  X ~ normal(theta, Sigma);
}
