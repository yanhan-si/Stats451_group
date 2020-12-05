//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> J;
  real y[N];
  real<lower=0> sigma1;
  real y_test[J];
}
parameters {
  real mu;
  real<lower=0> alpha0;
  real<lower=0,upper=1> alpha1;
  real<lower=0,upper=(1-alpha1)> beta1;
}
transformed parameters {
  
  real<lower=0> sigma[N];
  sigma[1] = sigma1;
  // Priors
  for (t in 2:N)
    sigma[t] = sqrt(alpha0
                     + alpha1 * pow(y[t-1] - mu, 2)
                     + beta1 * pow(sigma[t-1], 2));
}
model {
  y ~ normal(mu, sigma);
}

generated quantities {
  real y_rep[N] = normal_rng(mu, sigma);
  real log_lik[N];
  real<lower=0> spred[J];
  real ypred[J];
  for (i in 1:N) {log_lik[i] = normal_lpdf(y[i] | mu, sigma[N]);}

  spred[1] = sigma[N];
  ypred[1] = normal_rng(mu, spred[1]);
  for (t in 2:J) {
    spred[t] = sqrt(
      alpha0
      + alpha1 * pow(ypred[t-1] - mu, 2)
      + beta1 * pow(spred[t-1], 2)
    );
    ypred[t] = normal_rng(mu, spred[t]);

}
}
