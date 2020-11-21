library(loo)
library(rstan)
garch11_setup <- rstan::stan_model(file = 'model_garch.stan')
dat = read.csv("bitcoin_train.csv")

stan_data <- list(y = dat$log_return,N = length(y),sigma1 = 0.01)

garch11 <- rstan::sampling(garch11_setup, data = stan_data)

garch11_fit <- rstan::extract(garch11, permuted = TRUE)

plot(garch11_fit$alpha1, type = "l")
plot(garch11_fit$mu, type = "l")

