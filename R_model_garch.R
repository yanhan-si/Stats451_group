library(loo)
library(rstan)
garch11_setup <- rstan::stan_model(file = './Stan/model_garch.stan')
dat = read.csv("./data/bitcoin_train.csv")
y = dat$log_return
N = length(y)
stan_data <- list(y = y,N = N,sigma1 = 0.01)

garch11 <- rstan::sampling(garch11_setup, data = stan_data)

garch11_fit <- rstan::extract(garch11, permuted = TRUE)

plot(garch11_fit$alpha1, type = "l")
plot(garch11_fit$mu, type = "l")

# Updates needed for computing the log likelihood
# log_lik_1 <- extract_log_lik(garch11, merge_chains = FALSE)
# r_eff_1 <- relative_eff(exp(log_lik_1), cores = 2)
# loo_1 <- loo(log_lik_1, r_eff = r_eff_1, cores = 2)
# print(loo_1)
# plot(loo_1, main = "PSIS diagnostic plot( Seperate model )")