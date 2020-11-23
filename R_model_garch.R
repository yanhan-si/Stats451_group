library(loo)
library(rstan)
garch11_setup <- rstan::stan_model(file = './Stan/model_garch.stan')
dat = read.csv("./data/bitcoin_train.csv")
y = dat$log_return
N = length(y)
stan_data <- list(y = y,N = N,sigma1 = 0.01)

garch11 <- rstan::sampling(garch11_setup, data = stan_data)
p1 = stan_plot(garch11, pars = c("mu", "alpha0", "alpha1", "beta1")) +
  ggtitle("GARCH(1,1)")
p1
p2 = stan_trace(garch11,
                pars = c("mu", "alpha0", "alpha1", "beta1")) +
  ggtitle("GARCH(1,1)")
p2
garch11_fit <- rstan::extract(garch11, permuted = TRUE)

mean(garch11_fit$mu)
# Updates needed for computing the log likelihood
# log_lik_1 <- extract_log_lik(garch11, merge_chains = FALSE)
# r_eff_1 <- relative_eff(exp(log_lik_1), cores = 2)
# loo_1 <- loo(log_lik_1, r_eff = r_eff_1, cores = 2)
# print(loo_1)
# plot(loo_1, main = "PSIS diagnostic plot( Seperate model )")