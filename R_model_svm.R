library(loo)
library(rstan)
svm_setup <- rstan::stan_model(file = './Stan/model_svm.stan')
dat = read.csv("./data/bitcoin_train.csv")
y = dat$log_return
N = length(y)
stan_data <- list(y = y,N = N)

svm <- rstan::sampling(svm_setup, data = stan_data)

svm_fit <- rstan::extract(svm, permuted = TRUE)

plot(svm_fit$mu, type = "l")

# Updates needed for computing the log likelihood
# log_lik_1 <- extract_log_lik(garch11, merge_chains = FALSE)
# r_eff_1 <- relative_eff(exp(log_lik_1), cores = 2)
# loo_1 <- loo(log_lik_1, r_eff = r_eff_1, cores = 2)
# print(loo_1)
# plot(loo_1, main = "PSIS diagnostic plot( Seperate model )")