library(loo)
library(rstan)
svm_setup <- rstan::stan_model(file = './Stan/model_svm.stan')
dat = read.csv("./data/bitcoin_train.csv")
y = dat$log_return
N = length(y)
stan_data <- list(y = y,N = N)

svm <- rstan::sampling(svm_setup, data = stan_data, iter = 4000)

p1 = stan_plot(svm, pars = c("mu_r", "mu", "phi", "sigma")) +
  ggtitle("Stochastic Volatility Model")
p1
p2 = stan_trace(svm, pars = c("mu_r", "mu", "phi", "sigma")) +
  ggtitle("Stochastic Volatility Model")
p2


svm_fit <- rstan::extract(svm, permuted = TRUE)

plot(svm_fit$mu_r, type = "l")
mean(svm_fit$mu_r)

#  computing the log likelihood
log_lik <- extract_log_lik(svm, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 2)
loo <- loo(log_lik, r_eff = r_eff, cores = 2)
print(loo)
plot(loo, main = "Stochastic Volatility Model PSIS diagnostic plot")
