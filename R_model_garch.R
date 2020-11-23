library(loo)
library(rstan)
garch11_setup <- rstan::stan_model(file = './Stan/model_garch.stan')
dat = read.csv("./data/bitcoin_train.csv")
test = read.csv("./data/bitcoin_test.csv")
y = dat$log_return
N = length(y)
y_test = test$log_return
J = length(y_test)
stan_data <- list(y = y,N = N,sigma1 = 0.01, J = J, y_test = y_test)

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
# Compute the loglikelihood
log_lik <- extract_log_lik(garch11, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 2)
loo <- loo(log_lik, r_eff = r_eff, cores = 2)
print(loo)
plot(loo, main = "GARCH PSIS diagnostic plot")
