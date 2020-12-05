library(loo)
library(rstan)
library(tidyverse)
svm_setup <- rstan::stan_model(file = './Stan/model_svm.stan')
dat = read.csv("./data/bitcoin_train.csv")
test = read.csv("./data/bitcoin_test.csv")
y = dat$log_return
N = length(y)


y_test = test$log_return
J = length(y_test)

stan_data <- list(y = y,N = N, J =J)

svm <- rstan::sampling(svm_setup, data = stan_data, iter = 4000)

p1 = stan_plot(svm, pars = c("mu_r", "mu", "phi", "sigma")) +
  ggtitle("Stochastic Volatility Model")
p1
p2 = stan_trace(svm, pars = c("mu_r", "mu", "phi", "sigma")) +
  ggtitle("Stochastic Volatility Model")
p2


svm_fit <- rstan::extract(svm, permuted = TRUE)
## Calculate the predicted prices
# get the posterior predicted log return
y_pred = apply(svm_fit$ypred, 2, median)

# compute the predicted log price
y_pred_log_price = rep(NA, 7)
for (i in 1:7) {
  if (i == 1) {y_pred_log_price[i] = y_pred[i] + 9.059321}
  else {y_pred_log_price[i] =  y_pred_log_return[i - 1] + y_pred[i] } 
}
# compute the predicted price
y_pred_price = exp(y_pred_log_price)
y_true_price = test$Close
# plot the predicion
plot_df = tibble(date = as.Date(test$Date), y_pred = y_pred_price, y_true = y_true_price)
# save the prediction to csv files
write.csv(plot_df,"./data/SVM_pred_price.csv", row.names = FALSE)
ggplot() +
  geom_line(data = plot_df, aes(x = date, y = y_true), color = "blue") +
  geom_line(data = plot_df, aes(x = date, y = y_pred), color = "red") +
  xlab('Date') +
  ylab('Price (dollars)') +
  ggtitle("Bitcoin Closing Price") + theme_bw()
## 


# svm_fit$ypred
plot(svm_fit$mu_r, type = "l")
mean(svm_fit$mu_r)

#  computing the log likelihood
log_lik <- extract_log_lik(svm, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 2)
loo <- loo(log_lik, r_eff = r_eff, cores = 2)
print(loo)
plot(loo, main = "Stochastic Volatility Model PSIS diagnostic plot")
