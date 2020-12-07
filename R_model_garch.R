library(loo)
library(rstan)
library(tidyverse)
garch11_setup <- rstan::stan_model(file = './Stan/model_garch.stan')
dat = read.csv("./data/bitcoin_train.csv")
test = read.csv("./data/bitcoin_test.csv")
y = dat$log_return
N = length(y)
y_test = test$log_return
J = length(y_test)
stan_data <- list(N = N,  J = J, y = y,sigma1 = 0.01, y_test = y_test)

garch11 <- rstan::sampling(garch11_setup, data = stan_data)
p1 = stan_plot(garch11, pars = c("mu", "alpha0", "alpha1", "beta1")) +
  ggtitle("GARCH(1,1)")
p1
p2 = stan_trace(garch11,
                pars = c("mu", "alpha0", "alpha1", "beta1")) +
  ggtitle("GARCH(1,1)")
p2
garch11_fit <- rstan::extract(garch11, permuted = TRUE)

# get the mean log return
mean(garch11_fit$mu)

## Calculate the predicted prices
# get the posterior predicted log return
y_pred = apply(garch11_fit$ypred, 2, median)
y_true_log_return = test$log_return
# MSE of predicted log return 
sum((y_pred-y_true_log_return)^2)/7


# compute the predicted log price
y_pred_log_price = rep(NA, 7)
for (i in 1:7) {
  if (i == 1) {y_pred_log_price[i] = y_pred[i] + 9.059321}
  else {y_pred_log_price[i] =  y_pred_log_price[i - 1] + y_pred[i] } 
}
# compute the predicted price
y_pred_price = exp(y_pred_log_price)
y_true_price = test$Close
# plot the predicion
plot_df = tibble(date = as.Date(test$Date), y_pred = y_pred_price, y_true = y_true_price)
# save the prediction to csv files
write.csv(plot_df,"./data/GARCH11_pred_price.csv", row.names = FALSE)

ggplot() +
  geom_line(data = plot_df, aes(x = date, y = y_true), color = "blue") +
  geom_line(data = plot_df, aes(x = date, y = y_pred), color = "red") +
  xlab('Date') +
  ylab('Price (dollars)') +
  ggtitle("Bitcoin Closing Price") + theme_bw()
## 
mean(garch11_fit$mu)
# Compute the loglikelihood
log_lik <- extract_log_lik(garch11, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 2)
loo <- loo(log_lik, r_eff = r_eff, cores = 2)
print(loo)
plot(loo, main = "GARCH PSIS diagnostic plot")


# RMSE for the baseline model
mean((y_true_log_return - 0.005)^2)
# compute the predicted log price
y_pred_log_price_b = rep(NA, 7)
for (i in 1:7) {
  y_pred_log_price_b[i] = 0.005 + 9.059321
}
y_pred_price_b= exp(y_pred_log_price_b)
mean( (y_pred_price_b - y_true_price)^2 )


