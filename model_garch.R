# Data Modeling(GARCH)
# Code partially adopted from Statistics and Data Analysis for Financial 
# Engineering Chap 20, Ruppert and Matteson 
# Author: Yanhan Si
# Created by: Nov 20, 2020 (GMT-5)
# Updated by: Nov 20, 2020 (GMT-5)
# --------------------------------------------------------------------------------
library(rjags)
mod_string = "model
{
  for (t in 1:N)
  {
    y[t] ~ dt(mu, tau[t], nu)
    a[t] <- y[t] - mu
    tau[t] <- 1/h[t]
  }
  for (t in 2:N)
  {
    h[t] <- alpha0 + alpha1 * pow(a[t-1], 2) + beta1 * h[t-1]
  }
  mu ~ dnorm(0, 0.001)
  h[1] ~ dunif(0, 0.0012)
  alpha0 ~ dunif(0, 0.2)
  alpha1 ~ dunif(0.00001, 0.8)
  beta1 ~ dunif(0.00001, 0.8)
  nu ~ dunif(1,30)
}"

dat = read.csv("bitcoin_train.csv")
y = dat$log_return
N = length(y)
data=list(y=y,N=N)
inits_garch11 = function(){list(alpha0 = runif(1, 0.001, 0.25),
                                  beta1 = runif(1, 0.001, 0.25),
                                mu = runif(1, 0.001, 0.25),
                                  alpha1 = runif(1, 0.001, 0.25), 
                                nu = runif(1, 2, 10))}
garch11 <- jags.model(textConnection(mod_string), data=data,
                        inits = inits_garch11,
                         n.chains = 3, n.adapt = 1000, quiet = FALSE)
nthin = 20
garch11.coda = coda.samples(garch11,c("mu", "beta1", "alpha0",
                                          "alpha1", "nu", "tau"), 100*nthin, thin = nthin)
dic.garch11 = dic.samples(garch11, 100*nthin, thin = nthin)
dic.garch11
diffdic(dic.garch11, dic.stochVol_ARMA11)
summ_garch11 = summary(garch11.coda)
head(summ_garch11[[1]])
tail(summ_garch11[[1]])

