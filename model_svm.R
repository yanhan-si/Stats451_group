# Data Modeling(Stochastic Volatility Models)
# Code partially adopted from Statistics and Data Analysis for Financial 
# Engineering Chap 20, Ruppert and Matteson 
# Author: Yanhan Si
# Created by: Nov 19, 2020 (GMT-5)
# Updated by: Nov 19, 2020 (GMT-5)
# --------------------------------------------------------------------------------
library(rjags)
mod_string = "model
{
   for (i in 1:N)
    {
       y[i] ~ dt(mu, tau[i], nu) 
    }
   logh[1] ~ dnorm(0, 1.0E-6)
   for(i in 2:N)
    {
       logh[i] ~ dnorm(beta0 + phi * logh[i-1] + theta * v[i-1], tau_v)
       v[i] <- logh[i] - beta0 + phi * logh[i-1] + theta * v[i-1]
       }
   for (i in 1:N)
     {
       tau[i] <- exp(-logh[i])
       h[i] <- 1/tau[i]
       }
 mu ~ dnorm(0.0, 1)
 beta0 ~ dnorm(0, 0.0001)
 phi ~ dnorm(0.4, 0.0001)
 theta ~ dnorm(0, 0.0001)
 tau_v ~ dgamma(0.01, 0.01)
 v[1] ~ dnorm(0, 0.001)
 nu ~ dunif(1,30)
 sigma_v <- 1 / sqrt(tau_v)
}"


dat = read.csv("bitcoin_train.csv")

y = dat$log_return
##### get initial estimates #####
N = length(y)
logy2 = log(y^2+1e-6) # Avoid zero

fitar = lm(logy2[2:N] ~ logy2[1:(N - 1)])

beta0Init = as.numeric(fitar$coef[1])
phiInit = as.numeric(fitar$coef[2])
sfitar = summary(fitar)
sfitar

tauInit = 1/sfitar$sigma^2

##### Set up for MCMC #####
N = length(y)
data=list(y=y,N=N)
inits_stochVol_ARMA11 = function(){list(mu = rnorm(1, mean = mean(y),
 sd = sd(y) / sqrt(N)), logh = log(y^2+1e-6),
 beta0 = runif(1, beta0Init * 1.5, beta0Init/1.5),
 phi = runif(1,phiInit / 1.5, phiInit * 1.5),
 tau_v = runif(1, tauInit / 1.5, tauInit * 1.5),
 theta = runif(1, -0.5, 0.5))}

stochVol_ARMA11 <- jags.model(textConnection(mod_string), data = data,
 inits = inits_stochVol_ARMA11,
 n.chains = 3, n.adapt = 1000, quiet = FALSE)

nthin = 20

stochVol_ARMA.coda = coda.samples(stochVol_ARMA11, c("mu", "beta0",
 "phi", "theta", "tau_v", "nu", "tau"), 100 * nthin, thin = nthin)
summ_stochVol_ARMA11 = summary(stochVol_ARMA.coda)
summ_stochVol_ARMA11
head(summ_stochVol_ARMA11[[1]], 8)
tail(summ_stochVol_ARMA11[[1]], 8)
dic.stochVol_ARMA11 = dic.samples(stochVol_ARMA11, 100 * nthin,
thin = nthin, type = "pD")
dic.stochVol_ARMA11
effectiveSize(stochVol_ARMA.coda)

