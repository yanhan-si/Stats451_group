# Data Visualization
# Author: Yanhan Si
# Created by: Nov 23, 2020 (GMT-5)
# Updated by: Nov 23, 2020 (GMT-5)
# --------------------------------------------------------------------------------
library(tidyverse)
library(fBasics)

bitcoin = read_csv("./data/bitcoin.csv")
logR = bitcoin$log_return

summary(logR)
sd(logR)
skewness(logR)
kurtosis(logR)
hist(logR)
boxplot(logR)
