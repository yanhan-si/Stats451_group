# Data Visualization
# Author: Yanhan Si
# Created by: Nov 23, 2020 (GMT-5)
# Updated by: Nov 23, 2020 (GMT-5)
# --------------------------------------------------------------------------------
library(tidyverse)
library(fBasics)

bitcoin = read_csv("./data/bitcoin.csv")
logR = bitcoin$log_return
R = bitcoin$ret
summary(R)
summary(logR)
sd(logR)
skewness(logR)
kurtosis(logR)
hist(logR)
boxplot(logR)

HMM_pred <- read_csv("data/HMM_pred_price.csv")
SVM_pred <- read_csv("data/SVM_pred_price.csv")
GARCH_pred <- read_csv("data/GARCH11_pred_price.csv")
training = read_csv("./data/bitcoin_train.csv")
train_log_ret = mean(training$log_return)
# Calculate the mean squared error
HMM = sum((SVM_pred$y_true - HMM_pred$y_pred)^2) / 7 # 3056606
SVM = sum((SVM_pred$y_true - SVM_pred$y_pred)^2) / 7 # 3109227
GARCH = sum((GARCH_pred$y_true - GARCH_pred$y_pred)^2) / 7 # 3113032
