# Data Visualization
# Author: Yanhan Si
# Created by: Nov 19, 2020 (GMT-5)
# Updated by: Nov 19, 2020 (GMT-5)
# --------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
# read data

bitcoin = read_csv("./data/bitcoin.csv")
names(bitcoin)
bitcoin %>% 
  ggplot(aes(x = Date, y = Close)) +
  geom_line(color = "darkblue") +
  theme_bw() 
bitcoin %>% ggplot(aes(x = Date, y = ret)) + geom_line()
bitcoin %>% ggplot(aes(x = Date, y = log_return)) + geom_line()

# plot the predicted bitcoin prices of the three models
# read in dataset
HMM_pred <- read_csv("data/HMM_pred_price.csv")
SVM_pred <- read_csv("data/SVM_pred_price.csv")
GARCH_pred <- read_csv("data/GARCH11_pred_price.csv")
HMM_pred = HMM_pred %>% rename(HMM = y_pred)
SVM_pred = SVM_pred %>% rename(SVM = y_pred) %>% select(SVM)
GARCH_pred = GARCH_pred %>% rename(GARCH = y_pred, `True price` = y_true)
plot_df = cbind(GARCH_pred, SVM_pred, HMM_pred)

vars = names(plot_df)[-1]

plot_df %>% pivot_longer(
  cols = all_of(vars), 
  names_to = "Labels"
) %>% 
  ggplot(aes(x = date, y= value, color = Labels)) + 
    geom_line() + 
    xlab('Date') +
    ylab('Price (dollars)') +
    ggtitle("Bitcoin Daily Price") + 
    theme_bw()
