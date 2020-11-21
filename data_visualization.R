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
