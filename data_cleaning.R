# Data Cleaning
# Author: Zhen Yang
# This bitcoin_dataset  contains info from 2010 to 2018. (Row: 2920)
# The bitcoin_price  contains info from 2013 to 2018. (Row: 1760)
# Created by: Nov 20, 2020
# Updated by: Nov 20, 2020
# --------------------------------------------------------------------------------

library(tidyverse)

# read data
directory = "./archive/"
bitcoin_price = read_csv(paste0(directory, "bitcoin_price.csv"))
bitcoin_dataset = read_csv(paste0(directory, "bitcoin_dataset.csv"))

# format the date
## This will give NA(s) in some locales; uncomment the code below.
## lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
bitcoin_price = bitcoin_price %>% mutate(Date = as.Date(Date, "%b %d, %Y"))
bitcoin_dataset = bitcoin_dataset %>% mutate(Date = as.Date(Date, "%b %d, %Y"))

# join two datasets by date
bitcoin = bitcoin_price %>% left_join(bitcoin_dataset, by = "Date")

# process missing values
# 1. Volume
## We notice that in bitcoin_price dataset, Volume is the only column 
## having missing values from 2013-04-28 to 2013-12-26; 
## uncomment the code below to see the missing rows.
## bitcoin_price %>% filter(is.na(Volume)) %>% arrange(Date) 

bitcoin[!complete.cases(bitcoin),]
bitcoin = bitcoin %>% filter(!is.na(Volume))

# 2. btc_trade_volume - undone
## Rows having missing btc_trade_volume are scattered from someday in 2014 to somday in 2015;
## For now, we do not process them and leave us with 13 missing values of btc_trade_volume;
## uncomment the code below to see the missing rows.
## bitcoin[!complete.cases(bitcoin), ] %>% arrange(Date)

# write output
write_csv(bitcoin, "bitcoin.csv")