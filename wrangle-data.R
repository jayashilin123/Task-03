

#loading the file into R
#murders <- read_csv("data/murders.csv") 

filename <- c("data/bitcoin_price_historical_data.csv")
ylab <- c("Price")
xlab <- c("Months from Jan 2015")
title <- c("Bitcoin_Price, Jan. 2015")
xcol <- c(1)
ycol <- c(2)

example <- 1
rawdata <- read.csv(filename[example])

save(rawdata, file = "rdas/rawdata.rda")
