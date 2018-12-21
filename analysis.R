#Loading the required libraries

library(forecast)
library(tseries)
require(graphics)


#loading the file into R
load("rdas/rawdata.rda") 

nrow(rawdata)

#Let's create the model using the first 71 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser <- ts(rawdata$Price)
indata <- rawdata[1:29,]
timeser <- ts(indata$Price)
plot(timeser)
ggsave("figs/timeser.png")

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)
                         
                         
                        # width= window
                        # method convolution is moving average
                        # sides=2, is a two sided filter

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Months
lines(smoothedseries, col="blue", lwd=2)
ggsave("figs/smoothedtimeser.png")

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Months', 'Price')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Price ~ sin(0.5*Months) * poly(Months,3) + cos(0.5*Months) * poly(Months,3)
            + Months, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)
ggsave("figs/global_pred.png")

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
ggsave("figs/local_pred.png")
acf(local_pred)
ggsave("figs/acf_plot.png")
acf(local_pred, type="partial")
ggsave("figs/pacf_plot.png")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
ggsave("figs/tsdiag_armafit_plot.png")
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- rawdata[30:32,]
timevals_out <- outdata$Months

global_pred_out <- predict(lmfit,data.frame(Months =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
ggsave("figs/true_vs_pred_classical_decomposition_plot.png")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
ggsave("figs/tsdiag_autoarima.png")
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
ggsave("figs/true_pred_autoarima.png")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 3)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")
lines(fcast_auto_arima$pred, col = "blue")

ggsave("figs/ts_autoarima_compare_plot.png")

