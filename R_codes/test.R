#----
# read csv, rename columns, change trade_date data type to date
sufficient_trades_ts_df = read.csv(
  "../Data/sufficient_trades_time_series.csv", encoding = "UTF-8")

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "X.U.FEFF.persian_symbol"
] <- "persian_symbol"

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "pd_trade_date"
] <- "trade_date"

colnames(sufficient_trades_ts_df)

sufficient_trades_ts_df$trade_date = as.Date(sufficient_trades_ts_df$trade_date)

str(sufficient_trades_ts_df)

#----
# plot a sample time series of final prices (symbol="آسیا")
library(dplyr)

prices_ts_example <- sufficient_trades_ts_df[
  sufficient_trades_ts_df$persian_symbol=="آسيا",
] %>% select("trade_date", "final_price")

tail(prices_ts_example)

myexample.ts <- ts(prices_ts_example$final_price, frequency = 1)

myexample.ts

plot(
  myexample.ts, xlab = "Day", ylab = "Final Price", 
  ylim = c(0, 50000), bty = "l",
)

#----
# plot above time series example in 2 shapes: with trend & zoom
# 1- with trend line
library("forecast")

myexample.lm <- tslm(myexample.ts ~ trend + I(trend^2))

# Invoking dev.off() to make RStudio open up a new graphics device 
# with default settings worked for me.
dev.off()

plot(myexample.ts, xlab = "time", ylab = 'final price',)
lines(myexample.lm$fitted , lwd = 2)

# 2- with zoom in 4 months
myexample.ts.zoom <- window(myexample.ts, start = 602, end = 616)

plot(myexample.ts.zoom, xlab = "time", ylab = 'final price',)

#----
# Forecasting in the validation period from 
# a quadratic trend model estimated from the training period
n.valid <- 3
n.train <- length(myexample.ts) - n.valid

train.ts <- window(myexample.ts, start = 600, end = n.train)
valid.ts <- window(myexample.ts, start = n.train+1, end = length(myexample.ts))

myexample.lm <- tslm(train.ts ~ trend + I(trend^2))
myexample.lm.pred <- 
  forecast(myexample.lm, h = n.valid, level = 0)

plot(
  myexample.lm.pred, ylab = "Ridership", xlab = "Time", 
  bty = "l", xaxt = "n", main = "", flty = 2
)
lines(myexample.lm$fitted, lwd = 2)
lines(valid.ts)

# Validation period results of applying a quadratic trend model
accuracy(myexample.lm.pred$mean, valid.ts)

# Time plot of forecast errors (residuals) for a quadratic trend model
names(myexample.lm.pred)
myexample.lm.pred$residuals
valid.ts - myexample.lm.pred$mean

# Histogram of forecast errors in the training period 
# from a quadratic trend model
hist(
  myexample.lm.pred$residuals, ylab = "frequency", 
  xlab = "forecast errors", bty = "l", main = "",
)

# To create a normal probability plot in R, 
# put the forecast error into the function qqnorm()
qqnorm(
  myexample.lm.pred$residuals, ylab = "frequency", 
  xlab = "forecast errors", bty = "l", main = "",
)

# To compute a percentile (also known as a quantile),
# use R’s quantile() function
quantile(myexample.lm.pred$residuals)

#----
# Point forecasts & 95% prediction intervals in the 
# validation period from a quadratic trend model.
# The R code for creating this plot is the same as 
# the code used for the first plot of the previous section,
# except that level = 95
n.valid <- 3
n.train <- length(myexample.ts) - n.valid

train.ts <- window(myexample.ts, start = 600, end = n.train)
valid.ts <- window(myexample.ts, start = n.train+1, end = length(myexample.ts))

myexample.lm <- tslm(train.ts ~ trend + I(trend^2))
myexample.lm.pred <- forecast(myexample.lm, h = n.valid, level = 95)

plot(
  myexample.lm.pred, ylab = "Ridership", xlab = "Time", 
  bty = "l", xaxt = "n", main = "", flty = 2
)
lines(myexample.lm$fitted, lwd = 2)
lines(valid.ts)

#----
# Prediction cones from 3 exponential smoothing models
library("forecast")
myexample.ts <- ts(prices_ts_example$final_price, frequency = 1)

myexample.ets.AAN <- ets(myexample.ts, model = "AAN")
myexample.ets.MMN <- ets(myexample.ts, model = "MMN", damped = FALSE)
myexample.ets.MMdN <- ets(myexample.ts, model = "MMN", damped = TRUE)
?forecast
myexample.ets.AAN.pred <- forecast(myexample.ets.AAN, h = 115, 
                                   level = c(80, 95),)

myexample.ets.MMN.pred <- forecast(myexample.ets.MMN, h = 115, 
                                   level = c(80, 95),)

myexample.ets.MMdN.pred <- forecast(myexample.ets.MMdN, h = 115, 
                                    level = c(80, 95),)

accuracy(myexample.ets.AAN.pred)[1,"MAE"]

# This command sets the plot window to show 1 row of 3 plots.
par(mfrow = c(1, 3))

plot(myexample.ets.AAN.pred, xlab = 'day', ylab = 'price')
plot(myexample.ets.MMN.pred, xlab = 'day', ylab = 'price')
plot(myexample.ets.MMdN.pred, xlab = 'day', ylab = 'price')

#----
# Use the naive() and snaive() functions in the forecast package to create 
# the naive and seasonal naive forecasts in the fixed validation period
fixed.nValid <- 3
fixed.nTrain <- length(myexample.ts) - fixed.nValid

train.ts <- window(myexample.ts, start = 600, end = fixed.nTrain)
valid.ts <- window(
  myexample.ts, start = fixed.nTrain + 1, end = fixed.nTrain + fixed.nValid,
)

naive.pred <- naive(train.ts, h = fixed.nValid)
snaive.pred <- snaive(train.ts, h = fixed.nValid)

# Use the accuracy function to find the predictive measures
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)


#----
# computing the predictive measures for roll-forward one-month-ahead forecasts
rm(list=ls())

library("forecast")
myexample.ts <- ts(prices_ts_example$final_price, frequency = 1)

fixed.nValid <- 3
fixed.nTrain <- length(myexample.ts) - fixed.nValid
stepsAhead <- 1

#rep(x, y): replicate x, y times
naive.error <- rep(0, fixed.nValid - stepsAhead + 1) 
naive.percent.error <- rep(0, fixed.nValid - stepsAhead + 1)

snaive.error <- rep(0, fixed.nValid - stepsAhead + 1) 
snaive.percent.error <- rep(0, fixed.nValid - stepsAhead + 1)

for(j in fixed.nTrain : (fixed.nTrain + fixed.nValid - stepsAhead)) {
  train.ts <- window(myexample.ts, start = 1, end = j)
  valid.ts <- window(myexample.ts, start = j + 1, end = j + stepsAhead)
  
  naive.pred <- naive(myexample.ts, h = stepsAhead)
  snaive.pred <- snaive(myexample.ts, h = stepsAhead)
  
  naive.error[j - fixed.nTrain + 1] <- valid.ts - naive.pred$mean[stepsAhead]
  naive.percent.error[j - fixed.nTrain + 1] <- 
    naive.error[j -fixed.nTrain + 1] / valid.ts
  
  snaive.error[j - fixed.nTrain + 1] <- valid.ts - snaive.pred$mean[stepsAhead]
  snaive.percent.error[j - fixed.nTrain + 1] <- 
    snaive.error[j -fixed.nTrain + 1] / valid.ts
}

print("naive")
mean(abs(naive.error)) # MAE
sqrt(mean(naive.error^2)) # RMSE
mean(abs(naive.percent.error))
naive.pred

print("snaive")
mean(abs(snaive.error))
sqrt(mean(snaive.error^2))
mean(abs(snaive.percent.error))
snaive.pred

#----
# write a sample dataframe
employee <- c('John Doe','کمال ملکی','Jolie Hope')
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))

employ.data <- data.frame(employee, salary, startdate)
names(employ.data) <- c("name", "salary", "strat date")

temp.df <- data.frame("سلطان عثمانی", "1000000", "1585-06-09")
names(temp.df) <- names(employ.data)

employ.data <- rbind(employ.data, temp.df)

tdf <- data.frame(matrix(nrow=0, ncol=3))
tdf <- data.frame("Johnny", "2000000", "2069-08-05")
colnames(tdf) <- c("name", "salary", "strat date")
employ.data <- rbind(employ.data, tdf)

#----
# ets test
models <- c("AAN", "AAZ", "ANN", "ANZ", "MAN", "MAZ", "MNN", "MNZ", "MMN", "MMZ")
ets(myexample.ts, model = "AMN")

