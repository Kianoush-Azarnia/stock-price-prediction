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

# class(myexample.ts[[1]])

# run lag func from stats package (not dplyr) to work with ts objects:
lag.example <- stats::lag(myexample.ts, k = 10)

plot(
  myexample.ts, xlab = "Day", ylab = "Final Price", 
  ylim = c(0, 50000), main="compare ts with lag"
) #  bty = "l",
lines(lag.example, col = "red")
#----
# test loop of ets benchmark
library("forecast")
len = length(myexample.ts)
valid.size = 1
train.size = valid.size * 9
j = 20
for(j in 1 : (len - train.size - valid.size)) {
  train.ts <- window(myexample.ts, start = j , end = j + train.size)
  
  valid.ts <- window(myexample.ts, start = j + train.size + 1, 
                     end = j + train.size + valid.size)
  myexample.ts[j + train.size + valid.size]
  
  ets.fit <- ets(
    train.ts, model = "AAN"
  )
  # , alpha = sel.alpha, beta = sel.beta
  # gamma = sel.gamma, restrict = FALSE, 
  # allow.multiplicative.trend = TRUE,
  
  ets.pred <- forecast(ets.fit, h = valid.size)
  ets.pred$mean[[1]]
  is.ts(ets.pred$mean)
  ets.pred[1]
  
  ets.mae.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MAE"]
  ets.rmse.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"RMSE"]
  
  print(j)
  
}
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

class(prices_ts_example[j,]$final_price)

#----
# tbats benchmark function test
# read csv, rename columns, change trade_date data type to date
sufficient.trades.ts.df = read.csv(
  "../Data/sufficient_trades_time_series.csv", encoding = "UTF-8")

names(sufficient.trades.ts.df)[ 
  names(sufficient.trades.ts.df) == "X.U.FEFF.persian_symbol"
] <- "persian_symbol"

names(sufficient.trades.ts.df)[ 
  names(sufficient.trades.ts.df) == "pd_trade_date"
] <- "trade_date"

colnames(sufficient.trades.ts.df)

sufficient.trades.ts.df$trade_date = as.Date(sufficient.trades.ts.df$trade_date)

str(sufficient.trades.ts.df)

sufficient.trades.symbols <- read.csv(
  "../Data/symbols_with_sufficient_trades.csv", encoding = "UTF-8")

set.seed(123)

random.selected.symbols <- sufficient.trades.symbols[
  sample(nrow(sufficient.trades.symbols),20),]

random.selected.symbols

#----
library("forecast")
library(dplyr)
library(readr)

ets.models <- c("AAN", "ANN", "MAN", "MNN", "MMN")

validations <- c(1)

tbats.benchmark <- function(models, stock.symbols, trades) {
  
  bench.cols <- c("model", "symbol", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE")
  
  bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
  names(bench.df) <- bench.cols
  
  pred.cols <- c("symbol", "trade_date", "actual_final_price", "model", 
                 "predicted_price", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE")
  pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
  names(pred.df) <- pred.cols
  
    stock.sym <- stock.symbols[[1]]
    
    stock.df <- sufficient.trades.ts.df[
      sufficient.trades.ts.df$persian_symbol == stock.sym,
    ] %>% select("trade_date", "final_price")
    
    stock.ts <- ts(stock.df$final_price, frequency = 1)
    
    len <- length(stock.ts)
      
      valid.size <- 1
      train.size <- valid.size * 9
      
      ets.me.list <- rep(0, len - train.size - valid.size + 1)
      ets.mae.list <- rep(0, len - train.size - valid.size + 1) 
      ets.rmse.list <- rep(0, len - train.size - valid.size + 1)
      ets.mpe.list <- rep(0, len - train.size - valid.size + 1)
      ets.mape.list <- rep(0, len - train.size - valid.size + 1)
      ets.mase.list <- rep(0, len - train.size - valid.size + 1)
      
      for(j in 1 : (len - train.size - valid.size)) {
        day.index <- j + train.size + valid.size
        
        train.ts <- window(stock.ts, start = j , end = j + train.size)
        
        valid.ts <- window(stock.ts, start = j + train.size + 1, 
                           end = day.index)
        
        ets.fit <- tbats(
          train.ts
        )
        # print(ets.fit)
        
        ets.pred <- forecast(ets.fit, h = valid.size)
        # print(ets.pred)
        
        ets.me.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"ME"]
        ets.mae.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MAE"]
        ets.rmse.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"RMSE"]
        ets.mpe.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MPE"]
        ets.mape.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MAPE"]
        ets.mase.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MASE"]
        
        if(j%%40==0){print(j)}
        
        temp.pred.df <- data.frame(matrix(
          nrow=1, ncol=length(pred.cols)))
        
        tdate <- stock.df[day.index,]$trade_date
        actual.price <- valid.ts[[1]]
        pred.price <- ets.pred$mean[[1]]
        
        temp.pred.df <- data.frame(
          stock.sym, tdate, actual.price, sel.model, pred.price, 
          ets.me.list[[j]], ets.mae.list[[j]], ets.rmse.list[[j]],
          ets.mpe.list[[j]], ets.mape.list[[j]], ets.mase.list[[j]]
        )
        
        colnames(temp.pred.df) <- pred.cols
        
        pred.df <- rbind(pred.df, temp.pred.df)
        
      }
      
      p <- c(sel.model, stock.sym, mean(ets.mae.list), mean(ets.rmse.list))
      print(p)
      
      temp.bench.df <- data.frame(matrix(
        nrow=1, ncol=length(bench.cols)))
      
      temp.bench.df <- data.frame(
        sel.model, stock.sym, mean(ets.me.list), mean(ets.mae.list), 
        mean(ets.rmse.list), mean(ets.mpe.list), mean(ets.mape.list),
        mean(ets.mase.list)
      )
      
      colnames(temp.bench.df) <- bench.cols
      
      bench.df <- rbind(bench.df, temp.bench.df)

  write_excel_csv(pred.df, "../Data/tabats_test_predictions.csv")
  write_excel_csv(bench.df, "../Data/tabats_test_benchmark.csv")
  print("__________Finished__________")
}

tbats.benchmark(models = ets.models[1], 
                stock.symbols = random.selected.symbols[15], 
                trades = sufficient.trades.ts.df)


#----
# Use the tslm function (which stands for time series linear model) 
# with the formula train.ts ~ trend to produce a linear trend model
library(forecast)
train.ts <- window(myexample.ts, start = 501 , end = 590)
valid.ts <- window(myexample.ts, start = 591 , end = 600)

train.lm <- tslm(train.ts ~ trend)

# forecasting with linear regression
train.lm.pred <- forecast(train.lm, h = 3, level = 0)
len <- length(train.ts)
# lines(x=list(seq(len, len+10)) ,y=train.lm.pred)
plot(train.lm.pred, xlim=c(501,605), cex.main=0.85)

lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

# Summary of output from a linear regression model
summary(train.lm)

# accuracy of linear regression predictions
accuracy(train.lm.pred, valid.ts)

# the residuals time plot
plot(train.lm$residuals, col="red")


#----
# To fit an exponential trend, simply replace the output variable y with 
# log(y) and fit a linear regression
# We use "log" to denote natural logarithm (base e). In R, use function log()
library(forecast)
train.ts <- window(myexample.ts, start = 500 , end = 508)
valid.ts <- window(myexample.ts, start = 509 , end = 510)

train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = 3, level = 0)

len <- length(train.ts)

plot(train.lm.pred, xlim=c(500,522), ylim=c(7000, 10000), cex.main=0.85)

lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts, lwd=2)

# Summary of output from a linear regression model
summary(train.lm)

# accuracy of linear regression predictions
accuracy(train.lm.pred, valid.ts)

# the residuals time plot
plot(train.lm$residuals, col="red")


#----
# Exponential (and linear) trend fitted to the data
library(forecast)
train.ts <- window(myexample.ts, start = 589 , end = 597)
valid.ts <- window(myexample.ts, start = 598 , end = 600)
nValid <- 3

# To fit a model with linear, quadratic, sine, and cosine terms to the data 
# in R, we add three additional predictors to the linear trend model
train.lm.sin.trend <- tslm(
  train.ts ~ trend + I(trend^2) + I(sin(2*pi*trend/2)) + I(cos(2*pi*trend/2))
)
train.lm.sin.trend.pred <- forecast(train.lm.sin.trend, h = nValid, level = 0)

# polynomial 4
train.lm.poly4.trend <- tslm(train.ts ~ trend + I(trend^2) + I(trend^3) + 
                               I(trend^4))
train.lm.poly4.trend.pred <- forecast(train.lm.poly4.trend, h=nValid, level=0)

# quadratic trend prediction
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h=nValid, level=0)

# exponential trend prediction
train.lm.expo.trend <- tslm(train.ts ~ trend, lambda = 0)
train.lm.expo.trend.pred <- forecast(train.lm.expo.trend, h=nValid, level=0)

# linear trend prediction
train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h=nValid, level=0)

# green: original data, blue: exponential, black: linear, red = quadratic
plot(train.ts, lwd=1, col="green", xlim=c(589,601), ylim=c(38000,46000))
lines(train.lm.sin.trend$fitted.values, lwd=2, col="pink", lty=6)
lines(train.lm.sin.trend.pred$upper, lwd=4, , col="pink", lty=3)
# poly4 regression: brown
lines(train.lm.poly4.trend$fitted.values, lwd=2, col="orange", lty=5)
lines(train.lm.poly4.trend.pred$upper, lwd=4, col="orange", lty=3)
# quadratic regression: red
lines(train.lm.poly.trend$fitted.values, lwd=1, col="red", lty=1)
lines(train.lm.poly.trend.pred$upper, lwd=1, col="red", lty=1)
# exponential regression: blue
lines(train.lm.expo.trend.pred$fitted, lwd = 1, col = "blue")
lines(train.lm.expo.trend.pred$upper, lwd = 1, col = "blue")
# linear regression: black
lines(train.lm.linear.trend.pred$fitted, lwd = 1, col = "black", lty = 1)
lines(train.lm.linear.trend.pred$mean, lwd = 1, col = "black", lty = 1)
lines(valid.ts, col="green")

# Summary of output from fitting a quadratic trend
summary(train.lm.poly.trend)

summary(train.lm.poly4.trend)

plot(train.lm.expo.trend$residuals)

accuracy(train.lm.sin.trend.pred, valid.ts)

#----
# prophet example
library(prophet)
proph.df <- prices_ts_example

names(proph.df)[ 
  names(proph.df) == "trade_date"
] <- "ds"

names(proph.df)[ 
  names(proph.df) == "final_price"
] <- "y"

proph.model <- prophet(proph.df)

# make_future_dataframe : Make dataframe with future dates for forecasting.
# output : Dataframe that extends forward from the end of m$history 
# for the requested number of periods.
future <- make_future_dataframe(proph.model, periods = 20)
tail(future)

forecast <- predict(proph.model, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(proph.model, forecast)

prophet_plot_components(proph.model, forecast)

dyplot.prophet(proph.model, forecast)

proph.cv.df <- cross_validation(proph.model, horizon=1, units="days",)

performance_metrics(proph.cv.df, rolling_window=1)

#----
# TSstudio : Functions for Time Series Analysis and Forecasting. Provides a set 
# of tools for descriptive and predictive analysis of time series data
library(TSstudio)

# Let's start with ts_info and review the structure of the myexample.ts series
ts_info(myexample.ts)

ts_plot(myexample.ts, title = "a stock price time series", 
        Xtitle = "day", Ytitle = "price")

# We can get a deeper view of the series components by decomposing the series
# into its components and plotting them with the ts_decompose function:
# the data we want to use here, is USA monthly vehicle sales time series
data(USVSales)
ts_decompose(USVSales)
# if you use the myexample.ts series in the above function, you will see this
# error message : "time series has no or less than 2 periods"
# it means that your time series, has no seasonal cycles or less than 2 
# seasonal cycles. (This may not indicate that the data are not seasonal)

# To get a closer look at the seasonal component of the series, we will 
# subtract from the series and decompose the trend we discussed previously.
USVSales.detrend <- USVSales - decompose(USVSales)$trend
ts_plot(USVSales.detrend)

# use the ts_seasonal function to plot the box plot of the seasonal component 
# of the detrend series
ts_seasonal(USVSales.detrend, type = "box")

# We can zoom in on the relationship of the series with the last three seasonal 
# lags using the ts_lags function
ts_lags(myexample.ts, lags = c(1, 2, 3))

# We will use the ts_to_prophet function from the TSstudio package to transform 
# the series from a ts object into a data.frame, and the window function to 
# subset the series observations since day 500
df <- ts_to_prophet(window(USVSales, start = c(2010,1)))
names(df) <- c("date", "y")
head(df)

ts_plot(df, title = "US Total Monthly Vehicle Sales (Subset)",
        Ytitle = "Thousands of Units", Xtitle = "Year")

#----
# We will use the dplyr and lubridate packages to create trend, seasonal 
# component and series correlation features, as we can see in the following:
library(dplyr)
#library(tidyverse)
library(lubridate)

example.df <- prices_ts_example
# rename trade_date to date and final_price to y
names(example.df) <- c("date", "y")

example.df <- example.df %>% mutate(lag1 = lag(y, n = 1)) %>% 
  filter(!is.na(lag1))

# extract and add month & lag12 to example dataframe
# example.df <- example.df %>% mutate(month = factor(month(date, label = TRUE), 
#   ordered = FALSE), lag12 = lag(y, n = 12)) %>% filter(!is.na(lag12))

# We will then add the trend component and its second polynomial (trend squared)
example.df$trend <- 1:nrow(example.df)
example.df$trend_sqr <- example.df$trend ^ 2

# the h variable represents the forecast horizon
h <- 2
train_df <- example.df[(nrow(example.df) - h - 9):(nrow(example.df) - h),]
test_df <- example.df[(nrow(example.df) - h + 1):nrow(example.df), ]

df.cols <- colnames(example.df)
forecast_df <- data.frame(matrix(nrow=h, ncol=length(df.cols)))
colnames(forecast_df) <- df.cols

forecast_df$date <- test_df$date
forecast_df$trend <- test_df$trend
forecast_df$trend_sqr <- forecast_df$trend ^ 2
forecast_df$lag1 <- tail(df$y, 1)

# let's train the linear regression model and evaluate its performance with the 
# testing partitions
lr <- lm(y ~ date + lag1 + trend + trend_sqr, data = train_df)
summary(lr)
test_df$yhat <- predict(lr, newdata = test_df)

# We will use MAPE of LR to benchmark the performance of the ML models
mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)
mape_lr

#----
# Starting a h2o cluster
# The h2o package is based on the use of distributed and parallel computing in 
# order to speed up the compute time and be able to scale up for big data. 
# All of this is done on either in-memory (on the computer's internal RAM) or 
# parallel distributed processing clusters
library(h2o)

# load the package and then set the in-memory cluster with h2o.init function
# h2o.init allows you to set memory size of cluster with max_mem_size argument
h2o.init(max_mem_size = "3G")

# Any data that is used throughout the training and testing process of the 
# models by the h2o package must load to the cluster itself.

# The as.h2o function allows us to transform any data.frame object into a 
# h2o cluster
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)

# transform the forecast_df object (the future values of the series inputs) 
# into an h2o object, which will be used to generate, later.
forecast_h <- as.h2o(forecast_df)

# we will label the names of the dependent and independent variables:
x <- c("date", "lag1" ,"trend", "trend_sqr") 
#date variable not considered, trend used as index
y <- "y"

# The h2o.randomForest function from the h2o package provides the framework 
# for training and tuning the RF model
rf_md <- h2o.randomForest(training_frame = train_h, validation_frame = test_h,
          nfolds = 3, x = x, y = y, ntrees = 500, stopping_rounds = 10, 
          stopping_metric = "RMSE", score_each_iteration = TRUE, 
          stopping_tolerance = 0.0001, seed = 1234)
# The h2o.randomForest function returns an object with information about the 
# parameter settings of the model and its performance on the training set 
# (and validation, if used)

# The  h2o.varimp_plot function returns a plot with the ranking of the input 
# variables' contribution to the model performance using a scale between 0 & 1.
h2o.varimp_plot(rf_md)

# review the model summary
rf_md@model$model_summary

# The following plot demonstrates the learning process of the model as a 
# function of the number of trees
library(plotly)
tree_score <- rf_md@model$scoring_history$training_rmse

plot_ly(x = seq_along(tree_score),y = tree_score, type = "scatter", 
  mode = "line") %>% layout(title = "The Trained Model Score History",
         yaxis = list(title = "RMSE"), xaxis = list(title = "Num. of Trees"))

# use the h2o.predict function to predict the corresponding values of the 
# series on the testing partition
test_h$pred_rf <- h2o.predict(rf_md, test_h)
# transfer the h2o data frame to a data.frame object with the as.data.frame()
test_1 <- as.data.frame(test_h)

# calculate the MAPE score of the RF model on the test partition
mape_rf <- mean(abs(test_1$y - test_1$pred_rf) / test_1$y)

# use h2o.grid function for a grid search approach for tuning an ML model
# setting the search parameters
hyper_params_rf <- list(mtries = c(2, 3, 4),
                      sample_rate = c(0.632, 0.8, 0.95),
                      col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                      max_depth = c(seq(1, 30, 3)), min_rows = c(1, 2, 5, 10))

# stoping criteria
search_criteria_rf <- list(strategy = "RandomDiscrete", 
                        stopping_metric = "rmse", stopping_tolerance = 0.0001,
                        stopping_rounds = 10, max_runtime_secs = 60 * 20)

# After we set the search arguments for the h2o.grid function, we start search:
rf2 <- h2o.grid(algorithm = "randomForest", 
          search_criteria = search_criteria_rf, hyper_params = hyper_params_rf,
          x = x, y = y, training_frame = train_h, ntrees = 5000, nfolds = 3,
          grid_id = "rf_grid", seed = 1234)

# extract grid results, sort models by their RMSE score, & pull the lead model:
rf2_grid_search <- h2o.getGrid(grid_id = "rf_grid", sort_by = "rmse",
                      decreasing = FALSE)

rf_grid_model <- h2o.getModel(rf2_grid_search@model_ids[[1]])

# test the model on the testing partition and evaluate its performance:
test_h$rf_grid <- h2o.predict(rf_grid_model, test_h)

# transfer the h2o data frame to a data.frame object with the as.data.frame()
test_1 <- as.data.frame(test_h)

mape_rf_grid <- mean(abs(test_1$y - test_h$rf_grid) / test_1$y)
mape_rf_grid

# The following plot provides an additional view of the model's performance:
plot_ly(data = test_1) %>%
  add_lines(x = ~ date, y = ~y, name = "Actual") %>%
  add_lines(x = ~ date, y = ~ yhat, name = "Linear Regression", 
            line = list(dash = "dot")) %>% 
  add_lines(x = ~ date, y = ~ pred_rf, name = "Random Forest", 
            line = list(dash = "dash")) %>% 
  add_lines(x = ~ date, y = ~ rf_grid, name = "Random Forest (grid)", 
            line = list(dash = "dash")) %>% 
  layout(title = "Actual vs. Prediction (Random Forest)", 
         yaxis = list(title = "Thousands of Units"),
         xaxis = list(title = "Month"))


#----
# GBM model

# Starting a h2o cluster
library(h2o)
h2o.init(max_mem_size = "3G")
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)
x <- c("date", "lag1" ,"trend", "trend_sqr") 
y <- "y"