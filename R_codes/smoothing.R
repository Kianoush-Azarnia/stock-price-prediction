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
library(dplyr)

prices_ts_example <- sufficient_trades_ts_df[
  sufficient_trades_ts_df$persian_symbol=="آسيا",
] %>% select("trade_date", "final_price")

tail(prices_ts_example)

myexample.ts <- ts(prices_ts_example$final_price, frequency = 1)

myexample.ts

#----
# Install and load the zoo package. 
# Its rollmean() function will calculate a trailing moving average.
# The function ma() in the forecast package creates a centered moving average.
library("zoo")
library("forecast")

ma.trailing <- rollmean(myexample.ts, k = 7, align = "right")
ma.centered <- ma(myexample.ts, order = 7)

plot(
  myexample.ts, ylab = "Price", xlab = "Day", bty = "l", xaxt = "n", main = ""
)

lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)

#----
# Trailing moving average forecaster with w = 7
library("zoo")

n.valid <- 7
n.train <- 14
mylen <- length(myexample.ts)

train.ts <- window(
  myexample.ts, start = mylen - n.valid - n.train, end = mylen - n.valid
)
valid.ts <- window(myexample.ts, start = mylen - n.valid + 1, end = mylen)

ma.trailing <- rollmean(train.ts, k = 7, align = "right")

last.ma <- tail(ma.trailing, 1)
# Trailing moving average forecaster with w = 7
ma.trailing.pred <- ts(rep(last.ma, n.valid), 
    start = mylen - n.valid + 1, end = mylen, frequency = n.valid)

# This command sets the plot window to show 1 row of 1 plots.
par(mfrow = c(1, 1))

plot(train.ts, ylab = "price", xlab = "day", bty = "l", main = "", 
     xlim = c(600, 616), ylim = c(29000, 44000))

lines(ma.trailing, lwd = 2, col = "blue")
lines(ma.trailing.pred, lwd = 2, lty =2, col = "blue")

lines(seq(610, 616), valid.ts, col = "green")

#----
# differencing : removing seasonalirity & trend
# In R, this twice differencing of myexample time series is created by running:
# diff(diff(myexample.ts,lag = 12), lag = 1)
# This command sets the plot window to show 1 row of 2 plots.
par(mfrow = c(1, 2))
plot(train.ts)

train.ts.diff1 = diff(train.ts, lag = 1)
plot(train.ts.diff1)

#----
library("forecast")

# In R, forecasting using simple exponential smoothing (ses) can be done 
# via the ets() function in the forecast package
# comparison between exponential smoothing with alpha = 0.2 & estimated alpha:

example.diff <- diff(myexample.ts, lag = 1)

n.valid <- 7
n.train <- n.valid * 3
len <- length(example.diff)

train.ts <- window(example.diff, start = len - n.train - n.valid + 1, 
                   end = len - n.valid)
valid.ts <- window(example.diff, start = len - n.valid + 1, end = len)

# simple exponential smoothing with alpha = 0.2
ses <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = n.valid, level = 0)
ses.pred

ses2 <- ets(train.ts, model = "ANN", alpha = 0.1)
ses2.pred <- forecast(ses2, h = n.valid, level = 0)
ses2.pred

ses3 <- ets(train.ts, model = "ANN", alpha = 0.01)
ses3.pred <- forecast(ses2, h = n.valid, level = 0)
ses3.pred

par(mfrow = c(1, 3))

plot(ses2.pred, ylab = "price", xlab = "day", bty = "l", main = "alpha=0.1",)
lines(ses2.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

plot(ses.pred, ylab = "price", xlab = "day", bty = "l", main = "alpha=0.2",)
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

# simple exponential smoothing with estimated alpha
ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = n.valid, level = 0)

plot(ses.opt.pred, ylab = "price", xlab = "day", bty = "l", main = "estimated",)
lines(ses.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

# Comparison of performance of two simple exponential smoothing models
accuracy(ses.opt.pred, valid.ts)
accuracy(ses.pred, valid.ts)
accuracy(ses2.pred, valid.ts)
accuracy(ses3.pred, valid.ts)
# The last line provides a summary of the ses.opt model
ses.opt

#----
# Forecasts from the Multiplicative exponential smoothing
library("forecast")

n.valid <- 3
n.train <- n.valid * 4
len <- length(myexample.ts)
# comparison between (start = len - n.train - n.valid + 1) & (start = 1)
# first start has more RMSE on training set, but less on validation set
train.ts <- window(
  myexample.ts, start = len - n.train - n.valid + 1, end = len - n.valid
)
valid.ts <- window(myexample.ts, start = len - n.valid + 1, end = len)

hwin <- ets(train.ts, model = "MMN")
hwin.pred <- forecast(hwin, h = n.valid, level = 0)

par(mfrow = c(1, 1))
plot(hwin.pred, ylab="price", xlab="day", bty="l", flty=2, 
     ylim = c(25000,50000), main="ets(M,M,N) with alpha=0.99")
lines(hwin.pred$fitted, lwd=2, col="blue")
lines(valid.ts)
accuracy(hwin.pred, valid.ts)

# Summary of the multiplicative exponential smoothing model
hwin
hwin$states[1, ] # initial states
hwin$states[nrow(hwin$states), ] # final states

#----
# Automated model selection
library("forecast")

n.valid <- 3
n.train <- n.valid * 4
len <- length(myexample.ts)

train.ts <- window(
  myexample.ts, start = len - n.train - n.valid + 1, end = len - n.valid
)
valid.ts <- window(myexample.ts, start = len - n.valid + 1, end = len)

# Automated model selection
auto.ets <- ets(train.ts, restrict = FALSE, allow.multiplicative.trend = TRUE)
auto.ets.pred <- forecast(auto.ets, h = n.valid, level = 0)
accuracy(auto.ets.pred, valid.ts)

#----
# Forecasts from tbats (good RMSE)
library("forecast")

n.valid <- 3
n.train <- n.valid * 4
len <- length(myexample.ts)

train.ts <- window(
  myexample.ts, start = len - n.train - n.valid + 1, end = len - n.valid
)
valid.ts <- window(myexample.ts, start = len - n.valid + 1, end = len)

example.tbats <- tbats(train.ts)
example.tbats.pred <- forecast(example.tbats, h = n.valid)

plot(example.tbats.pred, xlab = "day", ylab = "price", main = "TBATS")
lines(valid.ts)

accuracy(example.tbats.pred, valid.ts)
?tbats

#----
