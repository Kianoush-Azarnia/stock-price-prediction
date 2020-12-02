#----
# read csv, rename columns, change trade_date data type to date
sufficient_trades_ts_df <- read.csv(
  "../Data/sel_stocks_next_prices.csv", encoding = "UTF-8")

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "X.U.FEFF.persian_symbol"
] <- "persian_symbol"

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "pd_trade_date"
] <- "date"

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "next_price"
] <- "y"

colnames(sufficient_trades_ts_df)

sufficient_trades_ts_df$date <- as.Date(sufficient_trades_ts_df$date)

str(sufficient_trades_ts_df)

selected.symbols <- regression.bench.df$X.U.FEFF.symbol

# set seed for producing repeatable random results
set.seed(123)
random.selected.symbols <- sample(selected.symbols, size = 10)
random.selected.symbols
#----
# multi variable regression benchmark
library(readr)

library(dplyr)
library(lubridate)

stock.symbols <- random.selected.symbols
trades <- sufficient_trades_ts_df
bench.cols <- c("symbol", "model", "MAPE", "RMSE")

bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
names(bench.df) <- bench.cols

pred.cols <- c("symbol", "model", "date", "actual", "predicted", "MAPE", "RMSE")

pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))

names(pred.df) <- pred.cols

valid.size <- 20
train.size <- valid.size * 10

my.model <- "Multi Variate Regression as Base Model"

for (sym.i in 1:length(stock.symbols)) {
  stock.sym <- stock.symbols[[sym.i]]
  print(stock.sym)
  
  stock.df <- sufficient_trades_ts_df[
    sufficient_trades_ts_df$persian_symbol==stock.sym,]
  
  stock.df$index <- 1:nrow(stock.df)
  
  stock.df$trades_number_inverse <- stock.df$number_of_trades^(-1)
  
  stock.df.cols <- colnames(stock.df)
  
  print(nrow(stock.df))
  # reg.mape.list <- rep(0, valid.size)
  # reg.rmse.list <- rep(0, vvalid.size)
    
  trade.num <- nrow(stock.df)
  day.index <- train.size + valid.size
  
  shift <- 4
  train_df <- stock.df[(0):(trade.num - valid.size - shift),]
  test_df <- stock.df[(trade.num - valid.size + 1 - shift):(trade.num - shift),]
  # print("check")
  # print(nrow(test_df))
  
  lr <- lm(y ~ index + trades_number_inverse + volume + value + min_price 
           + max_price, data = train_df)
  
  test_df$yhat <- predict(lr, newdata = test_df)
  
  symbol_list <- rep(stock.sym, valid.size)
  model_list <- rep(my.model, valid.size)
  actual.price <- test_df$y
  tdate <-test_df$date
  pred.price <- test_df$yhat
  reg.mape.list <- abs(actual.price - pred.price) / 
    (actual.price + 0.000001)
  reg.rmse.list <- (sum((actual.price - pred.price)^2/valid.size))^(0.5)
  
  temp.pred.df <- data.frame(matrix(
    nrow=1, ncol=length(pred.cols)))
  
  temp.pred.df <- data.frame(
    symbol_list, model_list, tdate, actual.price, pred.price, reg.mape.list, 
    reg.rmse.list
  )
  colnames(temp.pred.df) <- pred.cols
  
  pred.df <- rbind(pred.df, temp.pred.df)
  
  remove(temp.pred.df)
    
  mape_lr_mean <- mean(reg.mape.list)
  rmse_lr_mean <- mean (reg.rmse.list)
  p <- c(stock.sym, my.model, mape_lr_mean, rmse_lr_mean)
  print(p)
  
  temp.bench.df <- data.frame(matrix(
    nrow=1, ncol=length(bench.cols)))
  
  temp.bench.df <- data.frame(stock.sym, my.model, mape_lr_mean, rmse_lr_mean)
  colnames(temp.bench.df) <- bench.cols
  
  bench.df <- rbind(bench.df, temp.bench.df)
  
}
write_excel_csv(bench.df, "../Data/base_model_multi_var_regression_benchmark.csv")
write_excel_csv(pred.df, "../Data/base_model_multi_var_regression_predictions.csv")
