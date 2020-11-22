#----
# read csv, rename columns, change trade_date data type to date
sufficient_trades_ts_df <- read.csv(
  "../Data/sufficient_trades_time_series.csv", encoding = "UTF-8")

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "X.U.FEFF.persian_symbol"
] <- "persian_symbol"

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "pd_trade_date"
] <- "date"

names(sufficient_trades_ts_df)[ 
  names(sufficient_trades_ts_df) == "final_price"
] <- "y"

colnames(sufficient_trades_ts_df)

sufficient_trades_ts_df$date <- as.Date(sufficient_trades_ts_df$date)

str(sufficient_trades_ts_df)

# set seed for producing repeatable random results
set.seed(123)

random.selected.symbols <- sufficient.trades.symbols[
  sample(nrow(sufficient.trades.symbols),20),]

random.selected.symbols
#----
# multi variable regression benchmark
library(readr)

library(dplyr)
library(lubridate)

stock.symbols <- random.selected.symbols
trades <- sufficient_trades_ts_df
bench.cols <- c("symbol", "MAPE", "RMSE")

bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
names(bench.df) <- bench.cols

pred.cols <- c("symbol", "date", "actual", "predicted", "MAPE", "RMSE")

pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))

names(pred.df) <- pred.cols

valid.size <- 1
train.size <- valid.size * 9

for (sym.i in 1:length(stock.symbols)) {
  stock.sym <- stock.symbols[[sym.i]]
  print(stock.sym)
  
  stock.df <- sufficient_trades_ts_df[
      sufficient_trades_ts_df$persian_symbol==stock.sym,]
  
  stock.df$index <- 1:nrow(stock.df)
  
  stock.df$trades_number_inverse <- stock.df$number_of_trades^(-1)
  
  stock.df.cols <- colnames(stock.df)

  print(nrow(stock.df))
  reg.mape.list <- rep(0, nrow(stock.df) - train.size - valid.size)
  reg.rmse.list <- rep(0, nrow(stock.df) - train.size - valid.size)
  
  for(j in 1 : (nrow(stock.df) - train.size - valid.size)) {
    
    day.index <- j + train.size + valid.size
    
    train_df <- stock.df[j:j+train.size,]
    test_df <- stock.df[day.index:day.index,]
    # print("check")
    # print(nrow(test_df))
    
    lr <- lm(y ~ index + trades_number_inverse + volume + value + min_price 
             + max_price, data = train_df)
    
    test_df$yhat <- predict(lr, newdata = test_df)
    
    actual.price <- test_df$y
    tdate <-test_df$date
    pred.price <- test_df$yhat
    reg.mape.list[[j]] <- abs(actual.price - pred.price) / 
      (actual.price + 0.000001)
    reg.rmse.list[[j]] <- (sum((actual.price - pred.price)^2/valid.size))^(0.5)
    
    if(j%%40==0){print(j)}
    
    temp.pred.df <- data.frame(matrix(
      nrow=1, ncol=length(pred.cols)))
    
    temp.pred.df <- data.frame(
      stock.sym, tdate, actual.price, pred.price, reg.mape.list[[j]], 
      reg.rmse.list[[j]]
    )
    colnames(temp.pred.df) <- pred.cols
    
    pred.df <- rbind(pred.df, temp.pred.df)
    
    remove(temp.pred.df)
    
  }
  mape_lr_mean <- mean(reg.mape.list)
  rmse_lr_mean <- mean (reg.rmse.list)
  p <- c(stock.sym, mape_lr_mean, rmse_lr_mean)
  print(p)
  
  temp.bench.df <- data.frame(matrix(
    nrow=1, ncol=length(bench.cols)))
  
  temp.bench.df <- data.frame(stock.sym, mape_lr_mean, rmse_lr_mean)
  colnames(temp.bench.df) <- bench.cols
  
  bench.df <- rbind(bench.df, temp.bench.df)
  
}
write_excel_csv(bench.df, "../Data/multi_var_regression_benchmark.csv")
write_excel_csv(pred.df, "../Data/multi_var_regression_predictions.csv")
