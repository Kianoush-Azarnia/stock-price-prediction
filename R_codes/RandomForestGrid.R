#----
# read csv, rename columns, change date data type to date
sufficient_trades_ts_df = read.csv(
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

sufficient_trades_ts_df$date = as.Date(sufficient_trades_ts_df$date)

str(sufficient_trades_ts_df)

# set seed for producing repeatable random results
set.seed(123)

random.selected.symbols <- sufficient.trades.symbols[
  sample(nrow(sufficient.trades.symbols),5),]

random.selected.symbols

#----
library(readr)
library(dplyr)
library(lubridate)
# h2o config

stock.symbols <- random.selected.symbols
trades <- sufficient_trades_ts_df
bench.cols <- c("symbol", "model", "MAPE", "RMSE")

bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
names(bench.df) <- bench.cols

pred.cols <- c("symbol", "model", "date", "actual", "predicted", "MAPE")

pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))

names(pred.df) <- pred.cols

valid.size <- 20
train.size <- valid.size * 3

my.model <- "Random Forest with Grid Search"
#----
for (sym.i in 1:length(stock.symbols)) {
  library(h2o)
  h2o.init(max_mem_size = "4G")
  
  stock.sym <- stock.symbols[[sym.i]]
  print(stock.sym)
  
  # temp dataframes
  temp.pred.df <- data.frame(matrix(nrow=1, ncol=length(pred.cols)))
  temp.bench.df <- data.frame(matrix(nrow=1, ncol=length(bench.cols)))
  
  # stock.df is symbol's df
  stock.df <- sufficient_trades_ts_df[
    sufficient_trades_ts_df$persian_symbol==stock.sym,]
  
  trade.num <- nrow(stock.df)
  stock.df$index <- 1:trade.num
  
  stock.df$trades_number_inverse <- stock.df$number_of_trades^(-1)
  
  print(trade.num)
  day.index <- train.size + valid.size
  
  mape.list <- rep(0, day.index)
  
  train_df <- stock.df[(trade.num - day.index):(trade.num - valid.size),]
  test_df <- stock.df[(trade.num - valid.size + 1):trade.num,]
  
  #----
  # h2o train, test, prediction and benchmarks datasets
  train_h <- as.h2o(train_df)
  test_h <- as.h2o(test_df)
  
  x <- colnames(stock.df)
  y <- "y"
  
  #----
  # Random Forest with grid search
  hyper_params_rf <- list(mtries = c(2, 3, 4),
                          sample_rate = c(0.632, 0.8, 0.95),
                          col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                          max_depth = c(seq(1, 30, 3)),
                          min_rows = c(1, 2, 5, 10))
  
  search_criteria_rf <- list(strategy = "RandomDiscrete",
                             stopping_metric = "rmse",
                             stopping_tolerance = 0.0001,
                             stopping_rounds = 10,
                             max_runtime_secs = 60 * 5)
  
  rf_grid <- h2o.grid(algorithm = "randomForest",
                  search_criteria = search_criteria_rf,
                  hyper_params = hyper_params_rf,
                  x = x, y = y,
                  training_frame = train_h,
                  ntrees = 5000,
                  grid_id = "rf_grid",
                  seed = 1234)
                  #nfolds = 3,
  
  rf2_grid_search <- h2o.getGrid(grid_id = "rf_grid",
                                 sort_by = "rmse",
                                 decreasing = FALSE)
  
  rf_grid_model <- h2o.getModel(rf2_grid_search@model_ids[[1]])
  
  test_h$yhat <- h2o.predict(rf_grid_model, test_h)
  
  symbol_list <- rep(stock.sym, valid.size)
  model_list <- rep(my.model, valid.size)
  tdate <- test_df$date
  actual.price <- as.data.frame(test_h$y)
  pred.price <- as.data.frame(test_h$yhat)
  mape.list <- as.data.frame(abs(actual.price - pred.price) / 
                               (actual.price + 0.000001))
  
  temp.pred.df <- data.frame(
    symbol_list, model_list, tdate, actual.price, pred.price, mape.list
  )
  colnames(temp.pred.df) <- pred.cols
  pred.df <- rbind(pred.df, temp.pred.df)
  remove(temp.pred.df)
  
  mape_rf_mean <- mean(mape.list[[y]])
  rmse_rf_mean <- (sum((actual.price$y - pred.price$yhat)^2/valid.size))^(0.5)
  
  p <- c(stock.sym, mape_rf_mean, rmse_rf_mean)
  print(p)
  
  temp.bench.df <- data.frame(stock.sym, my.model, mape_rf_mean, rmse_rf_mean)
  colnames(temp.bench.df) <- bench.cols
  bench.df <- rbind(bench.df, temp.bench.df)
  remove(temp.bench.df)
  
  h2o.shutdown(prompt = FALSE)
  
  Sys.sleep(20)
}

previous.bench.df <- read.csv("../Data/ML_benchmark.csv", encoding = "UTF-8")
names(previous.bench.df)[ 
  names(previous.bench.df) == "X.U.FEFF.symbol"
] <- "symbol"

previous.pred.df <- read.csv("../Data/ML_predictions.csv", encoding = "UTF-8")
names(previous.pred.df)[ 
  names(previous.pred.df) == "X.U.FEFF.symbol"
] <- "symbol"

bench.df <- rbind(previous.bench.df, bench.df)
pred.df <- rbind(previous.pred.df, pred.df)

write_excel_csv(bench.df, "../Data/ML_benchmark.csv")
write_excel_csv(pred.df, "../Data/ML_predictions.csv")

