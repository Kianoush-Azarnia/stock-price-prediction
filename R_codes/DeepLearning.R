#----
# read csv, rename columns, change date data type to date
sel_stocks_df = read.csv(
  "../Data/selected_stocks_with_indicators.csv", encoding = "UTF-8")

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "X.U.FEFF.persian_symbol"
] <- "persian_symbol"

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "pd_trade_date"
] <- "date"

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "next_change"
] <- "y"

colnames(sel_stocks_df)

sel_stocks_df$date = as.Date(sel_stocks_df$date)

str(sel_stocks_df)

regression.bench.df <- read.csv(
  "../Data/regression_benchmark.csv", encoding = "UTF-8")
#sufficient.trades.symbols[sample(nrow(sufficient.trades.symbols),5),]

selected.symbols <- regression.bench.df$X.U.FEFF.symbol

# set seed for producing repeatable random results
set.seed(123)
random.selected.symbols <- sample(selected.symbols, size = 1)
random.selected.symbols

#----
library(readr)
library(dplyr)
library(lubridate)
# h2o config
library(h2o)

stock.symbols <- random.selected.symbols

# trades <- select(df, -one_of(excluded_vars))
# excluded_vars <- c("Fuckkk", "market_value", "Chaikin_MF", "persian_symbol", "change")

bench.cols <- c("symbol", "model", "MAPE", "RMSE")
bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
names(bench.df) <- bench.cols

pred.cols <- c("symbol", "model", "date", "actual", "predicted", "MAPE")

pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))

names(pred.df) <- pred.cols

valid.size <- 20
train.size <- valid.size * 10

my.model <- "Deep Learning"
#----
for (sym.i in 1:length(stock.symbols)) {
  h2o.init(max_mem_size = "4G")
  
  stock.sym <- stock.symbols[[sym.i]]
  print(stock.sym)
  
  # temp dataframes
  temp.pred.df <- data.frame(matrix(nrow=1, ncol=length(pred.cols)))
  temp.bench.df <- data.frame(matrix(nrow=1, ncol=length(bench.cols)))
  
  # stock.df is symbol's df
  stock.df <- sel_stocks_df[
    sel_stocks_df$persian_symbol==stock.sym,]
  
  trade.num <- nrow(stock.df)
  stock.df$index <- 1:trade.num
  
  stock.df$trades_number_inverse <- stock.df$number_of_trades^(-1)
  
  # stock.df <- select(df, -one_of(excluded_vars))
  
  print(trade.num)
  day.index <- train.size + valid.size
  
  mape.list <- rep(0, day.index)
  
  train_df <- stock.df[(0):(trade.num - valid.size),]
  test_df <- stock.df[(trade.num - valid.size + 1):trade.num,]
  
  #----
  # h2o train, test, prediction and benchmarks datasets
  train_h <- as.h2o(train_df)
  test_h <- as.h2o(test_df)
  
  x <- colnames(train_df)
  y <- "y"
  print(train_df$y)
  #----
  # Deep Learning 
  # activation = Tanh, Tanh with dropout, Rectifier, Rectifier with dropout, 
  # Maxout, Maxout with dropout, default = Rectifier
  # ditribution = poisson, laplace, tweedie, gaussian, huber, gamma, quantile
  # tweedie_power = 1.5,
  dl_md <- h2o.deeplearning(x = x, y = y,
                            distribution = "gaussian",
                            activation = "Tanh",
                            epochs = 1000,
                            train_samples_per_iteration = -1,
                            reproducible = FALSE,
                            balance_classes = FALSE,
                            force_load_balance = FALSE,
                            seed = 2020,
                            score_training_samples = 0,
                            score_validation_samples = 0,
                            training_frame = train_h,
                            stopping_rounds = 0,
                            stopping_metric = "RMSE",
                            max_runtime_secs = 60 * 0.1)
  
  print(h2o.performance(dl_md))
  
  test_h$yhat <- h2o.predict(dl_md, test_h)
  
  symbol_list <- rep(stock.sym, valid.size)
  model_list <- rep(my.model, valid.size)
  tdate <- test_df$date
  actual.price <- as.data.frame(test_h$y)
  pred.price <- as.data.frame(test_h$yhat)
  mape.list <- as.data.frame(abs(actual.price - pred.price) / 
                               (actual.price + 0.000001))
  
  # print("-------------------------------------------")
  # print(length(symbol_list))
  # print(length(model_list))
  # print(length(tdate))
  # print((actual.price))
  # print((pred.price))
  # print(length(mape.list))
  # print("-------------------------------------------")
  
  temp.pred.df <- data.frame(
    symbol_list, model_list, tdate, actual.price, pred.price, mape.list
  )
  colnames(temp.pred.df) <- pred.cols
  pred.df <- rbind(pred.df, temp.pred.df)
  remove(temp.pred.df)
  
  mape_dl_mean <- mean(mape.list[[y]])
  rmse_dl_mean <- (sum((actual.price$y - pred.price$yhat)^2/valid.size))^(0.5)
  
  p <- c(stock.sym, mape_dl_mean, rmse_dl_mean)
  print(p)
  
  temp.bench.df <- data.frame(stock.sym, my.model, mape_dl_mean, rmse_dl_mean)
  colnames(temp.bench.df) <- bench.cols
  bench.df <- rbind(bench.df, temp.bench.df)
  remove(temp.bench.df)
  
  h2o.shutdown(prompt = FALSE)
  
  Sys.sleep(5)
}

write_excel_csv(bench.df, "../Data/ML_DL_benchmark.csv")
write_excel_csv(pred.df, "../Data/ML_DL_predictions.csv")

