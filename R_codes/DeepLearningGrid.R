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

my.model <- "Deep Learning with Grid Search"
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
  # Deep Learning with grid search
  hyper_params_dl <- list(
            activation = c("Rectifier", "Maxout", "Tanh",
              "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
            hidden = list(c(5, 5, 5, 5, 5), c(10, 10, 10, 10), c(50, 50, 50), 
                          c(100, 100, 100)),
            epochs = c(1000, 5000, 10000),
            l1 = c(0, 0.00001, 0.0001), 
            l2 = c(0, 0.00001, 0.0001),
            rate = c(0, 01, 0.005, 0.001),
            rate_annealing = c(1e-8, 1e-7, 1e-6),
            rho = c(0.9, 0.95, 0.99, 0.999),
            epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
            momentum_start = c(0, 0.5),
            momentum_stable = c(0.99, 0.5, 0),
            input_dropout_ratio = c(0, 0.1, 0.2),
            max_w2 = c(10, 100, 1000, 3.4028235e+38))
  
  search_criteria_dl <- list(strategy = "RandomDiscrete", 
                             max_models = 100,
                             max_runtime_secs = 60 * 15,
                             stopping_tolerance = 0.001,
                             stopping_rounds = 15,
                             stopping_metric = "RMSE",
                             seed = 2020)
  
  dl_grid <- h2o.grid(algorithm = "deeplearning",
                      search_criteria = search_criteria_dl,
                      hyper_params = hyper_params_dl,
                      x = x, y = y,
                      training_frame = train_h,
                      grid_id = "dl_grid",
                      seed = 1234)
  #nfolds = 3,
  
  dl2_grid_search <- h2o.getGrid(grid_id = "dl_grid",
                                 sort_by = "rmse",
                                 decreasing = FALSE)
  
  dl_grid_model <- h2o.getModel(dl2_grid_search@model_ids[[1]])
  
  test_h$yhat <- h2o.predict(dl_grid_model, test_h)
  
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

write_excel_csv(bench.df, "../Data/ML_grid_DL_benchmark.csv")
write_excel_csv(pred.df, "../Data/ML_grid_DL_predictions.csv")
