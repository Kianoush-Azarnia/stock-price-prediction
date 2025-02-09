#----
library("lubridate")
library("dplyr")
library("plotly")
library("forecast")

library("h2o")

library("shiny")
library("shinyjs")
# library("shinydashboard")
# library("shinycssloaders")

#----
# initialize data frame
trade_df <- read.csv("../Data/trades_dataset.csv", encoding = "UTF-8")

# drop unwanted columns
drops <- c("X.U.FEFF.trade_id", "persian_name", "trade_date", "market_value")
trade_df <- trade_df[ , !(names(trade_df) %in% drops)]

# rename columns
names(trade_df)[ 
  names(trade_df) == "persian_symbol"
] <- "symbol"

names(trade_df)[ 
  names(trade_df) == "pd_trade_date"
] <- "date"

# change date variable type from character to Date:
trade_df$date <- as.Date(trade_df$date)

# unique symbols
unique_symbols <- trade_df %>% count(symbol) %>% filter(n >= 30) %>% 
  select(symbol) %>% distinct()

# define start date and end date variables:
start_date <- (trade_df %>% select(date) %>% slice(which.min(date)))$date
end_date <- (trade_df %>% select(date) %>% slice(which.max(date)))$date

stat_models <- list(
  "linear regression" = "reg", 
  "multi-variable regression" = "multireg"
) # "exponential smoothing" = "ets", 

ml_models <- list(
  "deep learning" = "dl", 
  "deep learning grid" = "dlgrid"
) #"gradient boost machine" = "gbm", "random forest" = "rf",

ml_stopping_metric <- c("AUTO", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "AUCPR")

ets_models <- c("AAN", "ANN", "MAN", "MNN", "MMN")

true_false_list <- list("True" = TRUE, "False" = FALSE)

gbm_distributions <- c(
  "AUTO", "bernoulli", "quasibinomial", "multinomial", "gaussian", "poisson", 
  "gamma", "tweedie", "laplace", "quantile", "huber", "custom"
)
rf_distributions <- gbm_distributions

dl_loss <- c(
  "Automatic", "CrossEntropy", "Quadratic", "Huber", "Absolute", "Quantile"
)

dl_distribution = c(
  "AUTO", "bernoulli", "multinomial", "gaussian", "poisson", "gamma",
  "tweedie", "laplace", "quantile", "huber"
)

dl_activation = c(
  "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout",
  "Maxout", "MaxoutWithDropout"
)

shift_data_frame <- function(stock.df) {
  stock.df$first_price <- shift_vector(stock.df$first_price, 1)
  stock.df$last_trade_price <- shift_vector(stock.df$last_trade_price, 1)
  stock.df$number_of_trades <- shift_vector(stock.df$number_of_trades, 1)
  stock.df$volume <- shift_vector(stock.df$volume, 1)
  stock.df$value <- shift_vector(stock.df$value, 1)
  stock.df$min_price <- shift_vector(stock.df$min_price, 1)
  stock.df$max_price <- shift_vector(stock.df$max_price, 1)
  stock.df$final_price <- shift_vector(stock.df$final_price, 1)
  
  return(stock.df)
}

#----
# shift vector
shift_vector <- function(x, n, up = FALSE){
  if (up) {
    res <- c(x[-(seq(n))], rep(0, n))
  } else {
    res <- c(rep(0, n), x[-length(x):(-length(x)+n-1)])    
  }
  return (res)
}

# deep learning
do_deep_learning <- function(
  stock.symbol = "شفن", trades = trade_df, window.size = 30,
  ml_max_runtime_secs = 7, ml_stopping_metric = "RMSE"
  # ml_stopping_rounds,
  # dl_loss, dl_distribution, dl_activation,
  # forecast_ahead = 1
) {
  # withProgress(message = 'Calculating', value = 0, {
    
    model <- "DL"
    rows.number <- length(model) * length(stock.symbol) 
    
    pred.cols <- c(
      "symbol", "date", "actual_final_price", "change", "model", 
      "predicted_price", "ME", "MAPE", "RMSE"
    )
    pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
    names(pred.df) <- pred.cols
    
    stock.df <- trades %>% filter(
      symbol == stock.symbol & date <= "2020-09-09" & date >= "2020-05-05")
    
    # ahead_df <- forecasting_ahead(
    #   prices = stock.df$final_price, 
    #   start_date = tail(stock.df, 1)$date,
    #   horizon = forecast_ahead
    # )
    
    stock.df <- shift_data_frame(stock.df)
    
    len <- nrow(stock.df)
    stock.df$index <- 1:len
    
    stock.df$trades_number_inverse <- (
      (stock.df$number_of_trades + 0.0001)^(-1)
    )
    
    valid.size <- 1
    train.size <- window.size
    
    #rep(x, y): replicate x, y times
    me.list <- rep(0, len - train.size - valid.size + 1)
    rmse.list <- rep(0, len - train.size - valid.size + 1)
    mape.list <- rep(0, len - train.size - valid.size + 1)
    
    h2o.init(max_mem_size = "4G")
    
    for(j in 1 : (len - train.size - valid.size)) {
      day.index <- j + train.size + valid.size
      
      train_df <- stock.df[j:(j+train.size),]
      test_df <- stock.df[day.index:day.index,]
      
      train_h <- as.h2o(train_df)
      test_h <- as.h2o(test_df)
      
      x <- colnames(stock.df)
      y <- "final_price"
      
      dl_md <- h2o.deeplearning(
        x = x, y = y,
        # distribution = dl_distribution,
        # activation = dl_activation,
        # loss = dl_loss,
        epochs = 1000,
        train_samples_per_iteration = -1,
        reproducible = FALSE,
        balance_classes = FALSE,
        force_load_balance = FALSE,
        seed = 2020,
        score_training_samples = 0,
        score_validation_samples = 0,
        training_frame = train_h,
        # stopping_rounds = ml_stopping_rounds,
        stopping_metric = ml_stopping_metric,
        max_runtime_secs = ml_max_runtime_secs
      )
      
      test_h$yhat <- h2o.predict(dl_md, test_h)
      
      symbol_list <- rep(stock.symbol, valid.size)
      model_list <- rep(model, valid.size)
      
      tdate <- test_df$date
      change <- test_df$change
      actual.price <- test_df$final_price
      
      pred.price <- as.data.frame(test_h$yhat)$yhat
      
      me.list[[j]] <- actual.price - pred.price
      
      rmse.list[[j]] <- (sum((actual.price - pred.price) ^ 2) / 
                           valid.size) ^ (0.5)
      
      mape.list[[j]] <- (abs(actual.price - pred.price) / 
                           (actual.price + 0.0001)) * 100
      
      temp.pred.df <- data.frame(
        symbol_list, tdate, actual.price, change, model_list, 
        pred.price, me.list[[j]], rmse.list[[j]], mape.list[[j]]
      )
      
      colnames(temp.pred.df) <- pred.cols
      pred.df <- rbind(pred.df, temp.pred.df)
      remove(temp.pred.df)
      
      if(j %% 40 == 0) {print(j)}
      # incProgress(
      #   1/(len - train.size - valid.size),
      #   detail = paste(j, " th day")
      # )
      
    }
    p <- list("model" = model, "sym" = stock.symbol, 
              "mape" = mean(mape.list), "rmse" = mean(rmse.list))
    print(p)
    
    pred.df[,"date"] <- as.character(pred.df[,"date"])
    
    profit <- sign(
      (
        pred.df$predicted_price - 
         shift_vector(pred.df$actual_final_price, 1)
      ) * shift_vector(pred.df$change, 1, up = TRUE)
    )
    
    result <- list(
      "df" = pred.df,
      
      #"ahead_df" = ahead_df,
      
      "error_parameters" = data.frame("mape" = p$mape, "rmse" = p$rmse),
      
      "predictions" = pred.df[,c("date", "actual_final_price", 
                                 "predicted_price")],
      
      "residuals" = pred.df[,c("date", "ME")],
      
      "profits" = data.frame(
        "profit" = c("Wrong", "Right"), 
        "color" = c("red", "green"),
        "sum" = c(
          abs(sum(profit[profit == -1])),
          sum(profit[profit != -1])
        )
      ),
      
      "profit_percent" = round(
        (length(profit[profit != -1])/length(profit)), 4
      ) * 100
    )
    
    h2o.shutdown(prompt = FALSE)
  #})
  
  print("__________Finished__________")
  
  return(result)
}

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


#----
library(dplyr)
library(lubridate)

excluded_vars <- c("persian_symbol")

prices_ts_example <- select(
  sufficient_trades_ts_df[sufficient_trades_ts_df$persian_symbol=="آسيا",], 
  - one_of(excluded_vars)
)

tail(prices_ts_example)

df <- prices_ts_example
df$index <- 1:nrow(df)
# df$trend_sqr <- df$trend ^ 2
df$num <- df$number_of_trades^(-1)

h <- 1
diff <- 73
train_df <- df[(nrow(df) - h - 9 - diff):(nrow(df) - h  - diff),]
test_df <- df[(nrow(df) - h + 1  - diff):(nrow(df)  - diff), ]

df.cols <- colnames(df)
forecast_df <- data.frame(matrix(nrow=h, ncol=length(df.cols)))
colnames(forecast_df) <- df.cols

forecast_df$date <- test_df$date
forecast_df$trend <- test_df$trend
forecast_df$trend_sqr <- test_df$trend_sqr

#----
# regression, linear model
lr <- lm(y ~ index + num + volume + value + min_price + max_price, 
         data = train_df)
# + trend + trend_sqr

summary(lr)

test_df$yhat <- predict(lr, newdata = test_df)
mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)
mape_lr


#----
# h2o config
library(h2o)
h2o.init(max_mem_size = "4G")

train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)

x <- c("index", "num", "volume", "value", "min_price", "max_price")
y <- "y"


#----
# Random Forest
rf_md <- h2o.randomForest(training_frame = train_h,
                          x = x, y = y,
                          ntrees = 500, 
                          stopping_rounds = 10,
                          stopping_metric = "RMSE",
                          score_each_iteration = TRUE,
                          stopping_tolerance = 0.0001,
                          seed = 1234)

h2o.varimp_plot(rf_md)
rf_md@model$model_summary

library(plotly)
tree_score <- rf_md@model$scoring_history$training_rmse

plot_ly(x = seq_along(tree_score), y = tree_score,
        type = "scatter", mode = "line") %>%
  layout(title = "The Trained Model Score History",
         yaxis = list(title = "RMSE"),
         xaxis = list(title = "Num. of Trees"))

test_h$pred_rf <- h2o.predict(rf_md, test_h)
test_1 <- as.data.frame(test_h)

mape_rf <- mean(abs(test_1$y - test_1$pred_rf) / test_1$y)
mape_rf

print(mape_rf < mape_lr)


#----
# random forest with gird search
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

rf2 <- h2o.grid(algorithm = "randomForest",
                search_criteria = search_criteria_rf,
                hyper_params = hyper_params_rf,
                x = x, y = y,
                nfolds = 3,
                training_frame = train_h,
                ntrees = 5000,
                grid_id = "rf_grid",
                seed = 1234)


rf2_grid_search <- h2o.getGrid(grid_id = "rf_grid",
                               sort_by = "rmse",
                               decreasing = FALSE)

rf_grid_model <- h2o.getModel(rf2_grid_search@model_ids[[1]])

test_h$rf_grid <- h2o.predict(rf_grid_model, test_h)
mape_rf2 <- mean(abs(test_1$y - test_h$rf_grid) / test_1$y)
mape_rf2

test_h_df <- as.data.frame(test_h)

plot_ly(data = test_h_df) %>%
  add_lines(x = ~ index, y = ~y, name = "Actual") %>%
  add_lines(x = ~ index, y = ~ yhat, name = "Linear Regression", line =
              list(dash = "dot")) %>%
  add_lines(x = ~ index, y = ~ pred_rf, name = "Random Forest", line =
              list(dash = "dash")) %>%
  add_lines(x = ~ index, y = ~ rf_grid, name = "Random Forest (grid)", line
            = list(dash = "dash"))  %>% 
  layout(title = "Actual vs Predicted stock prices",
         yaxis = list(title = "Price"),
         xaxis = list(title = "Day"))


#----
# GBM
gbm_md <- h2o.gbm(
  training_frame = train_h,
  x = x, y = y,
  max_depth = 20,
  distribution = "gaussian",
  ntrees = 500,
  learn_rate = 0.1,
  score_each_iteration = TRUE
)
# nfolds = 5, max_depth = 20,

# review the importance of the model's variables
h2o.varimp_plot(gbm_md)

test_h$pred_gbm <- h2o.predict(gbm_md, test_h)
test_1 <- as.data.frame(test_h)
mape_gbm <- mean(abs(test_1$y - test_1$pred_gbm) / test_1$y)
mape_gbm

plot_ly(data = test_1) %>%
  add_lines(x = ~ index, y = ~y, name = "Actual") %>%
  add_lines(x = ~ index, y = ~ yhat, name = "Linear Regression", line =
              list(dash = "dot")) %>%
  add_lines(x = ~ index, y = ~ pred_gbm, name = "Gradient Boosting Machine",
            line = list(dash = "dash")) %>%
  layout(title = "Actual vs Predicted stock prices",
         yaxis = list(title = "Price"),
         xaxis = list(title = "Day"))


#----
# Forecasting with the AutoML model
autoML1 <- h2o.automl(training_frame = train_h,
                      x = x,
                      y = y,
                      nfolds = 3,
                      max_runtime_secs = 60*30,
                      seed = 1234)

autoML1@leaderboard

test_h$pred_autoML <- h2o.predict(autoML1@leader, test_h)
test_1 <- as.data.frame(test_h)
mape_autoML <- mean(abs(test_1$y - test_1$pred_autoML) / test_1$y)
mape_autoML

plot_ly(data = test_1) %>%
  add_lines(x = ~ index, y = ~y, name = "Actual") %>%
  add_lines(x = ~ index, y = ~ yhat, name = "Linear Regression", line =
              list(dash = "dot")) %>%
  add_lines(x = ~ index, y = ~ pred_autoML, name = "autoML", line =
              list(dash = "dash")) %>%
  layout(title = "Actual vs Predicted stock prices",
         yaxis = list(title = "Price"),
         xaxis = list(title = "Day"))


# putting all together

final_forecast <- as.data.frame(test_h)
write_excel_csv(final_forecast, "../Data/test_ML_final_forecast.csv")

# dataset_used <- rbind(train_df, test_df)
plot_ly(x = final_forecast$index, y = final_forecast$y,
  type = "scatter", mode = "line", name = "Actual") %>%
    add_lines(x = final_forecast$index, y = final_forecast$rf_grid, name =
              "Random Forest") %>%
    add_lines(x = final_forecast$index, y = final_forecast$pred_gbm, name =
            "GBM") %>%
    add_lines(x = final_forecast$index, y = final_forecast$pred_autoML, name =
              "Auto ML") %>%
    layout(title = "Actual vs Predicted stock prices",
         yaxis = list(title = "Price"),
         xaxis = list(title = "Day"))

#----
# read csv, rename columns, change date data type to date
sel_stocks_df = read.csv(
  "../Data/sel_exp_next_df.csv", encoding = "UTF-8")

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "X.U.FEFF.symbol"
] <- "symbol"

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "trade_date"
] <- "date"

names(sel_stocks_df)[ 
  names(sel_stocks_df) == "next_price"
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
random.selected.symbols <- sample(selected.symbols, size = 10)
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
    sel_stocks_df$symbol==stock.sym,]
  
  trade.num <- nrow(stock.df)
  # print(nrow(stock.df))
  stock.df$index <- 1:trade.num
  stock.df$trades_number_inverse <- stock.df$number_of_trades^(-1)
  
  # stock.df <- select(df, -one_of(excluded_vars))
  
  print(trade.num)
  day.index <- train.size + valid.size
  
  mape.list <- rep(0, day.index)
  
  shift <- 4
  train_df <- stock.df[(0):(trade.num - valid.size - shift),]
  test_df <- stock.df[(trade.num - valid.size + 1 - shift):(trade.num - shift),]
  
  #----
  # h2o train, test, prediction and benchmarks datasets
  train_h <- as.h2o(train_df)
  test_h <- as.h2o(test_df)
  
  x <- colnames(train_df)
  y <- "y"
  
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
                            max_runtime_secs = 60 * 5)
  
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

