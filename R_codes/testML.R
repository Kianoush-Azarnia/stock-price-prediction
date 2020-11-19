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
