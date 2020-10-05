#----
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

# Persian symbols of stocks with sufficient trades - read csv
sufficient.trades.symbols <- read.csv(
  "../Data/symbols_with_sufficient_trades.csv", encoding = "UTF-8")

# set seed for producing repeatable random results
set.seed(123)
# pick 20 random stock symbols without replacement (default)
random.selected.symbols <- sufficient.trades.symbols[
  sample(nrow(sufficient.trades.symbols),20),]

random.selected.symbols

#----
# read csv exponential smoothing benchmark & predictions
library(dplyr)

preds.df = read.csv(
  "../Data/exp_smoothing_predictions.csv", encoding = "UTF-8")

names(preds.df)[ 
  names(preds.df) == "X.U.FEFF.symbol"
] <- "symbol"

ets.models <- c("AAN", "ANN", "MAN", "MNN", "MMN")

# Error in plot.new() : figure margins too large
# Invoking dev.off() to make RStudio open up a new graphics device 
# with default settings worked for me

# dev.off()
# graphics.off()

# mar – A numeric vector of length 4, which sets the margin sizes 
# in the following order: bottom, left, top, and right. 
par(mar=c(2,2,1,1))
par(mfrow = c(5, 4))

for (sym.i in 1:length(random.selected.symbols)) {
  stock.sym <- random.selected.symbols[[sym.i]]
  print(stock.sym)
  
  # stock.preds.df <- preds.df[
  #   preds.df$symbol == stock.sym
  # ] %>% select("trade_date", "actual_final_price", "predicted_price")
  
  sel.model <- "AAN"
    
  stock.preds.df <- preds.df %>% filter(symbol == stock.sym & model==sel.model) 
  stock.preds.df <- stock.preds.df[names(stock.preds.df) %in% 
    c("trade_date", "actual_final_price", "predicted_price")]
  
  actual.prices.ts <- ts(stock.preds.df$actual_final_price, frequency = 1)
  
  pred.prices.ts <- ts(stock.preds.df$predicted_price, frequency = 1)
  
  plot(
    actual.prices.ts, xlab="Day", ylab="Price", main=paste(stock.sym, sel.model)
  )
  
  lines(pred.prices.ts, col="red")
}

#----
# just test ts objects
library(dplyr)
library(scales)

preds.df = read.csv(
  "../Data/exp_smoothing_predictions.csv", encoding = "UTF-8")

names(preds.df)[ 
  names(preds.df) == "X.U.FEFF.symbol"
] <- "symbol"

sample.preds.df <- preds.df %>% filter(symbol == "وخارزم" & model=="ANN") 
sample.preds.df <- sample.preds.df[names(sample.preds.df) %in% 
  c("symbol", "model","trade_date", "actual_final_price", "predicted_price")]

actual.prices.ts <- ts(sample.preds.df$actual_final_price, frequency = 1)
# length(actual.prices.ts)
pred.prices.ts <- ts(sample.preds.df$predicted_price, frequency = 1)

par(mfrow = c(1, 1))

plot(
  actual.prices.ts, xlab = "Day", ylab = "Price", main="Actual vs Predicted",
)

# you have function alpha in package scales in which you can directly 
# input your vector of colors
lines(pred.prices.ts, col=alpha(rgb(0.6, 0.3, 0.8), 0.2))
