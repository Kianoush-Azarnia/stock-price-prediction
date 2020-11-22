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
library("forecast")
library(dplyr)
library(readr)

validations <- c(1)

#----
# regression benchmark function 
reg.benchmark <- function(stock.symbols, trades) {
  
  bench.cols <- c("symbol", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE")
  
  bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
  names(bench.df) <- bench.cols
  
  pred.cols <- c("symbol", "trade_date", "actual_final_price", 
                 "predicted_price", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE")
  pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
  names(pred.df) <- pred.cols
  
  for (sym.i in 1:length(stock.symbols)) {
    stock.sym <- stock.symbols[[sym.i]]
    
    stock.df <- sufficient.trades.ts.df[
      sufficient.trades.ts.df$persian_symbol == stock.sym,
    ] %>% select("trade_date", "final_price")
    
    stock.ts <- ts(stock.df$final_price, frequency = 1)
    
    len <- length(stock.ts)
      
      valid.size <- 1
      train.size <- valid.size * 9
      
      #rep(x, y): replicate x, y times
      reg.me.list <- rep(0, len - train.size - valid.size + 1)
      reg.mae.list <- rep(0, len - train.size - valid.size + 1) 
      reg.rmse.list <- rep(0, len - train.size - valid.size + 1)
      reg.mpe.list <- rep(0, len - train.size - valid.size + 1)
      reg.mape.list <- rep(0, len - train.size - valid.size + 1)
      reg.mase.list <- rep(0, len - train.size - valid.size + 1)
      
      for(j in 1 : (len - train.size - valid.size)) {
        day.index <- j + train.size + valid.size
        
        train.ts <- window(stock.ts, start = j , end = j + train.size)
        
        valid.ts <- window(stock.ts, start = j + train.size + 1, 
                           end = day.index)
        
        reg.poly.fit <- tslm(train.ts ~ trend + I(trend^2))
        reg.pred <- forecast(reg.poly.fit, h=valid.size, level=0)
        
        reg.me.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"ME"]
        reg.mae.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MAE"]
        reg.rmse.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"RMSE"]
        reg.mpe.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MPE"]
        reg.mape.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MAPE"]
        reg.mase.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MASE"]
        
        if(j%%40==0){print(j)}
        
        temp.pred.df <- data.frame(matrix(
          nrow=1, ncol=length(pred.cols)))
        
        tdate <- stock.df[day.index,]$trade_date
        actual.price <- valid.ts[[1]]
        pred.price <- reg.pred$mean[[1]]
        
        temp.pred.df <- data.frame(
          stock.sym, tdate, actual.price, pred.price, 
          reg.me.list[[j]], reg.mae.list[[j]], reg.rmse.list[[j]],
          reg.mpe.list[[j]], reg.mape.list[[j]], reg.mase.list[[j]]
        )
        
        colnames(temp.pred.df) <- pred.cols
        
        pred.df <- rbind(pred.df, temp.pred.df)
        
      }
      
      p <- c(stock.sym, mean(reg.mae.list), mean(reg.rmse.list))
      print(p)
      
      temp.bench.df <- data.frame(matrix(
        nrow=1, ncol=length(bench.cols)))
      
      temp.bench.df <- data.frame(
        stock.sym, mean(reg.me.list), mean(reg.mae.list), 
        mean(reg.rmse.list), mean(reg.mpe.list), mean(reg.mape.list),
        mean(reg.mase.list)
      )
      
      colnames(temp.bench.df) <- bench.cols
      
      bench.df <- rbind(bench.df, temp.bench.df)
  }
  write_excel_csv(pred.df, "../Data/regression_predictions.csv")
  write_excel_csv(bench.df, "../Data/regression_benchmark.csv")
  print("__________Finished__________")
}

# calling regression benchmark function
reg.benchmark(stock.symbols = random.selected.symbols, 
              trades = sufficient.trades.ts.df)


#----
# read csv regression benchmark & predictions
library(dplyr)

preds.df = read.csv(
  "../Data/regression_predictions.csv", encoding = "UTF-8")

names(preds.df)[ 
  names(preds.df) == "X.U.FEFF.symbol"
] <- "symbol"

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
  
  stock.preds.df <- preds.df %>% filter(symbol == stock.sym) 
  stock.preds.df <- stock.preds.df[names(stock.preds.df) %in% 
        c("trade_date", "actual_final_price", "predicted_price")]
  
  actual.prices.ts <- ts(stock.preds.df$actual_final_price, frequency = 1)
  
  pred.prices.ts <- ts(stock.preds.df$predicted_price, frequency = 1)
  
  plot(
    actual.prices.ts, xlab="Day", ylab="Price", main=paste(stock.sym), 
  )
  
  lines(pred.prices.ts, col="red")
}

#----
# plot some time series with regression predictions
