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

# pick 20 random stock symbols without replacement (default)
set.seed(123) # set seed for producing repeatable results
random.selected.symbols <- sufficient.trades.symbols[
  sample(nrow(sufficient.trades.symbols),20),]

random.selected.symbols

#----
library("forecast")
library(dplyr)
library(readr)

# a three-character string identifying method using the framework terminology 
# of Hyndman et al. (2002) and Hyndman et al. (2008).
# The first letter denotes the error type, the second letter denotes the 
# trend type and the third letter denotes the season type
# In all cases: "N"=none, "A"=additive and "M"=multiplicative
# IMPORTANT : "AMN" & "AMZ" <- Forbidden model combination
ets.models <- c("AAN", "ANN", "MAN", "MNN", "MMN")

parameters.vals <- list(0.7, 0.5, 0.2, 0.1, 0.01, NULL)
is.null(parameters.vals[[6]])

para.len <- length(parameters.vals)
elements <- list(a = parameters.vals, b = parameters.vals)
combinations.6 <- expand.grid(elements)
nrow(combinations.6)
ncol(combinations.6)
class(combinations.6[[5,1]])

validations <- c(1)

#----
# smoothing benchmark function
ets.benchmark <- function(models, parameters.values, stock.symbols, trades, 
                          validation.sizes) {
  
  bench.cols <- c("model", "symbol", "MAE", "RMSE", "alpha", "beta")
  # , "gamma", "valid.size",
  
  rows.number <- length(models) * length(stock.symbols) * 
    length(validation.sizes) * length(parameters.values)
  # 
  
  bench.df <-  data.frame(matrix(nrow=0, ncol=length(bench.cols)))
  
  names(bench.df) <- bench.cols
  
  for (model.i in 1:length(models)) {
    sel.model <- models[[model.i]]
    
    for (sym.i in 1:length(stock.symbols)) {
      stock.sym <- stock.symbols[[sym.i]]
      
      stock.df <- sufficient.trades.ts.df[
        sufficient.trades.ts.df$persian_symbol == stock.sym,
      ] %>% select("trade_date", "final_price")
      
      stock.ts <- ts(stock.df$final_price, frequency = 1)
      
      len <- length(stock.ts)
      
      for (para.row in 1:nrow(parameters.values)) {
        sel.alpha <- parameters.values[[para.row, "a"]]
        sel.beta <- parameters.values[[para.row, "b"]]
      #   sel.gamma <- parameters.values[para.row, "c"]
      
      for (valid.i in 1:length(validation.sizes)) {
        valid.size <- validation.sizes[[valid.i]]
        train.size <- valid.size * 4
        
        #rep(x, y): replicate x, y times
        ets.mae.list <- rep(0, len - train.size - valid.size + 1) 
        ets.rmse.list <- rep(0, len - train.size - valid.size + 1)
        
        for(j in 1 : (len - train.size - valid.size)) {
          train.ts <- window(stock.ts, start = j , end = j + train.size)
          
          valid.ts <- window(stock.ts, start = j + train.size + 1, 
                             end = j + train.size + valid.size)
          
          ets.fit <- ets(
            train.ts, model = sel.model, alpha = sel.alpha, beta = sel.beta
          )
          # 
          # gamma = sel.gamma, restrict = FALSE, 
          # allow.multiplicative.trend = TRUE,
          
          ets.pred <- forecast(ets.fit, h = valid.size)
          
          ets.mae.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"MAE"]
          ets.rmse.list[[j]] <- accuracy(ets.pred, valid.ts)[1,"RMSE"]
          
          if(j%%40==0){print(j)}
          
        }
        
        temp.df <- data.frame(matrix(
          nrow=1, ncol=length(bench.cols)))
        
        # make representation of alpha & beta in df, in case of null values:
        if(is.null(sel.alpha)) 
          sel.alpha.rep <- "estimated" 
        else 
          sel.alpha.rep <- sel.alpha
        
        if(is.null(sel.beta)) 
          sel.beta.rep <- "estimated" 
        else 
          sel.beta.rep <- sel.beta
        
        p <- c(sel.model, stock.sym, mean(ets.mae.list), mean(ets.rmse.list),
               sel.alpha.rep, sel.beta.rep)
        print(p)
        
        temp.df <- data.frame(
          sel.model, stock.sym, mean(ets.mae.list), mean(ets.rmse.list),
          sel.alpha.rep, sel.beta.rep
        )
        #  , sel.gamma, valid.size,
        
        colnames(temp.df) <- bench.cols
        
        bench.df <- rbind(bench.df, temp.df)
      }
      }
    }
  }
  write_excel_csv(bench.df, "../Data/exponential_smoothing_benchmark.csv")
}

# calling ets benchmark function
ets.benchmark(models = ets.models, parameters.values = combinations.6, 
              stock.symbols = random.selected.symbols, 
              trades = sufficient.trades.ts.df, validation.sizes = validations)

