library("shiny")
library("shinyjs")
library("shinydashboard")
library("dplyr")
library("plotly")
library("forecast")

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
unique_symbols <- select(trade_df, symbol) %>% distinct()

# define start date and end date variables:
start_date <- (trade_df %>% select(date) %>% slice(which.min(date)))$date
end_date <- (trade_df %>% select(date) %>% slice(which.max(date)))$date

models <- list(
    "gradient boost model" = "gbm", "exponential smoothing" = "ets", 
    "deep learning" = "dl", "random forest" = "rf", "regression" = "reg", 
    "multi varient regression" = "multireg", "deep learning grid" = "dlgrid"
)


#----
ui <- fluidPage(
    useShinyjs(),
   sidebarLayout(
        sidebarPanel(
            selectizeInput(
                "symbol_in", "Choose a symbol", choices = unique_symbols,
                options = list(
                    placeholder = "stock symbols",
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            
            dateRangeInput(
                "dateRange_in", "Prediction range", 
                start = start_date, end = end_date, 
                min = start_date, max = end_date,
                format = "yyyy-mm-dd", startview = "month", weekstart = 6,
                language = "en", separator = " to ", width = NULL
            ),
            
            selectizeInput(
                "model_in", "Choose a model", choices = models,
                options = list(
                    placeholder = "predictor models",
                    onInitialize = I('function() { this.setValue(""); }')
                )
            ),
            numericInput("windowSize", "window size", value = 9, 
                         min = 9, max = 30),
            
            actionButton("predictButton", "Predict"),
            
            
        ), mainPanel(
            fluidRow(
                plotlyOutput("symbol_plot"),
                plotlyOutput("predict_plot"),
            ),
            fluidRow(
                column(
                    6, plotlyOutput("residual_plot")
                ),
                column(
                    6, plotlyOutput("profit_plot")
                ),
            )
        )
   ) 
)

server <- function(input, output, session) {
    stock_df <- reactive({
        # make sure end date later than start date
        validate(
            need((input$dateRange_in[2] > input$dateRange_in[1]), 
                 "NOTE: end date should not be earlier than start date"
            )
        )
        
        validate(
            need(input$symbol_in != "", "Please select a stock symbol")
        )
        
        trade_df %>% filter(
            symbol == input$symbol_in 
            & date >= input$dateRange_in[1] & date <= input$dateRange_in[2]
        )
    })
    
    observeEvent(input$symbol_in != "",{
        shinyjs::hide(id = "predict_plot")
        shinyjs::hide(id = "residual_plot", anim = TRUE)
        shinyjs::hide(id = "profit_plot", anim = TRUE)
        shinyjs::show(id = "symbol_plot")
        output$symbol_plot <- renderPlotly({
            stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
        })
    })
    
    do_predict <- reactive({
        validate(need(input$model_in != "", "Please choose a model"))
        
        validate(need(nrow(stock_df()) > 2 * input$windowSize, 
                      "Insufficient data or too-large window size"))
        switch(
            input$model_in, 
            "gbm" = "gbm$",
            "ets" = (do_ets(stock.symbol = input$symbol_in, trades = stock_df(),
                    window.size = input$windowSize)),
            "dl" = "dl$",
            "rf" = "rf$",
            "reg" = "reg$",
            "multireg" = "multi$",
            "dlgrid" = "gird$"
        )
    })
    
    plot_prediction <- function(df) {
        m <- df$predictions
        p <- plot_ly(m, x = ~date, y = ~actual_final_price, 
                     type = "scatter", name = "actual price")
        p %>% add_trace(x = m$date, y = m$predicted_price, 
                        mode = "lines", name = "predicted price")
    }
    
    plot_residual <- function(df) {
        m <- df$residuals
        p <- plot_ly(m, x = ~date, y = ~ME, color = I("red"), 
                     type = "scatter", name = "residuals")
        p %>% add_trace(x = m$date, y = m$ME, 
                        mode = "lines", name = " ")
    }
    
    plot_profit <- function(df) {
        m <- df$profits
        p <- plot_ly(m, x = ~profit, y = ~sum, type = "bar", 
                     name = "profit")
        p
    }
    
    observeEvent(
        input$predictButton, {
            shinyjs::hide(id = "symbol_plot")
            shinyjs::show(id = "predict_plot")
            shinyjs::show(id = "residual_plot", anim = TRUE)
            shinyjs::show(id = "profit_plot", anim = TRUE)
            predict_result <- do_predict() 
            output$predict_plot <- renderPlotly({
                plot_prediction(predict_result)
            })
            output$residual_plot <- renderPlotly({
                plot_residual(predict_result)
            })
            output$profit_plot <- renderPlotly({
                plot_profit(predict_result)
            })
        }
    )
    
}

shinyApp(ui, server)

#----
# exponential smoothing
do_ets <- function(stock.symbol, trades, window.size) {
    model = "MNN"
    rows.number <- length(model) * length(stock.symbol) 
    
    pred.cols <- c("symbol", "date", "actual_final_price", "change", "model", 
                   "predicted_price", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE")
    pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
    names(pred.df) <- pred.cols
    
    stock.df <- trades %>% filter(symbol == stock.symbol) %>% select(
        "date", "final_price", "change")
    
    stock.ts <- ts(stock.df$final_price, frequency = 1)
    
    len <- length(stock.ts)
    
    valid.size <- 1
    train.size <- window.size
    
    #rep(x, y): replicate x, y times
    ets.me.list <- rep(0, len - train.size - valid.size + 1)
    ets.mae.list <- rep(0, len - train.size - valid.size + 1) 
    ets.rmse.list <- rep(0, len - train.size - valid.size + 1)
    ets.mpe.list <- rep(0, len - train.size - valid.size + 1)
    ets.mape.list <- rep(0, len - train.size - valid.size + 1)
    ets.mase.list <- rep(0, len - train.size - valid.size + 1)
    
    for(j in 1 : (len - train.size - valid.size)) {
        day.index <- j + train.size + valid.size
        
        train.ts <- window(stock.ts, start = j , end = j + train.size)
        
        valid.ts <- window(stock.ts, start = j + train.size + 1, 
                           end = day.index)
        
        ets.fit <- ets(
            train.ts, model = model
        )
        
        ets.pred <- forecast(ets.fit, h = valid.size)
        
        ets.me.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"ME"]
        ets.mae.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"MAE"]
        ets.rmse.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"RMSE"]
        ets.mpe.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"MPE"]
        ets.mape.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"MAPE"]
        ets.mase.list[[j]] <- accuracy(ets.pred, valid.ts)[2,"MASE"]
        
        if(j%%40==0){print(j)}
        
        temp.pred.df <- data.frame(matrix(
            nrow=1, ncol=length(pred.cols)))
        
        tdate <- stock.df[day.index,]$date
        actual.price <- valid.ts[[1]]
        pred.price <- ets.pred$mean[[1]]
        
        change <- stock.df[day.index,]$change
        
        temp.pred.df <- data.frame(
            stock.symbol, tdate, actual.price, change, model, pred.price, 
            ets.me.list[[j]], ets.mae.list[[j]], ets.rmse.list[[j]],
            ets.mpe.list[[j]], ets.mape.list[[j]], ets.mase.list[[j]]
        )
        
        colnames(temp.pred.df) <- pred.cols
        
        pred.df <- rbind(pred.df, temp.pred.df)
    }
    
    p <- c(model, stock.symbol, mean(ets.mape.list), mean(ets.rmse.list))
    print(p)
    
    pred.df[,"date"] <- as.character(pred.df[,"date"])
    
    profit <- sign(
        (pred.df$predicted_price - shift_vector(pred.df$actual_final_price, 1))* 
            pred.df$change
    )
    
    result <- list(
        "df" = pred.df,
        
        "predictions" = pred.df[,c("date", "actual_final_price", 
                                   "predicted_price")],
        
        "residuals" = pred.df[,c("date", "ME")],
        
        "profits" = data.frame(
            "profit" = c(-1,0,1), 
            "sum" = c(
                sum(profit[profit == -1]),
                sum(profit[profit == 0]),
                sum(profit[profit == 1])
            )
        ),
        
        "profit_percent" = round(sum(profit[profit != -1]/length(profit)), 4)
        
    )
    print("__________Finished__________")
    
    return (result)
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


#----
# dumb price direction move prediction
dumb_price_move_prediction <- function(df, stock.symbol){
    stock.df <- df %>% filter(symbol == stock.symbol) %>% select(
        "symbol","date", "final_price", "change")
    rows_num <- nrow(stock.df)
    
    pred.cols <- c("symbol", "date", "final_price", "change", "dumb_direct", 
                   "profit")
    
    pred.df <- data.frame(matrix(nrow=rows_num, ncol=length(pred.cols)))
    
    names(pred.df) <- pred.cols
    
    possible_profits <- c(1, -1)
    
    pred.df$symbol <- stock.df$symbol
    pred.df$date <- stock.df$date
    pred.df$final_price <- stock.df$final_price
    pred.df$change <- stock.df$change
    
    pred.df$dumb_direct <- sample(possible_profits, rows_num, TRUE)
    
    profit <- sign(
        pred.df$dumb_direct*pred.df$change
    )
    
    result <- list(
        "df" = pred.df,
        
        "profits" = profit,
        
        "profit_percent" = round(sum(profit[profit != -1]/length(profit)), 4)
    )
    return(result)
}