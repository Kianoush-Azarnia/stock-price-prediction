library("lubridate")
library("dplyr")
library("plotly")
library("forecast")

library(h2o)

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
    "exponential smoothing" = "ets", 
    "linear regression" = "reg", 
    "multi-variable regression" = "multireg"
)

ml_models <- list(
    "gradient boost machine" = "gbm", "deep learning" = "dl", 
    "random forest" = "rf",
    "deep learning grid" = "dlgrid"
)

#----
ui <- navbarPage("Stock Price Prediction",
    tabPanel("Statistical",
        useShinyjs(),
        sidebarLayout(
            sidebarPanel(
                selectizeInput(
                    "stat_symbol_in", "Choose a symbol", choices = unique_symbols,
                    options = list(
                        placeholder = "stock symbols",
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                ),
                
                dateRangeInput(
                    "stat_dateRange_in", "Prediction range", 
                    start = start_date, end = end_date, 
                    min = start_date, max = end_date,
                    format = "yyyy-mm-dd", startview = "month", weekstart = 6,
                    language = "en", separator = " to ", width = NULL
                ),
                
                selectizeInput(
                    "stat_model_in", "Choose a model", choices = stat_models,
                    options = list(
                        placeholder = "predictor models",
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                ),
                numericInput("stat_window_size", "window size", value = 9, 
                             min = 9, max = 30),
                
                actionButton("stat_predict_button", "Predict"),
                
                tableOutput("stat_model_error")
            ), 
            mainPanel(
                fluidRow(
                    plotlyOutput("stat_symbol_plot"),
                    
                    plotlyOutput("stat_predict_plot"),
                ),
                fluidRow(
                    column(
                        6, plotlyOutput("stat_residual_plot")
                    ),
                    column(
                        6, plotlyOutput("stat_profit_plot")
                    ),
                )
            )
        ) 
    ),
    tabPanel("Machine Learning", 
        useShinyjs(),
        sidebarLayout(
         sidebarPanel(
             selectizeInput(
                 "ml_symbol_in", "Choose a symbol", choices = unique_symbols,
                 options = list(
                     placeholder = "stock symbols",
                     onInitialize = I('function() { this.setValue(""); }')
                 )
             ),
             
             dateRangeInput(
                 "ml_dateRange_in", "Prediction range", 
                 start = start_date, end = end_date, 
                 min = start_date, max = end_date,
                 format = "yyyy-mm-dd", startview = "month", weekstart = 6,
                 language = "en", separator = " to ", width = NULL
             ),
             
             selectizeInput(
                 "ml_model_in", "Choose a model", choices = stat_models,
                 options = list(
                     placeholder = "predictor models",
                     onInitialize = I('function() { this.setValue(""); }')
                 )
             ),
             numericInput("ml_window_size", "window size", value = 9, 
                          min = 9, max = 30),
             
             actionButton("ml_predict_button", "Predict"),
             
             tableOutput("ml_model_error")
         ), 
         mainPanel(
             fluidRow(
                 plotlyOutput("ml_symbol_plot"),
                 
                 plotlyOutput("ml_predict_plot"),
             ),
             fluidRow(
                 column(
                     6, plotlyOutput("ml_residual_plot")
                 ),
                 column(
                     6, plotlyOutput("ml_profit_plot")
                 ),
             )
         )
        )
    )
)


server <- function(input, output, session) {
    stock_df <- reactive({
        # make sure end date later than start date
        validate(
            need((input$stat_dateRange_in[2] > input$stat_dateRange_in[1]), 
                 "NOTE: end date should not be earlier than start date"
            )
        )
        
        validate(
            need(input$stat_symbol_in != "", "Please select a stock symbol")
        )
        
        trade_df %>% filter(
            symbol == input$stat_symbol_in 
            & date >= input$stat_dateRange_in[1] & date <= input$stat_dateRange_in[2]
        )
    })
    
    observeEvent(input$stat_symbol_in != "",{
        shinyjs::hide(id = "stat_predict_plot", anim = TRUE)
        shinyjs::hide(id = "stat_residual_plot", anim = TRUE)
        shinyjs::hide(id = "stat_profit_plot", anim = TRUE)
        shinyjs::hide(id = "stat_model_error")
        
        output$stat_symbol_plot <- renderPlotly({
            stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
        })
        
        shinyjs::show(id = "stat_symbol_plot" , anim = TRUE)
    })
    
    do_stat_predict <- reactive({
        stat_validate_before_plot()
        switch(
            input$stat_model_in, 
            "ets" = (
                do_ets(stock.symbol = input$stat_symbol_in, 
                    trades = stock_df(), window.size = input$stat_window_size)
            ),
            "reg" = (
                do_reg(stock.symbol = input$stat_symbol_in, 
                    trades = stock_df(), window.size = input$stat_window_size)
            ),
            "multireg" = (
                do_multi_reg(stock.symbol = input$stat_symbol_in, 
                    trades = stock_df(), window.size = input$stat_window_size)
            )
        )
    })
    
    do_ml_predict <- reactive({
        switch(
            input$ml_model_in,
            "gbm" = (
                do_gbm(stock.symbol = input$ml_symbol_in, 
                    trades = stock_df(), window.size = input$ml_window_size)
            ),
            "dl" = (
                do_deep_learning(stock.symbol = input$ml_symbol_in, 
                    trades = stock_df(), window.size = input$ml_window_size)
            ),
            "rf" = (
                do_random_forest(stock.symbol = input$ml_symbol_in, 
                    trades = stock_df(), window.size = input$ml_window_size)
            ),
            "dlgrid" = (
                do_grid_on_deep_learning(stock.symbol = input$ml_symbol_in, 
                    trades = stock_df(), window.size = input$ml_window_size)
            )
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
        p <- plot_ly(m, x = ~date, y = ~ME, color = I("brown"), 
                     type = "scatter", name = "residuals")
        return(p)
    }
    
    plot_profit <- function(df) {
        m <- df$profits
        p <- plot_ly(m, x = ~profit, y = ~sum, type = "bar", 
                     color = ~profit=="Right", colors = ~color)
        return(p)
    }
    
    stat_validate_before_plot <- function() {
        validate(need(input$stat_model_in != "", "Please choose a model"))
        
        validate(need(nrow(stock_df()) > 2 * input$stat_window_size, 
                      "Insufficient data or too-large window size"))
    }
    
    observeEvent(
        input$stat_predict_button, {
            shinyjs::hide(id = "stat_predict_plot", anim = TRUE)
            shinyjs::hide(id = "stat_residual_plot", anim = TRUE)
            shinyjs::hide(id = "stat_profit_plot", anim = TRUE)
            shinyjs::hide(id = "stat_model_error")
            shinyjs::hide(id = "stat_symbol_plot" , anim = TRUE)
            
            if (input$stat_model_in == "") {
                showNotification("Please Choose a model")
            } else if (nrow(stock_df()) < 2 * input$stat_window_size) {
                showNotification("Too large window size or insufficient data")
            }
            
            predict_result <- do_stat_predict()
            
            output$stat_predict_plot <- renderPlotly({
                plot_prediction(predict_result)
            })
            
            output$stat_residual_plot <- renderPlotly({
                plot_residual(predict_result)
            })
            
            output$stat_profit_plot <- renderPlotly({
                plot_profit(predict_result)
            })
            
            output$stat_model_error <- renderTable({
                predict_result$error_parameters
            })
            
            shinyjs::show(id = "stat_predict_plot", anim = TRUE,)
            shinyjs::show(id = "stat_residual_plot", anim = TRUE)
            shinyjs::show(id = "stat_profit_plot", anim = TRUE)
            shinyjs::show(id = "stat_model_error") 
        }
    )
    
}

shinyApp(ui, server)


#----
# exponential smoothing
do_ets <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        model = "MNN"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE"
        )
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
            
            incProgress(
                1/(len - train.size - valid.size), 
                detail = paste(j, " th day")
            )
            
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
        
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(ets.mape.list), "rmse" = mean(ets.rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            ( pred.df$predicted_price - 
                shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
    })
    print("__________Finished__________")
    
    return (result)
}

#----
# do regression
do_reg <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        model <- "lr"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAE", "RMSE", "MPE", "MAPE", "MASE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol) %>% select(
            "date", "final_price", "change")
        
        stock.ts <- ts(stock.df$final_price, frequency = 1)
        
        len <- length(stock.ts)
        
        valid.size <- 1
        train.size <- window.size
        
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
            
            reg.fit <- tslm(train.ts ~ trend + I(trend^2))
            
            reg.pred <- forecast(reg.fit, h=valid.size, level=0)
            
            reg.me.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"ME"]
            reg.mae.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MAE"]
            reg.rmse.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"RMSE"]
            reg.mpe.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MPE"]
            reg.mape.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MAPE"]
            reg.mase.list[[j]] <- accuracy(reg.pred, valid.ts)[2,"MASE"]
            
            if(j%%40==0){print(j)}
            
            incProgress(
                1/(len - train.size - valid.size), 
                detail = paste(j, " th day")
            )
            
            temp.pred.df <- data.frame(matrix(
                nrow=1, ncol=length(pred.cols)))
            
            tdate <- stock.df[day.index,]$date
            actual.price <- valid.ts[[1]]
            pred.price <- reg.pred$mean[[1]]
            
            change <- stock.df[day.index,]$change
            
            temp.pred.df <- data.frame(
                stock.symbol, tdate, actual.price, change, model, pred.price, 
                reg.me.list[[j]], reg.mae.list[[j]], reg.rmse.list[[j]],
                reg.mpe.list[[j]], reg.mape.list[[j]], reg.mase.list[[j]]
            )
            
            colnames(temp.pred.df) <- pred.cols
            
            pred.df <- rbind(pred.df, temp.pred.df)
        }
        
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(reg.mape.list), "rmse" = mean(reg.rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (
                pred.df$predicted_price - 
                shift_vector(pred.df$actual_final_price, 1)) * 
                pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
    })
    print("__________Finished__________")
    
    return (result)
}

#----
# multi variable regression
do_multi_reg <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        
        model <- "multi-var LR"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAPE", "RMSE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol)
        
        stock.df <- shift_data_frame(stock.df)
        
        len <- nrow(stock.df)
        stock.df$index <- 1:len
        
        stock.df$trades_number_inverse <- (
            (stock.df$number_of_trades + 0.0001)^(-1)
        )
        
        valid.size <- 1
        train.size <- window.size
        
        #rep(x, y): replicate x, y times
        reg.me.list <- rep(0, len - train.size - valid.size + 1)
        reg.rmse.list <- rep(0, len - train.size - valid.size + 1)
        reg.mape.list <- rep(0, len - train.size - valid.size + 1)
        
        for(j in 1 : (len - train.size - valid.size)) {
            day.index <- j + train.size + valid.size
            
            train_df <- stock.df[j:(j+train.size),]
            test_df <- stock.df[day.index:day.index,]
            
            lr <- lm(
                final_price ~ index + trades_number_inverse + volume + 
                value + min_price + max_price, data = train_df
            )
            
            test_df$yhat <- predict(lr, newdata = test_df)
            pred.price <- test_df$yhat
            
            tdate <-stock.df[day.index,]$date
            actual.price <- stock.df[day.index,]$final_price
            change <- stock.df[day.index,]$change
            
            reg.me.list[[j]] <- actual.price - pred.price
            
            reg.mape.list[[j]] <- abs(actual.price - pred.price) / 
                (actual.price + 0.000001) * 100
            
            reg.rmse.list[[j]] <- (
                (sum((actual.price - pred.price)^2/valid.size))^(0.5)
            )
            
            
            if(j%%40==0){print(j)}
            
            incProgress(
                1/(len - train.size - valid.size),
                detail = paste(j, " th day")
            )
            
            temp.pred.df <- data.frame(matrix(
                nrow=1, ncol=length(pred.cols)))
            
            temp.pred.df <- data.frame(
                stock.symbol, tdate, actual.price, change, model, pred.price, 
                reg.me.list[[j]], reg.mape.list[[j]], reg.rmse.list[[j]]
            )
            
            colnames(temp.pred.df) <- pred.cols
            
            pred.df <- rbind(pred.df, temp.pred.df)
            
            remove(temp.pred.df)
        }
        
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(reg.mape.list), "rmse" = mean(reg.rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (pred.df$predicted_price - 
                shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
    })
    print("__________Finished__________")
    
    return (result)
}

#----
# Gradient boost machine
do_gbm <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {

        model <- "Gradient Boost"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAPE", "RMSE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol)
        
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
            
            gbm_md <- h2o.gbm(
                training_frame = train_h,
                x = x, y = y,
                max_depth = 20,
                distribution = "gaussian",
                stopping_metric = "RMSE",
                max_runtime_secs = 60 * 0.25,
                ntrees = 500,
                learn_rate = 0.1,
                score_each_iteration = TRUE
            )
            
            test_h$yhat <- h2o.predict(gbm_md, test_h)
            
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
            incProgress(
                1/(len - train.size - valid.size),
                detail = paste(j, " th day")
            )
            
        }
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(mape.list), "rmse" = mean(rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (pred.df$predicted_price - 
                 shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
        
        h2o.shutdown(prompt = FALSE)
    })
    
    print("__________Finished__________")
    
    return(result)
}

#----
# random forest
do_random_forest <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        
        model <- "Gradient Boost"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAPE", "RMSE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol)
        
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
            
            rf_md <- h2o.randomForest(
                training_frame = train_h,
                x = x, y = y,
                ntrees = 500,
                max_runtime_secs = 60 * 0.25,
                stopping_rounds = 10,
                stopping_metric = "RMSE",
                score_each_iteration = TRUE,
                stopping_tolerance = 0.0001,
                seed = 1234
            )
            
            test_h$yhat <- h2o.predict(rf_md, test_h)
            
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
            incProgress(
                1/(len - train.size - valid.size),
                detail = paste(j, " th day")
            )
            
        }
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(mape.list), "rmse" = mean(rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (pred.df$predicted_price - 
                 shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
        
        h2o.shutdown(prompt = FALSE)
    })
    
    print("__________Finished__________")
    
    return(result)
}

#----
# deep learning
do_deep_learning <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        
        model <- "Gradient Boost"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAPE", "RMSE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol)
        
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
                max_runtime_secs = 60 * 0.25
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
            incProgress(
                1/(len - train.size - valid.size),
                detail = paste(j, " th day")
            )
            
        }
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(mape.list), "rmse" = mean(rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (pred.df$predicted_price - 
                 shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
        
        h2o.shutdown(prompt = FALSE)
    })
    
    print("__________Finished__________")
    
    return(result)
}

#----
# grid search on deep learning
do_grid_on_deep_learning <- function(stock.symbol, trades, window.size) {
    withProgress(message = 'Calculating', value = 0, {
        
        model <- "Gradient Boost"
        rows.number <- length(model) * length(stock.symbol) 
        
        pred.cols <- c(
            "symbol", "date", "actual_final_price", "change", "model", 
            "predicted_price", "ME", "MAPE", "RMSE"
        )
        pred.df <- data.frame(matrix(nrow=0, ncol=length(pred.cols)))
        names(pred.df) <- pred.cols
        
        stock.df <- trades %>% filter(symbol == stock.symbol)
        
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
        
        for(j in 1 : (len - train.size - valid.size)) {
            h2o.init(max_mem_size = "4G")
            
            day.index <- j + train.size + valid.size
            
            train_df <- stock.df[j:(j+train.size),]
            test_df <- stock.df[day.index:day.index,]
            
            train_h <- as.h2o(train_df)
            test_h <- as.h2o(test_df)
            
            x <- colnames(stock.df)
            y <- "final_price"
            
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
                max_w2 = c(10, 100, 1000, 3.4028235e+38)
            )
            
            search_criteria_dl <- list(
                strategy = "RandomDiscrete", 
                max_models = 100,
                max_runtime_secs = 60 * 0.25,
                stopping_tolerance = 0.001,
                stopping_rounds = 15,
                stopping_metric = "RMSE",
                seed = 2020
            )
            
            dl_grid <- h2o.grid(
                algorithm = "deeplearning",
                search_criteria = search_criteria_dl,
                hyper_params = hyper_params_dl,
                x = x, y = y,
                training_frame = train_h,
                grid_id = "dl_grid",
                seed = 1234
            )
            
            dl2_grid_search <- h2o.getGrid(
                grid_id = "dl_grid",
                sort_by = "rmse",
                decreasing = FALSE
            )
            
            dl_grid_md <- h2o.getModel(dl2_grid_search@model_ids[[1]])
            
            test_h$yhat <- h2o.predict(dl_grid_md, test_h)
            
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
            incProgress(
                1/(len - train.size - valid.size),
                detail = paste(j, " th day")
            )
            
            h2o.shutdown(prompt = FALSE)
            
            Sys.sleep(2)
        }
        p <- list("model" = model, "sym" = stock.symbol, 
                  "mape" = mean(mape.list), "rmse" = mean(rmse.list))
        print(p)
        
        pred.df[,"date"] <- as.character(pred.df[,"date"])
        
        profit <- sign(
            (pred.df$predicted_price - 
                 shift_vector(pred.df$actual_final_price, 1)) * pred.df$change
        )
        
        result <- list(
            "df" = pred.df,
            
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
                sum(profit[profit != -1]/length(profit)), 4
            )
        )
        
    })
    
    print("__________Finished__________")
    
    return(result)
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

#----
# shift dataframe one day for ML algorithms
shift_data_frame <- function(stock.df) {
    stock.df$first_price <- shift_vector(stock.df$first_price, 1)
    stock.df$last_trade_price <- shift_vector(stock.df$last_trade_price, 1)
    stock.df$number_of_trades <- shift_vector(stock.df$number_of_trades, 1)
    stock.df$volume <- shift_vector(stock.df$volume, 1)
    stock.df$value <- shift_vector(stock.df$value, 1)
    stock.df$min_price <- shift_vector(stock.df$min_price, 1)
    stock.df$max_price <- shift_vector(stock.df$max_price, 1)
    stock.df$change <- shift_vector(stock.df$change, 1)
    
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
