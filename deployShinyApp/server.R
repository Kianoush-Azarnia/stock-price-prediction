server <- function(input, output, session) {
    # stat
    #----
    stat_stock_df <- reactive({
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
    
    stat_symbol_change <- reactive({
      list(input$stat_symbol_in, input$stat_dateRange_in)
    })
    
    observeEvent(stat_symbol_change(), {
      shinyjs::hide(id = "stat_predict_plot", anim = TRUE)
      shinyjs::hide(id = "stat_residual_plot", anim = TRUE)
      shinyjs::hide(id = "stat_profit_plot", anim = TRUE)
      shinyjs::hide(id = "stat_model_error")
      
      output$stat_symbol_plot <- renderPlotly({
        stat_stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
      })
      
      shinyjs::show(id = "stat_symbol_plot" , anim = TRUE)
    })
    
    do_stat_predict <- reactive({
      stat_validate_before_plot()
      switch(
        input$stat_model_in, 
        "ets" = (
          do_ets(stock.symbol = input$stat_symbol_in, 
                 trades = stat_stock_df(), window.size = input$stat_window_size)
        ),
        "reg" = (
          do_reg(stock.symbol = input$stat_symbol_in, 
                 trades = stat_stock_df(), window.size = input$stat_window_size)
        ),
        "multireg" = (
          do_multi_reg(stock.symbol = input$stat_symbol_in, 
                       trades = stat_stock_df(), window.size = input$stat_window_size)
        )
      )
    })
    
    stat_validate_before_plot <- function() {
      validate(need(input$stat_model_in != "", "Please choose a model"))
      
      validate(need(nrow(stat_stock_df()) >= 2 * input$stat_window_size, 
                    "Insufficient data or too-large window size"))
    }
    
    observeEvent(
      input$stat_predict_button, {
        shinyjs::hide(id = "stat_predict_plot", anim = TRUE)
        shinyjs::hide(id = "stat_residual_plot", anim = TRUE)
        shinyjs::hide(id = "stat_profit_plot", anim = TRUE)
        shinyjs::hide(id = "stat_model_error")
        
        if (input$stat_model_in == "") {
          showNotification("Please Choose a model")
        } else if (nrow(stat_stock_df()) < 2 * input$stat_window_size) {
          showNotification("Too large window size or insufficient data")
        }
        
        predict_result <- do_stat_predict()
        
        shinyjs::hide(id = "stat_symbol_plot" , anim = TRUE)
        
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
    
    # dl
    #----
    ml_stock_df <- reactive({
      # make sure end date later than start date
      validate(
        need((input$ml_dateRange_in[2] > input$ml_dateRange_in[1]), 
             "NOTE: end date should not be earlier than start date"
        )
      )
      
      validate(
        need(input$ml_symbol_in != "", "Please select a stock symbol")
      )
      
      trade_df %>% filter(
        symbol == input$ml_symbol_in 
        & date >= input$ml_dateRange_in[1] & 
          date <= input$ml_dateRange_in[2]
      )
    })
    
    ml_symbol_change <- reactive({
      list(input$ml_symbol_in, input$ml_dateRange_in)
    })
    
    observeEvent(ml_symbol_change(), {
      shinyjs::hide(id = "ml_predict_plot", anim = TRUE)
      shinyjs::hide(id = "ml_residual_plot", anim = TRUE)
      shinyjs::hide(id = "ml_profit_plot", anim = TRUE)
      shinyjs::hide(id = "ml_model_error")
      
      output$ml_symbol_plot <- renderPlotly({
        ml_stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
      })
      
      shinyjs::show(id = "ml_symbol_plot" , anim = TRUE)
    })
    
    ml_validate_before_plot <- function() {
      validate(need(input$ml_model_in != "", "Please choose a model"))
      
      validate(need(nrow(ml_stock_df()) >= 2 * input$ml_window_size, 
                    "Insufficient data or too-large window size"))
    }
    
    do_ml_predict <- reactive({
      ml_validate_before_plot()
      switch(
        input$ml_model_in,
        "dl" = (
          do_deep_learning(
            stock.symbol = input$ml_symbol_in, 
            trades = ml_stock_df(), window.size = input$ml_window_size,
            ml_max_runtime_secs = input$ml_max_runtime_secs,
            ml_stopping_metric = input$ml_stopping_metric,
            ml_stopping_rounds = input$ml_stopping_rounds,
            dl_activation = input$dl_activation,
            dl_distribution = input$dl_distribution,
            dl_loss = input$dl_loss
          )
        ),
        "dlgrid" = (
          do_grid_on_deep_learning(
            stock.symbol = input$ml_symbol_in, 
            trades = ml_stock_df(), window.size = input$ml_window_size,
            ml_max_runtime_secs = input$ml_max_runtime_secs,
            ml_stopping_metric = input$ml_stopping_metric,
            ml_stopping_rounds = input$ml_stopping_rounds
          )
        )
      )
    })
    
    observeEvent(
      input$ml_predict_button, {
        shinyjs::hide(id = "ml_predict_plot", anim = TRUE)
        shinyjs::hide(id = "ml_residual_plot", anim = TRUE)
        shinyjs::hide(id = "ml_profit_plot", anim = TRUE)
        shinyjs::hide(id = "ml_model_error")
        
        if (input$ml_model_in == "") {
          showNotification("Please Choose a model")
        } else if (nrow(ml_stock_df()) < 2 * input$ml_window_size) {
          showNotification("Too large window size or insufficient data")
        }
        
        predict_result <- do_ml_predict()
        
        shinyjs::hide(id = "ml_symbol_plot" , anim = TRUE)
        
        output$ml_predict_plot <- renderPlotly({
          plot_prediction(predict_result)
        })
        
        output$ml_residual_plot <- renderPlotly({
          plot_residual(predict_result)
        })
        
        output$ml_profit_plot <- renderPlotly({
          plot_profit(predict_result)
        })
        
        output$ml_model_error <- renderTable({
          predict_result$error_parameters
        })
        
        shinyjs::show(id = "ml_predict_plot", anim = TRUE,)
        shinyjs::show(id = "ml_residual_plot", anim = TRUE)
        shinyjs::show(id = "ml_profit_plot", anim = TRUE)
        shinyjs::show(id = "ml_model_error") 
      }
    )
    
    # gbm
    #----
    gbm_stock_df <- reactive({
      # make sure end date later than start date
      validate(
        need((input$gbm_dateRange_in[2] > input$gbm_dateRange_in[1]), 
             "NOTE: end date should not be earlier than start date"
        )
      )
      
      validate(
        need(input$gbm_symbol_in != "", "Please select a stock symbol")
      )
      
      trade_df %>% filter(
        symbol == input$gbm_symbol_in 
        & date >= input$gbm_dateRange_in[1] & 
          date <= input$gbm_dateRange_in[2]
      )
    })
    
    gbm_symbol_change <- reactive({
      list(input$gbm_symbol_in, input$gbm_dateRange_in)
    })
    
    observeEvent(gbm_symbol_change(), {
      shinyjs::hide(id = "gbm_predict_plot", anim = TRUE)
      shinyjs::hide(id = "gbm_residual_plot", anim = TRUE)
      shinyjs::hide(id = "gbm_profit_plot", anim = TRUE)
      shinyjs::hide(id = "gbm_model_error")
      
      output$gbm_symbol_plot <- renderPlotly({
        gbm_stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
      })
      
      shinyjs::show(id = "gbm_symbol_plot" , anim = TRUE)
    })
    
    gbm_validate_before_plot <- function() {
      validate(need(input$gbm_distribution_in != "", "Please choose a model"))
      
      validate(need(nrow(gbm_stock_df()) >= 2 * input$gbm_window_size, 
                    "Insufficient data or too-large window size"))
    }
    
    do_gbm_predict <- reactive({
      gbm_validate_before_plot()
      do_gbm(
        stock.symbol = input$gbm_symbol_in, 
        trades = gbm_stock_df(), window.size = input$gbm_window_size,
        gbm_max_runtime_secs = input$gbm_max_runtime_secs,
        gbm_stopping_metric = input$gbm_stopping_metric,
        gbm_stopping_rounds = input$gbm_stopping_rounds,
        gbm_ntrees = input$gbm_ntrees,
        gbm_max_depth = input$gbm_max_depth,
        gbm_learn_rate = input$gbm_learn_rate
      )
    })
    
    observeEvent(
      input$gbm_predict_button, {
        shinyjs::hide(id = "gbm_predict_plot", anim = TRUE)
        shinyjs::hide(id = "gbm_residual_plot", anim = TRUE)
        shinyjs::hide(id = "gbm_profit_plot", anim = TRUE)
        shinyjs::hide(id = "gbm_model_error")
        
        if (input$gbm_distribution_in == "") {
          showNotification("Please Choose a model")
        } else if (nrow(gbm_stock_df()) < 2 * input$gbm_window_size) {
          showNotification("Too large window size or insufficient data")
        }
        
        predict_result <- do_gbm_predict()
        
        shinyjs::hide(id = "gbm_symbol_plot" , anim = TRUE)
        
        output$gbm_predict_plot <- renderPlotly({
          plot_prediction(predict_result)
        })
        
        output$gbm_residual_plot <- renderPlotly({
          plot_residual(predict_result)
        })
        
        output$gbm_profit_plot <- renderPlotly({
          plot_profit(predict_result)
        })
        
        output$gbm_model_error <- renderTable({
          predict_result$error_parameters
        })
        
        shinyjs::show(id = "gbm_predict_plot", anim = TRUE,)
        shinyjs::show(id = "gbm_residual_plot", anim = TRUE)
        shinyjs::show(id = "gbm_profit_plot", anim = TRUE)
        shinyjs::show(id = "gbm_model_error") 
      }
    )
    
    # rf
    #----
    rf_stock_df <- reactive({
      # make sure end date later than start date
      validate(
        need((input$rf_dateRange_in[2] > input$rf_dateRange_in[1]), 
             "NOTE: end date should not be earlier than start date"
        )
      )
      
      validate(
        need(input$rf_symbol_in != "", "Please select a stock symbol")
      )
      
      trade_df %>% filter(
        symbol == input$rf_symbol_in 
        & date >= input$rf_dateRange_in[1] & 
          date <= input$rf_dateRange_in[2]
      )
    })
    
    rf_symbol_change <- reactive({
      list(input$rf_symbol_in, input$rf_dateRange_in)
    })
    
    observeEvent(rf_symbol_change(), {
      shinyjs::hide(id = "rf_predict_plot", anim = TRUE)
      shinyjs::hide(id = "rf_residual_plot", anim = TRUE)
      shinyjs::hide(id = "rf_profit_plot", anim = TRUE)
      shinyjs::hide(id = "rf_model_error")
      
      output$rf_symbol_plot <- renderPlotly({
        rf_stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
      })
      
      shinyjs::show(id = "rf_symbol_plot" , anim = TRUE)
    })
    
    rf_validate_before_plot <- function() {
      validate(need(input$rf_distribution_in != "", "Please choose a model"))
      
      validate(need(nrow(rf_stock_df()) >= 2 * input$rf_window_size, 
                    "Insufficient data or too-large window size"))
    }
    
    do_rf_predict <- reactive({
      rf_validate_before_plot()
      do_random_forest(
        stock.symbol = input$rf_symbol_in, 
        trades = rf_stock_df(), window.size = input$rf_window_size,
        ml_max_runtime_secs = input$rf_max_runtime_secs,
        ml_stopping_metric = input$rf_stopping_metric,
        ml_stopping_rounds = input$rf_stopping_rounds,
        rf_ntrees = input$rf_ntrees,
        rf_max_depth = input$rf_max_depth
      )
    })
    
    observeEvent(
      input$rf_predict_button, {
        shinyjs::hide(id = "rf_predict_plot", anim = TRUE)
        shinyjs::hide(id = "rf_residual_plot", anim = TRUE)
        shinyjs::hide(id = "rf_profit_plot", anim = TRUE)
        shinyjs::hide(id = "rf_model_error")
        
        if (input$rf_distribution_in == "") {
          showNotification("Please Choose a model")
        } else if (nrow(rf_stock_df()) < 2 * input$rf_window_size) {
          showNotification("Too large window size or insufficient data")
        }
        
        predict_result <- do_rf_predict()
        
        shinyjs::hide(id = "rf_symbol_plot" , anim = TRUE)
        
        output$rf_predict_plot <- renderPlotly({
          plot_prediction(predict_result)
        })
        
        output$rf_residual_plot <- renderPlotly({
          plot_residual(predict_result)
        })
        
        output$rf_profit_plot <- renderPlotly({
          plot_profit(predict_result)
        })
        
        output$rf_model_error <- renderTable({
          predict_result$error_parameters
        })
        
        shinyjs::show(id = "rf_predict_plot", anim = TRUE,)
        shinyjs::show(id = "rf_residual_plot", anim = TRUE)
        shinyjs::show(id = "rf_profit_plot", anim = TRUE)
        shinyjs::show(id = "rf_model_error") 
      }
    )
    
    # ets
    #----
    ets_stock_df <- reactive({
      # make sure end date later than start date
      validate(
        need((input$ets_dateRange_in[2] > input$ets_dateRange_in[1]), 
             "NOTE: end date should not be earlier than start date"
        )
      )
      
      validate(
        need(input$ets_symbol_in != "", "Please select a stock symbol")
      )
      
      trade_df %>% filter(
        symbol == input$ets_symbol_in & 
          date >= input$ets_dateRange_in[1] & 
          date <= input$ets_dateRange_in[2]
      )
    })
    
    ets_symbol_change <- reactive({
      list(input$ets_symbol_in, input$ets_dateRange_in)
    })
    
    observeEvent(ets_symbol_change(), {
      shinyjs::hide(id = "ets_predict_plot", anim = TRUE)
      shinyjs::hide(id = "ets_residual_plot", anim = TRUE)
      shinyjs::hide(id = "ets_profit_plot", anim = TRUE)
      shinyjs::hide(id = "ets_model_error")
      
      output$ets_symbol_plot <- renderPlotly({
        ets_stock_df() %>% plot_ly(x = ~date, y = ~final_price, 
                                   mode = "lines")
      })
      
      shinyjs::show(id = "ets_symbol_plot" , anim = TRUE)
    })
    
    do_ets_predict <- reactive({
      ets_validate_before_plot()
      do_ets(
        stock.symbol = input$ets_symbol_in, 
        trades = ets_stock_df(), 
        window.size = input$ets_window_size,
        model = input$ets_model_in,
        multiplicative_trend = input$ets_multiplicative,
        additive_only = input$ets_additive
        
      )
    })
    
    ets_validate_before_plot <- function() {
      validate(need(input$ets_model_in != "", "Please choose a model"))
      
      validate(need(nrow(ets_stock_df()) >= 2 * input$ets_window_size, 
                    "Insufficient data or too-large window size"))
    }
    
    observeEvent(
      input$ets_predict_button, {
        shinyjs::hide(id = "ets_predict_plot", anim = TRUE)
        shinyjs::hide(id = "ets_residual_plot", anim = TRUE)
        shinyjs::hide(id = "ets_profit_plot", anim = TRUE)
        shinyjs::hide(id = "ets_model_error")
        
        if (input$ets_model_in == "") {
          showNotification("Please Choose a model")
        } else if (nrow(ets_stock_df()) < 2 * input$ets_window_size) {
          showNotification("Too large window size or insufficient data")
        }
        
        predict_result <- do_ets_predict()
        
        shinyjs::hide(id = "ets_symbol_plot" , anim = TRUE)
        
        output$ets_predict_plot <- renderPlotly({
          plot_prediction(predict_result)
        })
        
        output$ets_residual_plot <- renderPlotly({
          plot_residual(predict_result)
        })
        
        output$ets_profit_plot <- renderPlotly({
          plot_profit(predict_result)
        })
        
        output$ets_model_error <- renderTable({
          predict_result$error_parameters
        })
        
        shinyjs::show(id = "ets_predict_plot", anim = TRUE,)
        shinyjs::show(id = "ets_residual_plot", anim = TRUE)
        shinyjs::show(id = "ets_profit_plot", anim = TRUE)
        shinyjs::show(id = "ets_model_error") 
      }
    )
    
    # plot
    #----
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
                   type = "scatter", mode = 'lines', name = "residuals")
      return(p)
    }
    
    plot_profit <- function(df) {
      m <- df$profits
      p <- plot_ly(m, x = ~profit, y = ~sum, type = "bar", 
                   color = ~profit=="Right", colors = ~color)
      return(p)
    }
}
