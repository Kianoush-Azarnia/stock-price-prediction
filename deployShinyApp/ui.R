ui <- navbarPage(
  "Stock Price Prediction",
  tabPanel("Regression",
          useShinyjs(),
          sidebarLayout(
            sidebarPanel(
              h2("Regression"),
              img(src = "lr2.jpg", height = "100%", width = "100%"),
              
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
              
              tableOutput("stat_model_error"),
              
              numericInput("stat_horizon", "Forcasting ahead", value = 1, 
                           min = 1, max = 30),
              
              numericInput("stat_window_size", "window size", value = 9, 
                           min = 9, max = 30),
              
              actionButton("stat_predict_button", "Predict")
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
                  6,  plotlyOutput("stat_profit_plot") 
                ),
              )
            )
          ) 
  ),
  
  tabPanel("Exponential Smoothing",
          useShinyjs(),
          sidebarLayout(
            sidebarPanel(
              h2("Exponential smoothing"),
              img(src = "exp1.png", height = "100%", width = "100%"),
              
              selectizeInput(
                "ets_symbol_in", "Choose a symbol", choices = unique_symbols,
                options = list(
                  placeholder = "stock symbols",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              
              dateRangeInput(
                "ets_dateRange_in", "Prediction range", 
                start = start_date, end = end_date, 
                min = start_date, max = end_date,
                format = "yyyy-mm-dd", startview = "month", 
                weekstart = 6, language = "en", separator = " to ", 
                width = NULL
              ),
              
              selectizeInput(
                "ets_model_in", "Choose a model", 
                choices = ets_models,
                options = list(
                  placeholder = "predictor models",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              
              tableOutput("ets_model_error"),
              
              numericInput("ets_horizon", "Forcasting ahead", value = 1, 
                           min = 1, max = 30),
              
              selectizeInput(
                "ets_damped", "Using damp trend", 
                choices = true_false_list,
                selected = true_false_list[2]
              ),
              
              selectizeInput(
                "ets_additive", "Consider just additive models", 
                choices = true_false_list,
                selected = true_false_list[2]
              ),
              
              selectizeInput(
                "ets_multiplicative", "Allow multiplicative trend", 
                choices = true_false_list,
                selected = true_false_list[1]
              ),
              
              numericInput("ets_window_size", "window size", value = 9, 
                           min = 9, max = 30),
              
              actionButton("ets_predict_button", "Predict")
            ), 
            mainPanel(
              fluidRow(
                plotlyOutput("ets_symbol_plot"),
                
                plotlyOutput("ets_predict_plot"),
              ),
              fluidRow(
                column(
                  6, plotlyOutput("ets_residual_plot")
                ),
                column(
                  6,  plotlyOutput("ets_profit_plot") 
                ),
              )
            )
          ) 
  ),
  
  tabPanel("Deep Learning", 
          useShinyjs(),
          sidebarLayout(
            sidebarPanel(
              h2("Multilayer Perceptron"),
              img(src = "deep1.jpeg", height = "100%", width = "100%"),
              
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
                "ml_model_in", "Choose a model", choices = ml_models,
                options = list(
                  placeholder = "predictor models",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              
              tableOutput("ml_model_error"),
              
              numericInput("ml_horizon", "Forcasting ahead", value = 1, 
                           min = 1, max = 30),
              
              numericInput("ml_window_size", "window size", value = 20, 
                           min = 20, max = 50),
              
              selectizeInput(
                "dl_loss", "Loss",
                choices = dl_loss, selected = dl_loss[1]
              ),
              
              selectizeInput(
                "dl_distribution", "Distribution",
                choices = dl_distribution, selected = dl_distribution[1]
              ),
              
              selectizeInput(
                "dl_activation", "Activation",
                choices = dl_activation, selected = dl_activation[1]
              ),
              
              numericInput("ml_max_runtime_secs", "Seconds per iteration", 
                           value = 0, min = 0, max = 600),
              
              selectizeInput(
                "ml_stopping_metric", "Stopping metric",
                choices = ml_stopping_metric, selected = ml_stopping_metric[1]
              ),
              
              numericInput("ml_stopping_rounds", "Stopping rounds", 
                           value = 0, min = 0, max = 100),
              
              actionButton("ml_predict_button", "Predict")
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
  ),
  
  tabPanel("Gradient Boost Machine", 
          useShinyjs(),
          sidebarLayout(
            sidebarPanel(
              h2("Gradient boosting"),
              img(src = "gbm5.png", height = "100%", width = "100%"),
              
              selectizeInput(
                "gbm_symbol_in", "Choose a symbol", choices = unique_symbols,
                options = list(
                  placeholder = "stock symbols",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              
              dateRangeInput(
                "gbm_dateRange_in", "Prediction range", 
                start = start_date, end = end_date, 
                min = start_date, max = end_date,
                format = "yyyy-mm-dd", startview = "month", weekstart = 6,
                language = "en", separator = " to ", width = NULL
              ),
              
              selectizeInput(
                "gbm_distribution_in", "Choose a distribution", 
                choices = gbm_distributions, selected = gbm_distributions[1],
                options = list(
                  placeholder = "distribution"
                )
              ),
              
              tableOutput("gbm_model_error"),
              
              numericInput("gbm_horizon", "Forcasting ahead", value = 1, 
                           min = 1, max = 30),
              
              numericInput("gbm_ntress", "number of trees", value = 500,
                           min = 10, max = 1000, step = 10),
              
              numericInput("gbm_max_depth", "maximum tree depth", value = 20,
                           min = 0, max = 50),
              
              numericInput("gbm_learn_rate", "learn rate", value = 0.1,
                           min = 0, max = 1, step = 0.1),
              
              numericInput("gbm_window_size", "window size", value = 20, 
                           min = 20, max = 50),
              
              numericInput("gbm_max_runtime_secs", "Seconds per iteration", 
                           value = 0, min = 0, max = 600),
              
              selectizeInput(
                "gbm_stopping_metric", "Stopping metric",
                choices = ml_stopping_metric, selected = ml_stopping_metric[1]
              ),
              
              numericInput("gbm_stopping_rounds", "Stopping rounds", 
                           value = 0, min = 0, max = 100),
              
              actionButton("gbm_predict_button", "Predict")
            ), 
            mainPanel(
              fluidRow(
                plotlyOutput("gbm_symbol_plot"),
                
                plotlyOutput("gbm_predict_plot"),
              ),
              fluidRow(
                column(
                  6, plotlyOutput("gbm_residual_plot")
                ),
                column(
                  6, plotlyOutput("gbm_profit_plot")
                ),
              )
            )
          )
  ),
  
  tabPanel("Random forest", 
          useShinyjs(),
          sidebarLayout(
            sidebarPanel(
              h2("Random Forest"),
              img(src = "rf1.png", height = "100%", width = "100%"),
              
              selectizeInput(
                "rf_symbol_in", "Choose a symbol", choices = unique_symbols,
                options = list(
                  placeholder = "stock symbols",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              
              dateRangeInput(
                "rf_dateRange_in", "Prediction range", 
                start = start_date, end = end_date, 
                min = start_date, max = end_date,
                format = "yyyy-mm-dd", startview = "month", weekstart = 6,
                language = "en", separator = " to ", width = NULL
              ),
              
              selectizeInput(
                "rf_distribution_in", "Choose a distribution", 
                choices = rf_distributions, selected = rf_distributions[1],
                options = list(
                  placeholder = "distribution"
                )
              ),
              
              tableOutput("rf_model_error"),
              
              numericInput("rf_horizon", "Forcasting ahead", value = 1, 
                           min = 1, max = 30),
              
              numericInput("rf_ntress", "number of trees", value = 500,
                           min = 10, max = 1000, step = 10),
              
              numericInput("rf_max_depth", "maximum tree depth", value = 20,
                           min = 0, max = 50),
              
              numericInput("rf_window_size", "window size", value = 20, 
                           min = 20, max = 50),
              
              numericInput("rf_max_runtime_secs", "Seconds per iteration", 
                           value = 0, min = 0, max = 600),
              
              selectizeInput(
                "rf_stopping_metric", "Stopping metric",
                choices = ml_stopping_metric, selected = ml_stopping_metric[1]
              ),
              
              numericInput("rf_stopping_rounds", "Stopping rounds", 
                           value = 0, min = 0, max = 100),
              
              actionButton("rf_predict_button", "Predict")
            ), 
            mainPanel(
              fluidRow(
                plotlyOutput("rf_symbol_plot"),
                
                plotlyOutput("rf_predict_plot"),
              ),
              fluidRow(
                column(
                  6, plotlyOutput("rf_residual_plot")
                ),
                column(
                  6, plotlyOutput("rf_profit_plot")
                ),
              )
            )
          )
  )
)