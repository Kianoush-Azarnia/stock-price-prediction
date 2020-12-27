library("shiny")
library("dplyr")
library("plotly")

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
                "dateRange_in", "Training data", 
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
            
            actionButton("predictButton", "Predict"),
            
            
        ), mainPanel(
            plotlyOutput("stock_plot"),
            textOutput("test1")
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
    
    output$stock_plot <- renderPlotly({
        stock_df() %>% plot_ly(x = ~date, y = ~final_price, mode = "lines")
    })
    
    do_predict <- reactive({
        validate(need(input$model_in != "", "Please choose a model"))
        switch(
            input$model_in, 
            "gbm" = "gbm$",
            "ets" = "ets$",
            "dl" = "dl$",
            "rf" = "rf$",
            "reg" = "reg$",
            "multireg" = "multi$",
            "dlgrid" = "gird$"
        )
    })
    
    output$test1 <- renderText({
        input$predictButton
        isolate(do_predict())
    })
    
}

shinyApp(ui, server)



