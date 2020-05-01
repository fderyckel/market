


# Install the required library. 
library(shiny)
library(shinydashboard)
library(RollingWindow)
library(tibble)
library(dplyr)
library(plotly)

thePath <- here::here()

source('getting_data.R', local = TRUE)
source('create_ggplot_chart.R', local = TRUE)

################# User Interface ####################
#####################################################
header <- dashboardHeader(
        title = "Monitoring the Market"
    )
    
sidebar <- dashboardSidebar(
        sidebarUserPanel('François de Ryckel', 
                         image = "https://avatars3.githubusercontent.com/u/7240992?s=460&v=4"), 
        sidebarMenu(
            menuItem("Charts", tabName = "finan_charts", icon = icon("area-chart"))),  
        dateRangeInput("date_range", 
                       label = "Date Range", 
                       start = Sys.Date() - 365, end = Sys.Date()), 
        selectInput("pick_data_provider", label = "Data provider",    
                    choices = c("Yahoo", "Alpha Vantage", "IEX"), selected = "Yahoo"), 
        selectInput("intervals", label = "Intervals", 
                    choices = c("5 min", "15 min", "30 min", "60 min"), selected = "60 min"), 
        textInput("pick_instrument", label = "Write your ticker", value = "SPY", 
                  placeholder = "Any symbols supported by your data provider"), 
        actionButton("goButton", "Download data")
    )
    
body <- dashboardBody(
    tabItem(tabName = "finan_charts", 
            fluidRow(
                column(width = 12, 
                       tabBox(title = "Charts", width = NULL, selected = "Charting", 
                              #The main grah of prices
                              tabPanel(title = "Plotly Chart", width = NULL,  
                                       plotlyOutput("plotly_chart", height = "600px")), 
                              tabPanel(title = "ggplot2 Chart", width = NULL, status = "primary", 
                                       plotOutput("ggplot_chart", height = "800px"))
                              )))
    )
)



ui <- dashboardPage(header, sidebar, body)


##################### Server ########################
#####################################################
server <- function(input, output) {
    
    
    ########## Server - Download the data ############
    observeEvent(input$goButton, {
        if (input$pick_data_provider == "Yahoo"){
            call_yahoo_quote(input$pick_instrument)
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "15 min"){
            call_av_quote_intraday(input$pick_instrument, "15min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "5 min"){
            call_av_quote_intraday(input$pick_instrument, "5min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "30 min"){
            call_av_quote_intraday(input$pick_instrument, "30min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "60 min"){
            call_av_quote_intraday(input$pick_instrument, "60min") 
        } 
        
    })
    
    ########## Server - THE candlestick chart with plotly ############
    output$plotly_chart <- renderPlotly({
        df_etf <- read_csv(paste0(thePath, "/stock_data_ya/", input$pick_instrument, ".csv")) %>% na.omit()
        df_etf$Index <- ymd(df_etf$Index)
        df_etf <- df_etf %>% filter(Adjusted != 0)
        ## Create MACD
        df2 <- TTR::MACD(df_etf$Adjusted) %>% as_tibble()
        colnames(df2) <- c("MACD_line", "MACD_signal")
        df_etf <- bind_cols(df_etf, df2)
        ## Create RSI and MA, then filter for dates
        df_etf <- df_etf %>% mutate(rsi14 = TTR::RSI(df_etf$Close, n= 14), 
                                    sma50 = round(TTR::SMA(df_etf$Close, n = 50), 2), 
                                    sma200 = round(TTR::SMA(df_etf$Close, n = 200), 2), 
                                    ema9 = round(TTR::EMA(df_etf$Close, n = 9), 2)) %>% 
            filter(Index >= input$date_range[1] & Index <= input$date_range[2])
        
        p1 <- plot_ly(data = df_etf, 
                      x = ~Index, type = "candlestick", 
                      open = ~Open, close = ~Close, high = ~High, low = ~Low) %>% 
            add_lines(x = ~Index, y = ~ema9, name = "EMA9", line = list(color = "rgb(230,39,28)", width = 2), inherit = F) %>% 
            add_lines(x = ~Index, y = ~sma50, name = "SMA50", line = list(color = "rgb(79,174,66)", width = 2, dash = "dot"), inherit = F) %>% 
            add_lines(x = ~Index, y = ~sma200, name = "SMA200", line = list(color = "rgb(151,81, 165)", width = 2, dash = "dot"), inherit = F) %>% 
            layout(xaxis = list(title = "Date", rangeslider = list(visible = F)), 
                   yaxis = list(title = "Adjusted Prices", type = "log"), 
                   showlegend = FALSE)
        
        p1
    })
    
    ########## Server - THE candlestick chart with ggplot2 ############
    output$ggplot_chart <- renderPlot({
        yo <- input$pick_instrument
        create_candlestick(yo)

    })
    
}


######################################################################
## End of dashboard
######################################################################
shinyApp(ui, server)