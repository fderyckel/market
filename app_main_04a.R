# Install the required library. 
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)              ## for our interactive graphs
library(googlesheets4)       ## to read the google sheets
library(DT)

thePath <- here::here()

source('get_data.R', local = TRUE)
source('create_plotly_chart.R', local = TRUE)
source('create_ggplot_chart.R', local = TRUE)
source('wrangle_data.R', local = TRUE)

################# User Interface ####################
#####################################################
header <- dashboardHeader(
        title = "Monitoring the Market"
    )
    
sidebar <- dashboardSidebar(
        sidebarUserPanel('François de Ryckel', 
                         image = "https://avatars3.githubusercontent.com/u/7240992?s=460&v=4"), 
        sidebarMenu(
            menuItem("Stock Charts", tabName = "finan_charts", icon = icon("area-chart")), 
            menuItem("Other Charts", tabName = "other_charts", icon = icon("area-chart")), 
            menuItem("ETF", tabName = "etf", icon = icon("copy")), 
            menuItem("Journals", tabName = "journals", icon = icon("newspaper"))),  
        dateRangeInput("date_range", 
                       label = "Date Range", 
                       start = Sys.Date() - 365, end = Sys.Date()), 
        selectInput("pick_data_provider", label = "Data provider",    
                    choices = c("Yahoo", "Tiingo", "Alpha Vantage", "CBOE"), selected = "Alpha Vantage"), 
        selectInput("intervals", label = "Intervals", 
                    choices = c("5 min", "15 min", "30 min", "60 min", "Daily"), selected = "Daily"), 
        textInput("pick_instrument", label = "Write your ticker", value = "SPY", 
                  placeholder = "Any symbols supported by your data provider"), 
        actionButton(inputId = "goButton", label = "Download data", icon = icon("download")), 
        actionButton(inputId = "chartButton", label = "Graph", icon = icon("chart-area")), 
        br(), 
        HTML("App developped by <a href=https://fderyckel.github.io/aboutme/>François de Ryckel</a>."),   
        br(), 
        HTML("Ideas on how to improve it? Mail me 
               <a href=mailto:f.deryckel@gmail.com?Subject=How%20would%20you%20make%20it%20better%?> here </a>")
    )
    
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "finan_charts", 
            fluidRow(
                column(width = 12, 
                       #####################################################
                       tabBox(title = "Charts", width = NULL, selected = "Charting", 
                              #The main grah of prices
                              tabPanel(title = "Plotly Chart", width = NULL,  
                                       plotlyOutput("plotly_chart", height = "600px")), 
                              tabPanel(title = "ggplot2 Chart", width = NULL, status = "primary", 
                                       plotOutput("ggplot_chart", height = "800px"), 
                                       br(),
                                       br(),
                                       "5 years chart", 
                                       plotOutput("ggplot_longterm_chart", height = "700px")), 
                              tabPanel(title = "ETF Relative", width = NULL, 
                                       plotOutput("ggplot_etf1", height = "600px"), 
                                       br(),
                                       br(),
                                       plotOutput("ggplot_etf2", height = "600px"), 
                                       br(),
                                       br(),
                                       plotOutput("ggplot_etf3", height = "600px"), 
                                       br(),
                                       br(),
                                       DT::dataTableOutput("gs_stock_relative_etf"),
                                       actionButton("refresh_gs_etf_stock", "Refresh Sheet"), 
                                       br(), 
                                       actionButton("etfButton2", "Download all"), 
                                       br(),
                                       br(), 
                                       htmlOutput("gf_etf_stock"))
                              ), 
                       box(title = "Inputs", status = "info", width = NULL, 
                           "This is to control some additional aspect of the chart.", 
                           checkboxInput(inputId = "fibonacci", label = "Fibonacci Retracement", value = FALSE, width = NULL), 
                           textInput("numerical_chart_input_one", label = "Look back period for Fibonacci Retracement", value = 90)), 
                       box(title = "Std away from the mean", status = "warning", width = NULL, 
                           "This chart shows how many std deviaton are the moving averages from their mean", 
                           plotlyOutput("plotly_std_away"))
                       ))
        ), 
        
        tabItem(tabName = "etf", 
            fluidRow(
                box(width = 3, title = "Select your ETF criteria", status = "info", solidHeader = TRUE, 
                    uiOutput("choose_area"), 
                    uiOutput("choose_sub_area"), 
                    uiOutput("choose_country"), 
                    uiOutput("pick_etf"), 
                    actionButton(inputId = "etfButton", label = "Download data", icon = icon("download"))), 
                tabBox(title = "Charts", width = 9, 
                       tabPanel(title = "Relative Performance", width = NULL, status = "warning", 
                                plotlyOutput("plotly_etf_return", height = "600px")))), 
            
            fluidRow(
                box(Title = "List of all ETF considered", status = "warning", width = 12, 
                                  DT::dataTableOutput("gs_etf_list"))) 
        ), 
        
        tabItem(tabName = "journals", 
                fluidRow(
                    column(width = 12, 
                           tabBox(title = "Journals (from Google Sheet)", width = NULL, selected = "Chartin", 
                                  tabPanel(title = "Market Commentary", width = NULL, 
                                           DT::dataTableOutput("gs_market_comment"),
                                           actionButton("refresh_gs_mm", "Refresh Sheet"), 
                                           br(), 
                                           br(), 
                                           htmlOutput("gf_market_comment")), 
                                  tabPanel(title = "Trading Journal", width = NULL, 
                                           DT::dataTableOutput("gs_trading_journal"),
                                           actionButton("refresh_gs_tj", "Refresh Sheet"), 
                                           br(), 
                                           br(), 
                                           htmlOutput("gf_trading_journal"))))
                    
                ))
    )
)



ui <- dashboardPage(header, sidebar, body)


##################### Server ########################
#####################################################
server <- function(input, output) {
    
    ### This function output a basic df of financial data based on a ticker. 
    ### It is only done once we enter the ticker, not everytime a chart is remade
    basic_fin_df <- reactive({
        df <- conform_data(input$pick_instrument, input$intervals, input$pick_data_provider)
        df <- create_base_fin_df(df)
        return(df)
    })
    
    ### This function output a basic df of financial data based on a gs read
    ### the gs will associate with a ticker 2 etf and they do a relative comparison. 
    ticker_with_etf <- reactive({
        df <- read_sheet(gs_etf_stock_responses) %>% 
            select(ticker = "Ticker", etf1 = "Overall index ETF", etf2 = "Sector ETF", etf3 = "Industry ETF") %>% 
            filter(ticker == input$pick_instrument) %>% 
            mutate(provider = input$pick_data_provider, 
                   ticker = map2(ticker, provider, quick_read), 
                   etf1 = map2(etf1, provider, possibly(quick_read, NA)), 
                   etf2 = map2(etf2, provider, possibly(quick_read, NA)), 
                   etf3 = map2(etf3, provider, possibly(quick_read, NA)))
        return(df)
    })
    
    ########## Server - Download the data ############
    observeEvent(input$goButton, {
        if (input$pick_data_provider == "Yahoo"){
            call_yahoo_quote(input$pick_instrument)
        } else if (input$pick_data_provider == "Tiingo" & input$intervals == "Daily"){
            call_tiingo_quote(input$pick_instrument) 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "Daily"){
            call_av_quote(input$pick_instrument) 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "15 min"){
            call_av_quote_intraday(input$pick_instrument, "15min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "5 min"){
            call_av_quote_intraday(input$pick_instrument, "5min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "30 min"){
            call_av_quote_intraday(input$pick_instrument, "30min") 
        } else if (input$pick_data_provider == "Alpha Vantage" & input$intervals == "60 min"){
            call_av_quote_intraday(input$pick_instrument, "60min") 
        } else if (input$pick_data_provider == "CBOE"){
            call_cboe_pcratio()
        } 
        
    })
    
    observeEvent(input$etfButton, {
        for (i in 1:nrow(etf_list)){
            print(etf_list$ticker[i])
            if(input$pick_data_provider == "Yahoo"){
                call_yahoo_quote(etf_list$ticker[i])
            } else if(input$pick_data_provider == "Alpha Vantage"){
                call_av_quote(etf_list$ticker[i])
                Sys.sleep(13)
            } else if (input$pick_data_provider == "Tiingo"){
                call_tiingo_quote(etf_list$ticker[i])
                Sys.sleep(5)
            }
        }
    })
    
    observeEvent(input$etfButton2, {
        df <- read_sheet(gs_etf_stock_responses) %>% 
            select(ticker = "Ticker", etf1 = "Overall index ETF", etf2 = "Industry ETF", etf3 = "Sector ETF")
        yo <- c(df$ticker, df$etf1, df$etf2, df$etf3) %>% unique()
        for (i in 1:length(yo)){
            print(yo[i])
            if(input$pick_data_provider == "Yahoo"){
                call_yahoo_quote(yo[i])
            } else if(input$pick_data_provider == "Alpha Vantage"){
                call_av_quote(yo[i])
                Sys.sleep(13)
            } else if (input$pick_data_provider == "Tiingo"){
                call_tiingo_quote(yo[i])
                Sys.sleep(5)
            }
        }
    })
    
    
    ########## Server - THE candlestick chart with plotly ############
    output$plotly_chart <- renderPlotly({
        
        p1 <- create_plotly_chart(basic_fin_df(), input$date_range[1], input$date_range[2])
        
        ### Now sth about the fibonacci retracement
        ### if check box is clicked then add the line to the graph. 
        p1
    })
    
    output$plotly_std_away <- renderPlotly({
        p1 <- create_plotly_std_away(basic_fin_df(), input$date_range[1], input$date_range[2])
        p1
    })
    
    output$plotly_etf_return <- renderPlotly({
        df <- tibble(ticker = input$etf_picked)
        df <- df %>% mutate(price_data = map(ticker, 
                                             function(.x) relative_return(.x, provider = input$pick_data_provider, 
                                                                          start_date = input$date_range[1], 
                                                                          end_date = input$date_range[2])))
        df <- df %>% unnest(cols = c(price_data))
        
        p1 <- plot_ly(df, x = ~Index, y = ~adj_return, color = ~ticker, 
                      type = "scatter", mode = "lines") %>% 
            layout(xaxis = list(title = "Dates"), 
                   yaxis = list(title = "Return on Adjusted Prices"))
        
        p1
    })
    
    ########## Server - THE candlestick chart with ggplot2 ############
    #### This is a candlestick stock chart that is on the finan_charts tabItem
    output$ggplot_chart <- renderPlot({
        input$chartButton
        create_candlestick(basic_fin_df(), input$date_range[1], input$date_range[2])

    })
    
    output$ggplot_longterm_chart <- renderPlot({
        create_longterm_linechart(basic_fin_df())
        
    })
    
    output$ggplot_etf1 <- renderPlot({
        df <- ticker_with_etf()
        plot1 <- left_join(df$ticker[[1]], df$etf1[[1]], by = "Index") %>% arrange(Index) %>% 
            mutate(rela_price = Close.x / Close.y) %>% 
            select(Index, Adjusted = rela_price)
        create_linechart(plot1)
    })
    
    output$ggplot_etf2<- renderPlot({
        df <- ticker_with_etf()
        plot1 <- left_join(df$ticker[[1]], df$etf2[[1]], by = "Index") %>% arrange(Index) %>% 
            mutate(rela_price = Close.x / Close.y) %>% 
            select(Index, Adjusted = rela_price)
        create_linechart(plot1)
    })
    
    output$ggplot_etf3<- renderPlot({
        df <- ticker_with_etf()
        plot1 <- left_join(df$ticker[[1]], df$etf3[[1]], by = "Index") %>% arrange(Index) %>% 
            mutate(rela_price = Close.x / Close.y) %>% 
            select(Index, Adjusted = rela_price)
        create_linechart(plot1)
    })
    
    ########## Server - Reading the GoogleSheet within shiny app ############
    #### 
    output$gf_market_comment <- renderUI({
        tags$iframe(id = "googleform_mm",
                    src = gf_market_monitor_embedded_link,
                    width = 800,
                    height = 400,
                    frameborder = 0,
                    marginheight = 0)
    })
    
    output$gs_market_comment <- DT::renderDataTable({
        input$refresh_gs_mm
        gs_dat <- read_sheet(gs_market_monitor_responses) %>% 
            select(Date, Geography, `Tags, Tickers`, Comment) %>% 
            mutate(Date = ymd(Date)) %>% 
            arrange(desc(Date))
        
        DT::datatable(gs_dat)
    })
    
    output$gf_trading_journal <- renderUI({
        tags$iframe(id = "googleform_tj",
                    src = gf_trading_journal_embedded_link,
                    width = 900,
                    height = 400,
                    frameborder = 0,
                    marginheight = 0)
    })
    
    output$gs_trading_journal <- DT::renderDataTable({
        input$refresh_gs_tj
        gs_dat <- read_sheet(gs_trading_journal_responses) %>% 
            select(-Timestamp) %>% 
            mutate(Date = ymd(Date)) %>% 
            arrange(desc(Date))
        
        DT::datatable(gs_dat)
    })
    
    output$gf_etf_stock <- renderUI({
        tags$iframe(id = "googleform_etf",
                    src = gf_etf_stock_embedded_link,
                    width = 800,
                    height = 400,
                    frameborder = 0,
                    marginheight = 0)
    })
    
    output$gs_stock_relative_etf <- DT::renderDataTable({
        input$refresh_gs_etf_stock
        gs_dat <- read_sheet(gs_etf_stock_responses) %>% 
            select(Ticker, "Overall index ETF", "Sector ETF", "Industry ETF")
        DT::datatable(gs_dat)
    })
    
    
    ########## Server - Dealing with the ETF tab ############
    #Choose the ETF region
    output$choose_area <- renderUI({
        selectInput("area", "Choose your main focus", unique(etf_list$area))
    }) 
    
    output$choose_sub_area <- renderUI({
        if(is.null(input$area))
            return()
        
        # Get the data from the sub-area
        etfss <- etf_list %>% filter(area == input$area)
        selectInput("sub_area", "Choose your sub focus", unique(c(etfss$sub_area, "None")), selected = "None")
    })
    
    output$choose_country <- renderUI({
        if(is.null(input$area))
            return()
        
        # Get the data from the sub-area
        etfss <- etf_list %>% filter(area == input$area)
        selectInput("country", "Choose your country of interest", unique(c(etfss$country, "None")), selected = "None")
    })
    
    
    output$pick_etf <- renderUI({
        ## If missing input, return to avoid error later in function
        if(is.null(input$area))
            return()
        
        if(input$sub_area == "None"){
            etfs <- etf_list %>% 
                filter(area == input$area & country == input$country) %>% .$ticker %>% sort()
        }
        
        ## Get the data from the region
        if(input$sub_area != "None"){
        etfs <- etf_list %>% 
            filter(area == input$area & sub_area == input$sub_area) %>% .$ticker %>% sort()
        }
        
        ## Create the check box
        checkboxGroupInput("etf_picked", "Choose your particular focus", 
                           choices = etfs)
    })
    
    output$gs_etf_list <- DT::renderDataTable(
        etf_list, 
        filter = 'top'
    )
       
}


######################################################################
## End of dashboard
######################################################################
shinyApp(ui, server)