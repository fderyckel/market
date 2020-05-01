# Install the required library. 
library(shiny)
library(shinydashboard)
library(tibble)
library(dplyr)
library(readr)
library(plotly)

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
            menuItem("Journals", tabName = "journals", icon = icon("newspaper"))),  
        dateRangeInput("date_range", 
                       label = "Date Range", 
                       start = Sys.Date() - 365, end = Sys.Date()), 
        selectInput("pick_data_provider", label = "Data provider",    
                    choices = c("Yahoo", "Tiingo", "Alpha Vantage", "CBOE"), selected = "Yahoo"), 
        selectInput("intervals", label = "Intervals", 
                    choices = c("5 min", "15 min", "30 min", "60 min", "Daily"), selected = "Daily"), 
        textInput("pick_instrument", label = "Write your ticker", value = "SPY", 
                  placeholder = "Any symbols supported by your data provider"), 
        actionButton("goButton", "Download data")
    )
    
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "finan_charts", 
            fluidRow(
                column(width = 12, 
                       tabBox(title = "Charts", width = NULL, selected = "Charting", 
                              #The main grah of prices
                              tabPanel(title = "Plotly Chart", width = NULL,  
                                       plotlyOutput("plotly_chart", height = "600px")), 
                              tabPanel(title = "ggplot2 Chart", width = NULL, status = "primary", 
                                       plotOutput("ggplot_chart", height = "800px"))
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
        
        tabItem(tabName = "other_charts", 
            fluidRow(
                column(width = 12, 
                       tabBox(title = "Charts", width = NULL, selected = "pcratio_chart", 
                              tabPanel(title = "Put-Call Ratio", width = NULL, 
                                       plotOutput("pcratio_chart", height = "700px"))))) 
        ), 
        
        tabItem(tabName = "journals", 
                fluidRow(
                    column(width = 12, 
                           tabBox(title = "Journals (from Google Sheet)", width = NULL, selected = "Chartin", 
                                  tabPanel(title = "Market Commentary", width = NULL, 
                                           plotOutput("pcratio_chart", height = "700px")), 
                                  tabPanel(title = "Trading Journal", width = NULL, 
                                           )))
                    
                ))
    )
)



ui <- dashboardPage(header, sidebar, body)


##################### Server ########################
#####################################################
server <- function(input, output) {
    
    basic_fin_df <- reactive({
        df <- conform_data(input$pick_instrument, input$intervals, input$pick_data_provider)
        df <- create_base_fin_df(df)
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
    
    ########## Server - THE candlestick chart with ggplot2 ############
    #### This is a candlestick stock chart that is on the finan_charts tabItem
    output$ggplot_chart <- renderPlot({
        create_candlestick(basic_fin_df(), input$date_range[1], input$date_range[2])

    })

    
    
    
    ########## Server - THE Put-Call Ratio line chart with ggplot2 ############
    #### This chart is on the other_charts tabItem. 
    #### The function for this chart is in the create_ggplot_chart.R file. 
    output$pcratio_chart <- renderPlot({
        create_pcratio_chart()
    })
    
}


######################################################################
## End of dashboard
######################################################################
shinyApp(ui, server)