# Install the required library. 
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)              ## for our interactive graphs
library(googlesheets4)       ## to read the google sheets
library(DT)

thePath <- here::here()

#source('get_data.R', local = TRUE)
#source('create_plotly_chart.R', local = TRUE)
#source('create_ggplot_chart.R', local = TRUE)
#source('wrangle_data.R', local = TRUE)

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
        )


ui <- dashboardPage(header, sidebar, body)


##################### Server ########################
#####################################################
server <- function(input, output) {
    
       
}


######################################################################
## End of dashboard
######################################################################
shinyApp(ui, server)
