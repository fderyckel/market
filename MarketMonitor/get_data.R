####################  Getting Data ####################

#### This file is called by app_main_xx.R 
#### Its purpose is to store the functions that will get financial data from various providers. 
#### We have data from Yahoo, Alpha Vantage


### loading libraries 
library(glue)
library(lubridate)
library(readr)
library(httr)
library(googlesheets4)
thePath <- here::here()

gs_etf_list <- "https://docs.google.com/spreadsheets/d/1tAnPTwJaewrzHCELQezo64CPjFG9gDxntoSmqdePd10/edit?usp=sharing"

gf_market_monitor_embedded_link <- "https://docs.google.com/forms/d/e/1FAIpQLSck1hN6Sv57UQZ3J3ukDZDVRbOY_m3Wb5o_y5cD-93A90FtDg/viewform?embedded=true"
gs_market_monitor_responses <- "https://docs.google.com/spreadsheets/d/13fCZewrEpfdtCkBK2VqyXWI6ZENWh71uvcriBMluJVg/edit?usp=sharing"

gf_trading_journal_embedded_link <- "https://docs.google.com/forms/d/e/1FAIpQLSdNjxwLNAHZtUOdeEWiUJFPhVg5ohnVMxc81TGim_0QMTHJzw/viewform?embedded=true"
gs_trading_journal_responses <- "https://docs.google.com/spreadsheets/d/10vItUW61s3a8qJvL4r--HievW5YZ6svfpXnLX1yeoiY/edit?usp=sharing"

gf_etf_stock_embedded_link <- "https://docs.google.com/forms/d/e/1FAIpQLSdBBtHcFNoeLgBtd4_lW7d6Nqwq5TT4SVk1GlsFC7TzQ67Vbw/viewform?embedded=true"
gs_etf_stock_responses <- "https://docs.google.com/spreadsheets/d/1P4jq437yJTCluZds2s6ns0y0sITGC6Qm9ekNYvRV9iY/edit?usp=sharing"



######## Immediately read the list of etf ##########
######## IT only needs to do it once, no reactivity here #########
etf_list <- read_sheet(gs_etf_list) %>% 
  select(ticker = Ticker, area = Area, sub_area = `Sub-area`, country = Country, 
         name = Name, expense_ratio = `Expense Ratio`, 
         first_sector = `First Sector`, second_sector = `Second Sector`, 
         third_sector = `Third Sector`) %>% 
  as_tibble()



######## Yahoo API ##########
call_yahoo_quote <- function(ticker, start = "1990-01-01", end = today()){
  
  ## Getting the date in a format Yahoo understands. A bit flunky but don't know how to make it better. 
  start = as.POSIXct(as.Date(start, origin = "1970-01-01")) 
  start = trunc(as.numeric(start))
  end = as.POSIXct(as.Date(end, origin = "1970-01-01")) 
  end = trunc(as.numeric(end))
  
  # Yahoo API works with straight http request and a .csv as output ;-) 
  url = "https://query1.finance.yahoo.com/v7/finance/download/"
  request = glue({url}, {ticker}, "?period1=", {start}, "&period2=", {end}, "&interval=1d&events=history")
  
  # putting the url to read_csv as submitting the url with give a .csv
  fin_data = read_csv(request)
  
  # rename the columns to get consistency between data providers for later use 
  colnames(fin_data) <- c("Index", "Open", "High", "Low", "Close", "Adjusted", "Volume")
  
  # save the file in our local database 
  write_csv(fin_data, glue({thePath}, "/data_stock_ya/", {ticker}, ".csv"))
}

######## Tiingo API #########
call_tiingo_quote <- function(ticker, start = "1990-01-01", end = today()){
  print(glue("Checking on ", {ticker}))
  fin_data <- GET(paste0("https://api.tiingo.com/tiingo/daily/", ticker, "/prices?startDate=", 
                         start, "&token=2e677f550418d4e5f5e2b73bd24104c92f820cfd&format=csv")) %>% 
    content() %>% as_tibble()
  
  colnames(fin_data) <- c("Index", "Close", "High", "Low", "Open", "Volume", 
                             "Adj_Close", "Adj_High", "Adj_Low", "Adj_Open", "Adj_Volume", "divCash", "splitFactor")
  
  write_csv(fin_data, glue({thePath}, "/data_stock_ti/", {ticker}, ".csv"))
  print(glue("Downloaded and saved ", {ticker}))
}

######## Alpha Vantage API #########
call_av_quote <- function(ticker){
  api_key = "W7SHG93NFG5YWE2K"
  
  print(glue("Checking on ", {ticker}))
  # AV API works with straight http request and a .csv as output ;-) 
  url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="
  request = glue({url}, {ticker}, "&outputsize=full", "&apikey=", {api_key}, "&datatype=csv")
  
  # putting the url to read_csv as submitting the url with give a .csv
  fin_data = read_csv(request)
  
  # rename the columns to get consistency between data providers for later use 
  colnames(fin_data) <- c("Index", "Open", "High", "Low", "Close", "Volume")
  
  # save the file in our local database 
  write_csv(fin_data, glue({thePath}, "/data_stock_av/", {ticker}, ".csv"))
  print(glue("Downloaded and saved ", {ticker}))
}

call_av_quote_intraday <- function(ticker, intervvval){
  interval = intervvval
  api_key = "W7SHG93NFG5YWE2K"
 
  # AV API works with straight http request and a .csv as output ;-) 
  url = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol="
  request = glue({url}, {ticker}, "&interval=", {interval}, "&apikey=", {api_key}, "&datatype=csv")
  
  # putting the url to read_csv as submitting the url with give a .csv
  fin_data = read_csv(request)
  
  # rename the columns to get consistency between data providers for later use 
  colnames(fin_data) <- c("Index", "Open", "High", "Low", "Close", "Volume")
  
  # save the file in our local database 
  write_csv(fin_data, glue({thePath}, "/data_stock_av_", {interval}, "/", {ticker}, "_", today(), ".csv"))
  
}


