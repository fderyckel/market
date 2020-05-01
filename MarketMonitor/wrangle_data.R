library(dplyr)
library(purrr)
library(readr)
library(glue)
library(lubridate)

thePath <- here::here()

### This function is to deal with intraday data and stich all the files into one df
######################################################################################
stitch_intraday <- function(ticker, interval){
  # First step: collect all the files name into one df (variable = file_name)
  file_list <- tibble(file_name = list.files(glue({thePath}, "/data_stock_av_", {interval}), pattern = "\\.csv", full.names=TRUE)) %>% 
    ## Step 2: create a second variable.  Stripping the file name to only get the ticker
    mutate(ticker_file = gsub("^(?:[^/]*/)*", "", file_name)) %>% 
    mutate(ticker_file = gsub("_.*", "", ticker_file)) %>% 
    ## step 3: filtering base on the ticker. 
    filter(ticker_file == ticker)
  
  df <- map_df(file_list$file_name, read_csv, col_names=TRUE) %>% 
    distinct()
  
  return(df)
}

### Function to harmonize the data.   
### Are the data from Yahoo or AV?  Are there intraday data or EoD data?  
######################################################################################
conform_data <- function(ticker, interval, provider){
  if (interval == "Daily" & provider == "Yahoo"){
    df <- read_csv(paste0(thePath, "/data_stock_ya/", ticker, ".csv")) %>% na.omit()
  } else if (interval == "Daily" & provider == "Alpha Vantage"){
    df <- read_csv(paste0(thePath, "/data_stock_av/", ticker, ".csv")) %>% na.omit() %>% 
      mutate(Adjusted = Close)
  } else if (interval == "15 min" & provider == "Alpha Vantage"){
    df <- stitch_intraday(ticker, interval = "15min") %>% 
      mutate(Adjusted = Close)
  } else if (interval == "Daily" & provider == "Tiingo"){
    df <- read_csv(paste0(thePath, "/data_stock_ti/", ticker, ".csv")) %>% na.omit() %>% 
      # The Adjusted variabel is names Adj_Close by Tiingo => need to rename it to confirm to the data. 
      mutate(Adjusted = Adj_Close)
  }
  return(df)
}

### This function will create a df that would be a base for all plotting activity. 
### Reason is that there is a lot of reuse of code with moving average, adx, etc.  
### Better to refactor the code for all basic use and each plotting function will re-use that code. 
### The function will take its arguments (and initial df) from a conform_data fonction. 
create_base_fin_df <- function(df){
  df <- df %>% na.omit() %>% filter(Open != "null" & Close != 0 & Adjusted != 0) %>% 
    select(Index, Open, High, Low, Close, Adjusted, Volume) %>% arrange(Index) %>% 
    mutate(Open = as.numeric(Open), High = as.numeric(High), Low = as.numeric(Low), 
           Close = as.numeric(Close), Adjusted = as.numeric(Adjusted))

  #for the ADX
  adx <- TTR::ADX(df[,3:5], n = 13) %>% as_tibble() %>% select(-DX)
  df <- bind_cols(df, adx)
  macd <- TTR::MACD(df$Adjusted) %>% as_tibble()
  colnames(macd) <- c("MACD_line", "MACD_signal")
  df <- bind_cols(df, macd)
  
  # Adding the other variables such a moving averages and relative strength index
  df <- df %>% 
    mutate(sma200 = TTR:: SMA(Close, 200), 
           sma200_perc= (Adjusted / sma200) - 1,                      
           roll_sd_sma200 = RollingStd(sma200_perc, window = 1200, na_method = "ignore"), 
           roll_mean_sma200 = RollingMean(sma200_perc, window = 1200, na_method = "ignore"), 
           diff_sd_sma200 = round((sma200_perc - roll_mean_sma200) / roll_sd_sma200, 2), 
           
           sma50 = TTR::SMA(Close, 50), 
           sma50_perc = (Adjusted / sma50) - 1, 
           roll_sd_sma50 = RollingStd(sma50_perc, window = 1200, na_method = "ignore"), 
           roll_mean_sma50 = RollingMean(sma50_perc, window = 1200, na_method = "ignore"), 
           diff_sd_sma50 = round((sma50_perc - roll_mean_sma50) / roll_sd_sma50, 2),  
           
           ema20 = TTR::EMA(Close, 20), 
           
           ema9 = TTR::EMA(Close, 9), 
           ema9_perc = (Adjusted / ema9) - 1,
           roll_sd_ema9 = RollingStd(ema9_perc, window = 1200, na_method = "ignore"), 
           roll_mean_ema9 = RollingMean(ema9_perc, window = 1200, na_method = "ignore"), 
           diff_sd_ema9 = round((ema9_perc - roll_mean_ema9) / roll_sd_ema9, 2), 
           
           rsi14 = TTR::RSI(Close, 14), 
           rsi5 = TTR::RSI(Close, 5), 
           ppo_line = (TTR::EMA(Close, n = 12) - TTR::EMA(Close, n = 26)) / TTR::EMA(Close, n = 26) * 100, 
           ppo_signal = TTR::EMA(ppo_line, n = 9)) 
  
  return(df)
  
  }

### To simply read the etf and get the Closing price. 
######################################################################################
quick_read <- function(ticker, provider, interval = "Daily"){
  df <- conform_data(ticker, interval, provider) %>% select(Index, Close)
}

relative_return <- function(ticker, provider, interval = "Daily", start_date, end_date){
  df <- conform_data(ticker, interval, provider) %>% select(Index, Adjusted) %>% 
    mutate(Index = ymd(Index)) %>% arrange(Index) %>% 
    filter(Index >= start_date & Index <= end_date) %>% 
    mutate(adj_return = Adjusted / first(Adjusted) - 1)
  return(df)
}






