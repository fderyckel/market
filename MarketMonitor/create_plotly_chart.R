####################  Create Plotly chart ####################

#### This file is called by shiny app_main_xx.R 
#### Its purpose is to store the functions that plot charts using the plotly library. 

thePath <- here::here()



########  Functions to create the main candlestic chart. 
######################################################################################
create_plotly_chart <- function(df, start_date = today() - 365, end_date = today()) {
  df <- df %>% 
    filter(Index >= start_date & Index <= end_date)
  
  p1 <- plot_ly(data = df, 
                x = ~Index, type = "candlestick", 
                open = ~Open, close = ~Close, high = ~High, low = ~Low) %>% 
    add_lines(x = ~Index, y = ~ema9, name = "EMA9", line = list(color = "rgb(230,39,28)", width = 2), inherit = F) %>% 
    add_lines(x = ~Index, y = ~ema20, name = "EMA20", line = list(color = "rgb(16,52,166)", width = 2), inherit = F) %>% 
    add_lines(x = ~Index, y = ~sma50, name = "SMA50", line = list(color = "rgb(79,174,66)", width = 2, dash = "dot"), inherit = F) %>% 
    add_lines(x = ~Index, y = ~sma200, name = "SMA200", line = list(color = "rgb(151,81, 165)", width = 2, dash = "dot"), inherit = F) %>% 
    layout(xaxis = list(title = "Date", rangeslider = list(visible = F)), 
           yaxis = list(title = "Adjusted Prices", type = "log"), 
           showlegend = FALSE)
  
  p1
}


create_plotly_std_away <- function(df, start_date = today() - 365, end_date = today()) {
  df <- df %>% select(Index, diff_sd_ema9, diff_sd_sma50, diff_sd_sma200) %>% 
    mutate(ema9 = as.numeric(diff_sd_ema9), sma50 = as.numeric(diff_sd_sma50), sma200 = as.numeric(diff_sd_sma200)) %>% 
    filter(Index >= start_date & Index <= end_date)
  
  #  %>%   gather(key = "variable", value = "value", -Index) 
  plot_ly(df, x = ~Index) %>% 
    add_trace(y = ~ema9, name = "EMA9", type = "scatter", mode = 'lines', line = list(color = "rgb(230,39,28)", width = 1)) %>% 
    add_trace(y = ~sma50, name = "SMA50", type = "scatter", mode = 'lines', line = list(color = "rgb(79,174,66)", width = 2)) %>% 
    add_trace(y = ~sma200, name = "SMA200", type = "scatter", mode = 'lines', line = list(color = "rgb(151,81, 165)", width = 2)) %>% 
    layout(xaxis = list(title = "Date"), 
           title = "How many std away is the MA from its latest mean", 
           showlegend = FALSE)
}