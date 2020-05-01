library(ggplot2)
library(dplyr)
library(lubridate)        # to deal with dates
library(gridExtra)        # to stack the charts
library(scales)           # again to deal with dates but on the x-axis
library(bdscale)          # to remove the weekends using the scale_x_bd
library(RollingWindow)


############################################################################################################
############################################################################################################
#################################### CreateGGPLOT candlestick chart ########################################


geom_candlestick <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE,
                             colour_up = "gray30", colour_down = "gray30",
                             fill_up = "green3", fill_down = "red",
                             ...) {
  
  linerange <- ggplot2::layer(
    stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  rect <- ggplot2::layer(
    stat = StatRectCS, geom = GeomRectCS, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  
  
  list(linerange, rect)
}

StatLinerangeBC <- ggplot2::ggproto("StatLinerangeBC", Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),
                                    
                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             colour_up, colour_down) {
                                      
                                      data <-  data %>%
                                        dplyr::mutate(colour = ifelse(open < close, colour_up, colour_down))
                                      
                                      tibble::tibble(x = data$x,
                                                     ymin = data$low,
                                                     ymax = data$high,
                                                     colour = data$colour)
                                    })

StatRectCS <- ggplot2::ggproto("StatRectCS", Stat,
                               required_aes = c("x", "open", "high", "low", "close"),
                               
                               compute_group = function(data, scales, params,
                                                        fill_up, fill_down,
                                                        colour_up, colour_down) {
                                 
                                 data <-  data %>%
                                   dplyr::mutate(fill = ifelse(open < close, fill_up, fill_down),
                                                 ymin = ifelse(open < close, open, close),
                                                 ymax = ifelse(open < close, close, open))
                                 
                                 tibble::tibble(xmin = data$x - 0.45,
                                                xmax = data$x + 0.45,
                                                ymin = data$ymin,
                                                ymax = data$ymax,
                                                fill = data$fill)                               })

GeomRectCS <- ggproto("GeomRectCS", GeomRect,
                      default_aes = aes(colour = NA,
                                        size = 0.5,
                                        linetype = 1,
                                        alpha = NA))

GeomLinerangeBC <- ggproto("GeomLinerangeBC", GeomLinerange,
                           default_aes = aes(size = 0.5,
                                             linetype = 1,
                                             alpha = NA))


create_candlestick <- function(df, start_date = today()-252, end_date = today()){
  df2 <- df %>% filter(Index >= start_date & Index <= end_date)
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=Index, y = Close)) + 
    geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) + 
    geom_line(aes(y = ema9), colour = "red", size = 0.2) + 
    geom_line(aes(y = sma200), colour = "darkorchid1", size = 0.3) + 
    geom_line(aes(y = sma50), colour = "Turquoise 1", size = 0.3) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b %y"), expand = c(0,0.3)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"),       # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor.y = element_line(colour = "white", linetype = "dotted", size = 0.15),
          panel.grid.minor.x = element_blank())
  
  # graphing of the ppo part.  
  p2 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = ppo_signal, colour = "darkorchid1"), size = 0.4) + 
    geom_line(aes(y = ppo_line, colour = "Royal Blue 1"), size = 0.5) + 
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed", size = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("PPO") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  p4 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = DIp), colour = "Turquoise 1", size = 0.2) + 
    geom_line(aes(y = DIn), colour = "red", size = 0.2) + 
    geom_line(aes(y = ADX), colour = "Gray 70", size = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("ADX") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  p3 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = rsi14, colour = "Dark Orange")) + 
    geom_line(aes(y = rsi5, colour = "Gray 80"), linetype = "dotted", size = 0.4) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("RSI") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.4), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.1), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  yo <- grid.arrange(p1, p2, p4, p3, ncol = 1, heights = c(2.5, 1, 1, 1))
  yo
}

create_longterm_linechart <- function(df, start_date = today()-365*5, end_date = today()){
  df2 <- df %>% 
    filter(Index >= start_date) %>% filter(Index <= end_date) %>% na.omit()
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=Index, y = Adjusted)) + 
    geom_line(color = "Gray 70") + 
    geom_line(aes(y = sma200), colour = "darkorchid1", size = 0.3) + 
    geom_line(aes(y = sma50), colour = "Turquoise 1", size = 0.3) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 30,
               labels=date_format("%b '%y"), expand = c(0,0.3)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 30, vjust = 0.5), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"), # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  # graphing of the ppo part.  
  p2 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = ppo_signal, color = "darkorchid1"), size = 0.4) + 
    geom_line(aes(y = ppo_line, color = "Royal Blue 1"), size = 0.5) + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b %y"), expand = c(0,0.1)) + 
    ylab("PPO") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(color = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  p3 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = rsi14, colour = "Dark Orange")) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 30, labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("RSI") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_blank(), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  
  grid.arrange(p1, p2, p3, ncol = 1, heights = c(2, 1, 1))
  
}

create_linechart <- function(df, start_date = today()-400, end_date = today()){
  df$Index <- ymd(df$Index)
  
  # Adding the other variables such a moving averages and relative strength index
  df2 <- df %>% na.omit() %>% arrange(Index) %>% 
    mutate(sma200 = TTR:: SMA(Adjusted, 200), 
           sma50 = TTR::SMA(Adjusted, 50), 
           ema20 = TTR::EMA(Adjusted, 20), 
           ema9 = TTR::EMA(Adjusted, 9), 
           rsi14 = TTR::RSI(Adjusted, 14), 
           rsi5 = TTR::RSI(Adjusted, 5)) %>% 
    filter(Index >= start_date) %>% filter(Index <= end_date) %>% na.omit()
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=Index, y = Adjusted)) + 
    geom_line(color = "Gray 70") + 
    geom_line(aes(y = ema9), colour = "red", size = 0.2) + 
    geom_line(aes(y = ema20), colour = "blue", size = 0.2) + 
    geom_line(aes(y = sma200), colour = "darkorchid1", size = 0.3) + 
    geom_line(aes(y = sma50), colour = "Turquoise 1", size = 0.3) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%d %b '%y"), expand = c(0,0.3)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"),       # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  
  p2 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = rsi14, colour = "Dark Orange")) + 
    geom_line(aes(y = rsi5, colour = "Gray 80"), linetype = "dotted", size = 0.4) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, 
               labels=date_format("%d %b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("RSI") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.5), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(colour = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(colour = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  
  grid.arrange(p1, p2, ncol = 1, heights = c(3, 1))
  
}


