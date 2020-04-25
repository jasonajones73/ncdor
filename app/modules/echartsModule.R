library(shiny)
library(tidyverse)
library(echarts4r)

e_common(font_family = "Roboto Condensed")

# counties object should be passed as a distinct character vector
echartUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    echarts4rOutput(ns("chart"))
    )
  
}

echartServer <- function(input, output, session, df, column_name, chart_name) {
  
  output$chart <- renderEcharts4r({
    new_dat <- df() %>%
      rename(series = all_of(column_name))
    
    new_dat %>%
      e_charts(date) %>%
      e_line(serie = series, name = chart_name, smooth = TRUE) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(style = "currency")) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(splitLine = FALSE) %>%
      e_legend(show = FALSE) %>%
      e_color(color = "#455a64") %>%
      e_title(text = chart_name, 
              subtext = sprintf("%s County, North Carolina", unique(new_dat$county))) %>%
      e_toolbox_feature(feature = "saveAsImage", title = "Download as Image")
  })
  
}