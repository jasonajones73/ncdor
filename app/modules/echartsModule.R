library(shiny)
library(echarts4r)

e_common(font_family = "Roboto Condensed")

# counties object should be passed as a distinct character vector
echartUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    echarts4rOutput(ns("chart"))
    )
  
}

echartServer <- function(input, output, session, df) {
  
  output$chart <- renderEcharts4r({
    chart_dat <- df()
    
    chart_dat %>%
      e_charts(date) %>%
      e_line(serie = gross_collections, name = "Gross Collections", smooth = TRUE) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter(style = "currency")) %>%
      e_y_axis(show = FALSE) %>%
      e_x_axis(splitLine = FALSE) %>%
      e_legend(show = FALSE) %>%
      e_color(color = "#455a64") %>%
      e_title(text = "Gross Collections", 
              subtext = sprintf("%s County, North Carolina", unique(chart_dat$county)))
  })
  
}