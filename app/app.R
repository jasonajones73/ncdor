# Load required libraries
library(shiny)
library(tidyverse)
library(echarts4r)

# Source modules
source("modules/filterCountyModule.R")
source("modules/echartsModule.R")

# Read data from GitHub
collections_refunds <- read_csv("https://raw.githubusercontent.com/jasonajones73/ncdor/master/data/collections_refunds.csv")

# Define UI 
ui <- fluidPage(
    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Condensed&display=swap" rel="stylesheet">
              <style>* {font-size: 100%; font-family: Roboto Condensed, sans-serif;}</style>')),
    sidebarLayout(
        sidebarPanel(
            filterDataInputsUI("county", counties = unique(collections_refunds$county))
        ), # closing sidebarPanel
        mainPanel(
            echartUI("chart")
        ) # closing mainPanel
    ) # closing sidebarLayout

) # closing fluidPage

# Define server
server <- function(input, output, session) {
    
    filt_cr <- callModule(filterDataServer, "county", df = collections_refunds)
    
    callModule(echartServer, "chart", df = filt_cr)

}

# Run the application 
shinyApp(ui = ui, server = server)
