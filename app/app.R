# Load required libraries
library(shiny)
library(tidyverse)
library(echarts4r)

# Source modules
source("modules/filterCountyModule.R")
source("modules/echartsModule.R")
source("modules/filterArticleModule.R")

# Read data from GitHub
collections_refunds <- read_csv("https://raw.githubusercontent.com/jasonajones73/ncdor/master/data/collections_refunds.csv")
monthly_sales <- read_csv("https://raw.githubusercontent.com/jasonajones73/ncdor/master/data/monthly_sales.csv")
article_overview <- read_csv("https://raw.githubusercontent.com/jasonajones73/ncdor/master/data/article_overview.csv") %>%
    filter(article != "Total" & is.na(article) != TRUE) %>%
    rename(county = county_name)

# Define UI 
ui <- fluidPage(
    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Condensed&display=swap" rel="stylesheet">
                   <style>* {font-size: 100%; font-family: Roboto Condensed, sans-serif;}</style>')),
    fluidRow(
        column(width = 6,
               wellPanel(filterDataInputsUI("county", counties = unique(collections_refunds$county)))
               ), # closing column
        column(width = 6,
               wellPanel(filterArticleInputsUI("article", articles = unique(article_overview$article)))
               ) # closing column
        ), # closing fluidRow
    tabsetPanel(type = "tabs",
                tabPanel(title = "Collections & Refunds", icon = icon("balance-scale"),
                         fluidRow(
                             column(width = 6,
                                    echartUI("chart1")), # closing column
                             column(width = 6,
                                    echartUI("chart2")) # closing column
                         ), # closing fluidRow
                         fluidRow(
                             column(width = 6,
                                    echartUI("chart3")), # closing column
                             column(width = 6,
                                    echartUI("chart4")) # closing column
                         ) # closing fluidRow
                         ), # closing tabPanel for Collections & Refunds
                tabPanel(title = "Taxable Sales", icon = icon("money-bill"),
                         echartUI("chart5")
                         ), # closing tabPanel Taxable Sales
                tabPanel(title = "Article Overview", icon = icon("landmark"),
                         uiOutput("art_view"),
                         echartUI("chart6")
                         ) # closing tabPanel Article Overview
                ) # closing tabsetPanel
    ) # closing fluidPage

# Define server
server <- function(input, output, session) {
    
    # This is calling the module necessary to filter by county
    filt_cr <- callModule(filterDataServer, "county", df = collections_refunds)
    
    # This is calling the module necessary to filter by article
    filt_art <- callModule(filterArticleServer, "article", df = article_overview)
    
    # This is applying the county filter to the Monthly Sales data
    filt_ms <- reactive({
        monthly_sales %>%
            filter(county %in% filt_cr()$county)
    })
    
    # This is applying the county filter to the filtered article data from the
    # article filter selection module
    fin_art <- reactive({
        filt_art() %>%
            filter(county %in% filt_cr()$county)
    })
    
    # This is a series of module calls to produce the necessary charts from the 
    # chart module. Every chart is being produced from the same module
    callModule(echartServer, "chart1", df = filt_cr,
               column_name = "gross_collections", chart_name = "Gross Collections")
    
    callModule(echartServer, "chart2", df = filt_cr,
               column_name = "foreign_collections", chart_name = "Foreign Collections")
    
    callModule(echartServer, "chart3", df = filt_cr,
               column_name = "refunds", chart_name = "Refunds")
    
    callModule(echartServer, "chart4", df = filt_cr,
               column_name = "net_collections", chart_name = "Net Collections")
    
    callModule(echartServer, "chart5", df = filt_ms,
               column_name = "taxable_sales", chart_name = "Taxable Sales")
    
    callModule(echartServer, "chart6", df = fin_art,
               column_name = "distributable_proceeds", chart_name = "Distributable Proceeds")
    
    # This is a bit of explanatory reactive UI that is being injected into the 
    # article overview tab to make sure users know what they are looking at
    output$art_view <- renderUI({
        art_text <- unique(filt_art()$article)
        
        tags$h4(HTML(paste("You have selected and are viewing data for Article", art_text)))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
