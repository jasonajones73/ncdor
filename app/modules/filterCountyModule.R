library(shiny)

# counties object should be passed as a distinct character vector
filterDataInputsUI <- function(id, counties) {
  ns <- NS(id)
  
  tagList(
    selectInput(inputId = ns("county"),
                label = "Please select the county of interest:",
                choices = counties,
                selected = "Guilford",
                selectize = TRUE
                )
    )
  
}

filterDataServer <- function(input, output, session, df) {
  
  dat <- reactive({
    df %>%
      filter(county == input$county)
  })

  return(reactive({ dat() }))
  
}