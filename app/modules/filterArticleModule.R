library(shiny)

# counties object should be passed as a distinct character vector
filterArticleInputsUI <- function(id, articles) {
  ns <- NS(id)
  
  tagList(
    selectInput(inputId = ns("article"),
                label = "Please select the article of interest:",
                choices = articles,
                selected = "39",
                selectize = TRUE
    )
  )
  
}

filterArticleServer <- function(input, output, session, df) {
  
  dat <- reactive({
    df %>%
      filter(article == input$article)
  })
  
  return(reactive({ dat() }))
  
}