# Shiny web app


library(shiny)
library(shiny.semantic)

# Define UI for application that draws a histogram

ui <- shiny.semantic::semanticPage(
    title = "My Page",
    div(class = "ui button", icon("user"), "Icon button")
)

server <- function(input, output){

}

shinyApp(ui, server)
