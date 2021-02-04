library(shiny)
library(shiny.semantic)

# making a marine data module
marine_ui <- function(id){
  shiny.semantic::semanticPage(

    tagList(
      dropdown_input(NS(id, "ship_type_dropdown"),
                     unique(cleaned_data$ship_type), value = "Tanker"),
      p("Selected ship type:"),
      textOutput(NS(id, "dropdown_ship_type_selected")),

      dropdown_input(NS(id, "ship_name_dropdown"),
                     unique(cleaned_data$SHIPNAME),  value = "MARINUS"),

      p("Selected Ship Name:"),
      textOutput(NS(id, "dropdown_ship_name_selected"))
    )
  )
}


marine_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    output$dropdown_ship_type_selected <- renderText({
      input$ship_type_dropdown
    })

    output$dropdown_ship_name_selected <- renderText({
      input$ship_name_dropdown
    })

  })
}

ui <- semanticPage(
  marine_ui("m1")
)

server <- function(input, output, session)(
  marine_server("m1")
)

shiny::shinyApp(ui = ui, server = server)
