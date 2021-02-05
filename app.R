library(shiny)
library(shiny.semantic)

# making a marine data module
marine_ui <- function(id){
  shiny.semantic::semanticPage(
    title = "Marine Data Visualization",

    tagList(

      shiny::tags$h3("Ship Type"),
      shiny.semantic::dropdown_input(NS(id, "ship_type_dropdown"),
                                     unique(cleaned_data$ship_type)),

      shiny::tags$br(),
      shiny::tags$h3("Ship Name"),
      shiny::uiOutput(NS(id, "ship_name_dropdown")),



      leaflet::leafletOutput(NS(id, "leaflet"))
    )

  )
}


marine_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    output$ship_name_dropdown <- renderUI({

      ns <- session$ns
      shiny.semantic::dropdown_input(ns("ship_name_dd_selected"),
                                     unique(cleaned_data[
    (cleaned_data$ship_type == input$ship_type_dropdown), "SHIPNAME"]))

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



































