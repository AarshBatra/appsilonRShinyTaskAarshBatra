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
      textOutput(NS(id, "dropdown_ship_name_selected")),
      tags$br(),
      textOutput(NS(id, "test")),

      leafletOutput(NS(id, "leaflet"))
    )

  )
}


marine_server <- function(id, df){
  shiny::moduleServer(id, function(input, output, session){
    output$dropdown_ship_type_selected <- renderText({
      input$ship_type_dropdown
    })

    ship_type <- reactive({input$ship_type_dropdown})

    output$dropdown_ship_name_selected <- renderText({
      input$ship_name_dropdown
    })

    updatedData <- reactive({
      filter(df(), ship_type == ship_type()) # nrow(df()) works
    })


    output$test <- renderText({
      paste(updatedData()$SHIPNAME) # paste(updatedData()) works
    })

    output$leaflet <- renderLeaflet({

    })

    # update_dropdown_input(session, "ship_name_dropdown",
    #                       choices = updatedData()$SHIPNAME, value = "foo")

    # observeEvent(input$ship_type_dropdown, {
    #   updated_data <- dplyr::filter(data(), ship_type == input$ship_type_dropdown)
    #   choices <- updated_data$SHIPNAME
    #   update_dropdown_input(session, "ship_name_dropdown",
    #             choices = choices, value = input$ship_name_dropdown)
    #
    # })

  })
}

ui <- semanticPage(
  marine_ui("m1")
)

server <- function(input, output, session)(
  marine_server("m1", reactive({cleaned_data}))
)

shiny::shinyApp(ui = ui, server = server)



































