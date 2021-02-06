library(shiny)
library(shiny.semantic)
library(magrittr)

options(shiny.maxRequestSize = 900*1024^2)

# save(list = ls(all = TRUE), file= "all.RData")
load("all.RData", .GlobalEnv)

# making a marine data module
marine_ui <- function(id){
  shiny.semantic::semanticPage(

    tagList(

      shiny::tags$h3("Ship Type"),
      shiny.semantic::dropdown_input(NS(id, "ship_type_dropdown"),
                                     unique(cleaned_data$ship_type), value = "Cargo"),

      shiny::tags$br(),
      shiny::tags$h3("Ship Name"),
      shiny::uiOutput(NS(id, "ship_name_dropdown")),

      shiny::tags$br(),
      shiny::tags$hr(),

      leaflet::leafletOutput(NS(id, "leafletMap"))
    )

  )
}


marine_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    output$ship_name_dropdown <- shiny::renderUI({

      ns <- session$ns
      shiny.semantic::dropdown_input(ns("ship_name_dd_selected"),
                                     unique(cleaned_data[
    (cleaned_data$ship_type == input$ship_type_dropdown), "SHIPNAME"]), value = "SANDETTIE"
      )

    })

    output$leafletMap <- leaflet::renderLeaflet({

      data_for_leaflet_map <- reactive({max_dist_travelled(cleaned_data = cleaned_data,
                                                 filter_ship_type = input$ship_type_dropdown,
                                                 filter_ship_name = input$ship_name_dd_selected)})

     leafletMapDisp <-  leaflet::leaflet() %>%
        leaflet::setView(lng = 18.9, lat = 54.8 , zoom = 7) %>%
        leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
        leaflet::addAwesomeMarkers(
          data = data_for_leaflet_map()[[2]][, (1:2)]
        ) %>% leaflet::addPolylines(lng = unlist(data_for_leaflet_map()[[2]][(1:2), 1]),
                     lat = unlist(data_for_leaflet_map()[[2]][(1:2), 2]))

     return(leafletMapDisp)

    })

  })

}




ui <- semanticPage(
  marine_ui("m1")
)

server <- shinyServer(function(input, output, session){
  marine_server("m1")
})

shiny::shinyApp(ui = ui, server = server)



































