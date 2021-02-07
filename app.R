## Appsilon R Shiny Task master script file: Aarsh Batra=======================

# metadata---------------------------------------------------------------------
# author: Aarsh Batra
# Start Date: February 2, 2021
# R version: 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)
# R Studio version: 1.4.1103
# e-mail: aarshbatra.in@gmail.com

# libraries--------------------------------------------------------------------
library(shiny)
library(shiny.semantic)
library(magrittr)
library(stringr)
library(testthat)

# setting global options
options(shiny.maxRequestSize = 900*1024^2)
options(semantic.themes =  TRUE)

# loading the .RData file that contain all required data objects
# save(list = ls(all = TRUE), file= "all.RData")
load("all.RData", .GlobalEnv)

# running tests from the inst/tests/tests.R file (uses 'testthat' package)
# I tested this before deploying, all tests were passed. But, because the
# 'test_file' function takes a local file path, I have commented it before
# deployment to R Shiny, to avoid local path errors.

# testthat::test_file(path = "inst/tests/tests.R")



# making a marine data ui module-----------------------------------------------

# Contains a header followed by a card, a map, and 2 dropdown menus.
# "Ship Name" dropdown menu list gets automatically updated, once a value for
# "Ship Type" is selected. The Map gets rendered automatically.
marine_ui <- function(id){
  shiny.semantic::semanticPage(
  tagList(

      shiny::tags$h1("Ships Travel Distance Visualizer"),

      card(div(class = "content", div(class = "header",
      "Created by: Aarsh Batra"), div(class = "meta", "For the ship selected,
 below map displays the markers corresponding to the 2 consecutive
 observations between which the ship sailed the longest distance."),
  div(class = "description", "If you found bugs, email me at:
      aarshbatra.in@gmail.com, I will fix it and get back to you."))),

      shiny::tags$hr(),

      shiny::tags$h3("Please wait, the map will be displayed below in 1 to 2
        seconds. If you see an error below, wait for a second it will go away
        and the map will be displayed. Refresh page if map does not display."),
      shiny::tags$br(),
      leaflet::leafletOutput(NS(id, "leafletMap")),
      shiny::tags$h3("Hover the mouse pointer over the pins dropped on
      map to get more information, e.g. distance travelled
    (in meters), etc. If viewing on mobile, click over the pins dropped on map
      to get the information on distance travelled."),

  shiny::tags$h3("Note: This initial error that you might see does not
                     affect the functionality of the app. The dropdowns and
                     all maps work perfectly. But, I am trying to fix this
                     intital error (that vanishes a second after app loads).
                     I think it has something to do with time lag with which
                     reactive objects gets rendered on Shiny.
                     I will fix this."),
       shiny::tags$br(),
      div(class = "ui horizontal divider", "Select your ship"),

      shiny::tags$h3("Ship Type"),
      shiny.semantic::dropdown_input(NS(id, "ship_type_dropdown"),
                        unique(cleaned_data$ship_type), value = "Cargo"),

      shiny::tags$br(),
      shiny::tags$h3("Ship Name"),
      shiny::uiOutput(NS(id, "ship_name_dropdown"))
    )

  )
}

# marine data server module----------------------------------------------------
marine_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){

    # Automatically update "ship_name" dropdown menu list once
    # "ship_type" is selected.
    output$ship_name_dropdown <- shiny::renderUI({

      ns <- session$ns
      shiny.semantic::dropdown_input(ns("ship_name_dd_selected"),
  unique(cleaned_data[(cleaned_data$ship_type == input$ship_type_dropdown),
                      "SHIPNAME"]),
  value = unique(
  cleaned_data[(cleaned_data$ship_type == input$ship_type_dropdown),
               "SHIPNAME"])[[1]][1]
      )

  })

    # render the leaflet map
    output$leafletMap <- leaflet::renderLeaflet({

    data_for_leaflet_map <- shiny::reactive({max_dist_travelled(
     cleaned_data = cleaned_data, filter_ship_type = input$ship_type_dropdown,
      filter_ship_name = input$ship_name_dd_selected)})

      icons <- leaflet::awesomeIcons(
        icon = 'fa-ship',
        iconColor = 'black',
        library = 'fa'
      )

  leafletMapDisp <-  leaflet::leaflet(padding = 4) %>%
  leaflet::setView(
    lng = unlist(data_for_leaflet_map()[[2]][(1:2), 1])[[1]][1],
    lat = unlist(data_for_leaflet_map()[[2]][(1:2), 2])[[1]][1] ,
    zoom = 12) %>%
      leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
      leaflet::addAwesomeMarkers(
          data = data_for_leaflet_map()[[2]][, (1:2)], icon = icons,
          label = stringr::str_c("Shipname: ",
          data_for_leaflet_map()[[2]]$SHIPNAME[1], "---",
          "Departing from: ", data_for_leaflet_map()[[2]]$DESTINATION[1],
          "---", "Arriving at: ", data_for_leaflet_map()[[2]]$DESTINATION[2],
          "---", "Distance Travelled (in meters): ",
          data_for_leaflet_map()[[1]])
        ) %>% leaflet::addPolylines(lng =
                        unlist(data_for_leaflet_map()[[2]][(1:2), 1]),
                     lat = unlist(data_for_leaflet_map()[[2]][(1:2), 2]))

     return(leafletMapDisp)

    })

  })

}

# Modules definition complete=================================================#
###############################################################################


# ui for the shiny app containing the above "marine data ui module"
# Note that: id for the marine data ui module matches the marine data
# server module.
ui <- shiny.semantic::semanticPage(
  theme = "paper",
  marine_ui("m1")
)

# server script for the shiny app containg the "marine data server module".
# Note that: id for the marine data ui module matches the marine data
# server module.
server <- shinyServer(function(input, output, session){
  marine_server("m1")
})

# create a Shiny app object
shinyApp(ui = ui, server = server)

