#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bs4Dash box userBox descriptionBlock
#' @importFrom leaflet leaflet leafletOutput renderLeaflet leafletOptions leafletProxy clearControls clearMarkers addMarkers addLegend fitBounds markerClusterOptions addTiles
mod_map_ui <- function(id, title = "Geographic Distribution"){
  ns <- NS(id)
  tagList(
    box(
      fluidRow(
        col_8(
          leafletOutput(ns("map"), height = 600),
        ),
        col_4(
          uiOutput(ns("provider_profile"))
        )
      ),
      id = ns("leaflet_box"),
      title = title,
      collapsible = TRUE,
      maximizable = TRUE,
      width = 12
    )
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id, map_df, default_coordinates = NULL, providers_rc){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # this version contains both the dynamic updates to data and the leaflet object itself
    # which is not optimal
    # output$demo_map <- renderLeaflet({
      
    #   req(patients_rc())
    #   mapping_df <- dplyr::bind_rows(
    #     patients_rc() %>%
    #       mutate(group = "patient", color = "blue") %>%
    #       select(group, id, lat, lon, color),
    #     providers_df %>%
    #       mutate(group = "provider", color = "green") %>%
    #       select(group, id, lat, lon, color)
    #   )
    #   m <- leaflet() %>%
    #     addTiles() %>%
    #     addCircleMarkers(data = mapping_df, lat = ~lat, lng = ~lon, color = ~color)
    # })

    # reactive for color palette in map
    map_pal <- reactive({
      req(map_df())
      # arrange by alphabetical order
      map_df() %>%
        distinct(group, color) %>%
        arrange(group)
    })

    output$map <- renderLeaflet({
      m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addTiles() 

      if (!is.null(default_coordinates)) {
        m <- m %>%
          fitBounds(
            default_coordinates$lng1, 
            default_coordinates$lat1, 
            default_coordinates$lng2, 
            default_coordinates$lat2
          )
      }

      return(m)
    })

    observeEvent(map_df(), {
      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        addMarkers(data = map_df(), lat = ~lat, lng = ~lon, layerId = ~id, clusterOptions = markerClusterOptions())
    })

    output$provider_profile <- renderUI({
      if (is.null(input$map_marker_click)) {
        tags$h3("Click on marker to view provider information")
      } else {
        # grab provider unique ID from click input
        provider_id <- input$map_marker_click$id
        
        # create bs4Dash user card with basic values
        provider_meta <- providers_rc() %>%
          filter(id == provider_id) %>%
          collect()

        provider_image <- case_when( 
          provider_meta$gender == "M" ~ "male_provider.png",
          provider_meta$gender == "F" ~ "female_provider.png",
          TRUE ~ "unknown.png"
        )

        userBox(
          descriptionBlock(
            header = gt::vec_fmt_integer(provider_meta$utilization),
            text = "Patient Visits",
            rightBorder = FALSE,
            marginBottom = FALSE
          ),
          title = userDescription(
            title = provider_meta$name,
            subtitle = provider_meta$speciality,
            type = 1,
            image = file.path("www/img", provider_image)
          ),
          width = 12,
          status = "primary",
          boxToolSize = "lg"
        )
      }
    })

    list(
      provider_click_id = reactive(input$map_marker_click$id),
      map_bounds <- reactive(input$map_bounds)
    )


  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
