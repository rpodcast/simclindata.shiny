#' ageselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ageselect_ui <- function(id, label = "Age"){
  ns <- NS(id)
  shinyWidgets::awesomeCheckboxGroup(
    ns("age_input"),
    label = label,
    choices = c(
      "Pediatric (<18)" = "18",
      "Adult (18-65)" = "18-65",
      "Senior (>65)" = "65"
    ),
    selected = c("18", "18-65", "65"),
    status = "success"
  )
  # sliderInput(
  #   ns("age_input"),
  #   label = label,
  #   min = 18,
  #   max = 110,
  #   value = c(18, 110),
  #   step = 1
  # )
}

#' ageselect Server Functions
#'
#' @noRd
mod_ageselect_server <- function(id, limits = c(18, 110)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    min_level <- round(limits[1], 0)
    max_level <- round(limits[2], 0)
    default_value <- c(min_level, max_level)

    value <- reactive({
      req(input$age_input)
      input$age_input
    })

    value
  })
}

## To be copied in the UI
# mod_ageselect_ui("ageselect_1")

## To be copied in the server
# mod_ageselect_server("ageselect_1")
