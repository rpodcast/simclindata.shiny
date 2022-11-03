#' dateselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList dateRangeInput updateDateRangeInput
mod_dateselect_ui <- function(id, label = "Date Range", start = "2020-03-10", end = "2021-12-30"){
  ns <- NS(id)
  dateRangeInput(
    ns("date"),
    label = label,
    start = start,
    end = end
  )
}

#' dateselect Server Functions
#'
#' @noRd
mod_dateselect_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #encounters_dates <- derive_encounter_dates(con)

    # updateDateRangeInput(
    #   session = session,
    #   inputId = "date",
    #   min = encounters_dates$start_date,
    #   max = encounters_dates$end_date
    #   #start = encounters_dates$start_date,
    #   #end = encounters_dates$end_date
    # )

    value <- reactive({
      req(input$date)
      input$date
    })

    value
  })
}

## To be copied in the UI
# mod_dateselect_ui("dateselect_1")

## To be copied in the server
# mod_dateselect_server("dateselect_1")
