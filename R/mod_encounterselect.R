#' encounterselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets awesomeCheckboxGroup updateAwesomeCheckboxGroup
mod_encounterselect_ui <- function(id){
  ns <- NS(id)
    shinyWidgets::awesomeCheckboxGroup(
      ns("enc_select"),
      "Encounter Class",
      choices = NULL,
      status = "success"
    )
}
    
#' encounterselect Server Functions
#'
#' @noRd 
mod_encounterselect_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    enc_classes <- derive_encounter_classes(con)

    shinyWidgets::updateAwesomeCheckboxGroup(
      session,
      "enc_select",
      choices = enc_classes,
      selected = enc_classes
    )

    enc_selected <- reactive({
      req(input$enc_select)
      input$enc_select
    })

    enc_selected
  })
}
    
## To be copied in the UI
# mod_encounterselect_ui("encounterselect_1")
    
## To be copied in the server
# mod_encounterselect_server("encounterselect_1")
