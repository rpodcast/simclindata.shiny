#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        htmltools::includeMarkdown(app_sys("docs", "welcome.md")),
        collapsible = FALSE,
        headerBorder = FALSE,
        width = 12
      )
    )
  )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_1")
    
## To be copied in the server
# mod_welcome_server("welcome_1")
