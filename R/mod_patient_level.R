#' patient_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr
#' @import plotly
#' @import ggplot2
mod_patient_level_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(
        ns("lab_panel"),
        "Select Lab Panel",
        choices = c(
          "D-dimer" = "48065-7",
          "Serum Ferritin" = "2276-4",
          "High-Sensitivity Cardiac Troponin I" = "89579-7",
          "IL-6" = "26881-3",
          "Lymphocytes" = "731-0",
          "Lactate dehydrogenase" = "14804-9"
        )
      )
    ),
    fluidRow(
      plotly::plotlyOutput(ns("lab_boxplots"))
    )
  )
}
    
#' patient_level Server Functions
#'
#' @noRd 
mod_patient_level_server <- function(id,  con, patients_rc){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    lab_source_data <- reactive({
      req(input$lab_panel)
      lab_selected <- input$lab_panel
      
      tbl(con, "sim_c19_labs") %>%
        filter(code == lab_selected) %>%
        collect()
    }) %>% bindCache(input$lab_panel)

    output$lab_boxplots <- plotly::renderPlotly({
      req(lab_source_data())
      p <- plot_ly(
        lab_source_data(),
        y = ~value,
        alpha = 0.1,
        boxpoints = "suspectedoutliers",
        source = "labs_box"
      ) %>%
        plotly::config(
          displaylogo = FALSE
        )

      p1 <- p %>%
        add_boxplot(x = ~factor(lab_days2)) %>%
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")

      p1
    }) %>%
    bindCache(input$lab_panel)

    output$plotly_debugging <- renderPrint({
      d <- event_data("plotly_brushed", source = "labs_box")
      if (is.null(d)) "Click bar to show event" else d
    })
  })


}
    
## To be copied in the UI
# mod_patient_level_ui("patient_level_1")
    
## To be copied in the server
# mod_patient_level_server("patient_level_1")
