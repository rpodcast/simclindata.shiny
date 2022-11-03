#' demographics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_demographics_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("number_pats_value"), width = 3),
      valueBoxOutput(ns("number_diag_covid"), width = 3),
      valueBoxOutput(ns("avg_health_expenses"), width = 3),
      valueBoxOutput(ns("number_prescriptions"), width = 3)
    )
  )
}

#' demographics Server Functions
#'
#' @noRd
mod_demographics_server <- function(id, con, patients_rc, providers_rc, encounters_rc, default_coordinates = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    pat_ids_rc <- reactive({
      patients_rc() %>%
        distinct(id) %>%
        pull()
    })

    map_df <- reactive({
      req(providers_rc())
      providers_rc() %>%
        mutate(group = "provider", color = "green") %>%
        select(group, id, lat, lon, utilization, color) %>%
        collect()
    })

    output$avg_health_expenses <- renderValueBox({
      req(patients_rc())
      avg_health_expenses <- patients_rc() %>%
        summarize(mean_expenses = mean(healthcare_expenses, na.rm = TRUE)) %>%
        pull(mean_expenses)

      valueBox(
        gt::vec_fmt_currency(avg_health_expenses),
        subtitle = "Average Healthcare Expenses",
        icon = icon("dollar"),
        color = "warning",
        width = NULL
      )
    })

    output$number_pats_value <- renderValueBox({
      req(patients_rc())
      n_pats <- patients_rc() %>%
        count() %>%
        pull(n)

      valueBox(
        gt::vec_fmt_integer(n_pats),
        subtitle = "Patients",
        icon = icon("person"),
        color = "info",
        width = NULL
      )
    })

    output$number_prescriptions <- renderValueBox({
      n_pres <- tbl(con, "sim_medications") %>%
        distinct(description) %>%
        count() %>%
        pull(n)

      valueBox(
        gt::vec_fmt_integer(n_pres),
        subtitle = "Unique Prescriptions",
        icon = icon("prescription"),
        color = "primary",
        width = NULL
      )
    })

    output$number_diag_covid <- renderValueBox({
      req(pat_ids_rc())
      pat_ids <- pat_ids_rc()

      n_diag <- tbl(con, "sim_conditions") %>%
        filter(code == 840539006) %>%
        filter(patient %in% pat_ids) %>%
        distinct(patient) %>%
        pull() %>%
        length()

      valueBox(
        gt::vec_fmt_integer(n_diag),
        subtitle = "Covid-19 Diagnoses",
        icon = icon("prescription"),
        color = "primary",
        width = NULL
      )
    })

    output$number_gloves <- renderValueBox({      
      n_gloves <- tbl(con, "sim_supplies") %>%
        filter(code == 713779008) %>%
        count(description) %>%
        pull(n)

      valueBox(
        n_gloves,
        subtitle = "Gloves",
        icon = icon("hand"),
        color = "primary",
        width = NULL
      )
    })
  })
}

## To be copied in the UI
# mod_demographics_ui("demographics_1")

## To be copied in the server
# mod_demographics_server("demographics_1")
