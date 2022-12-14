#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import lubridate
#' @noRd
app_server <- function(input, output, session) {
  # create database connection
  con <- db_con()

  # establish default values of key inputs
  age_range <- derive_age_range(con)

  # reactive for patients subset
  # key input: ageselect
  patients_rc <- reactive({
    req(age_patients())
    age_select <- age_patients()

    # perform age filtering using custom function
    age_filter_gen(con, age_select)
  })

  pat_ids_rc <- reactive({
    patients_rc() %>%
      distinct(id) %>%
      pull()
  })

  # reactive for encounters subset
  # key input: dateselect
  encounters_rc <- reactive({
    req(date_encounters())
    req(class_encounters())

    pat_ids <- pat_ids_rc()

    min_date <- date_encounters()[1]
    max_date <- date_encounters()[2]

    class_sub <- class_encounters()

    tbl(con, "sim_encounters_c19") %>%
      filter(patient %in% pat_ids) %>%
      filter(start_char >= min_date) %>%
      filter(stop_char <= max_date) %>%
      filter(encounterclass %in% class_sub)
  })

  # reactive for providers subset
  # dynamically re-compute number of encounters based on encounters subset
  providers_rc <- reactive({
    tbl(con, "sim_providers")
    req(encounters_rc())

    enc_sub <- encounters_rc() %>%
      count(provider, name = "utilization") %>%
      rename(id = provider)

    tbl(con, "sim_providers") %>%
      select(., -utilization) %>%
      right_join(enc_sub, by = "id")
  })

  

  # reactive for medications subset
  # filter based on patient
  medications_rc <- reactive({
    pat_ids <- pat_ids_rc()

    tbl(con, "sim_medications") %>%
      filter(patient %in% pat_ids)

    return(med_sub)
  })

  # reactive for observations subset
  # filter based on patients
  observations_rc <- reactive({
    pat_ids <- pat_ids_rc()

    tbl(con, "sim_observations_c19") %>%
      filter(patient %in% pat_ids)
  })

  # reactive for conditions subset
  # filter based on patients
  conditions_rc <- reactive({
    pat_ids <- pat_ids_rc()

    tbl(con, "sim_conditions") %>%
      filter(patient %in% pat_ids)
  })

  # run modules
  mod_demographics_server("demographics_1", con, patients_rc, providers_rc, encounters_rc, default_coordinates)
  date_encounters <- mod_dateselect_server("dateselect_1", con)
  age_patients <- mod_ageselect_server("ageselect_1")
  class_encounters <- mod_encounterselect_server("encounterselect_1", con)

  # clean connection on exit
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })

}
