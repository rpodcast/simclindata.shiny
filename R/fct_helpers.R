db_con <- function(prod = golem::app_prod()) {
  stopifnot(require("RSQLite", quietly = TRUE))
  if (prod) {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      "data-raw/synthea_covid19.sqlite"
    )
  } else {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      "data-raw/synthea_covid19_small.sqlite"
    )
  }
  return(con)
}

age_filter_gen <- function(con, age_input, patients_rc = NULL) {
  if (is.null(patients_rc)) patients_rc = tbl(con, "sim_patients")

  if (!is.function(patients_rc)) {
    patients_rc <- local({
      value <- patients_rc
      function() {
        value
      }
    })
  }

  if (length(age_input) == 3) {
    df <- patients_rc()
  } else {
    if (length(age_input) == 2) {
      if (identical(sort(age_input), c("18", "18-65"))) {
        df <- patients_rc() %>%
          filter(age < 65)
      } else if (identical(sort(age_input), c("18", "65"))) {
        df <- patients_rc() %>%
          filter(age < 18 || age > 65)
      } else {
        df <- patients_rc() %>%
          filter(age >= 18)
      }
    } else {
      if (age_input == "18") {
        df <- patients_rc() %>%
          filter(age < 18)
      } else if (age_input == "18-65") {
        df <- patients_rc() %>%
          filter(between(age, 18, 65))
      } else {
        df <- patients_rc() %>%
          filter(age > 65)
      }
    }
  }

  return(df)
}

derive_c19_labs <- function(
  con, 
  lab_codes = c('48065-7', '26881-3', '2276-4', '89579-7', '2532-0', '731-0', '14804-9'),
  lab_labels = c('D-dimer', 'Serum Ferritin', 'High Sensitivity Cardiac Troponin I', 'IL-6', 'Lymphocytes', 'Lactate dehydrogenase'),
  patients_rc = NULL, 
  observations_rc = NULL, 
  conditions_rc = NULL
) {

  
  
  if (is.null(patients_rc)) patients_rc = tbl(con, "sim_patients")
  if (is.null(observations_rc)) observations_rc = tbl(con, "sim_observations_c19")
  if (is.null(conditions_rc)) conditions_rc = tbl(con, "sim_conditions")

  if (!is.function(patients_rc)) {
    patients_rc <- local({
      value <- patients_rc
      function() {
        value
      }
    })
  }

  if (!is.function(observations_rc)) {
    observations_rc <- local({
      value <- observations_rc
      function() {
        value
      }
    })
  }

  if (!is.function(conditions_rc)) {
    conditions_rc <- local({
      value <- conditions_rc
      function() {
        value
      }
    })
  }

  # Grabs IDs for patients that have completed the care plan for isolation at home.
  # completed_isolation_patients = care_plans[(care_plans.CODE == 736376001) & (care_plans.STOP.notna()) & (care_plans.REASONCODE == 840539006)].PATIENT
  completed_isolation_patients <- tbl(con, "sim_careplans") %>%
    filter(code == 736376001) %>%
    filter(!is.na(stop)) %>%
    filter(reasoncode == 840539006) %>%
    distinct(patient) %>%
    pull(patient)

  # obtain patients with a negative COVID-19 test
  # negative_covid_patient_ids = observations[(observations.CODE == '94531-1') & (observations.VALUE == 'Not detected (qualifier value)')].PATIENT.unique()
  negative_covid_patient_ids <- observations_rc() %>%
    filter(code == '94531-1' & value == 'Not detected (qualifier value)') %>%
    #filter(value == 'Not detected (qualifier value)') %>%
    distinct(patient) %>%
    pull(patient)

  survivor_ids <- union(completed_isolation_patients, negative_covid_patient_ids)

  # obtain COVID-19 conditions and merge in patient survivor status
  covid_patients <- conditions_rc() %>%
    filter(code == 840539006) %>%
    left_join(patients_rc(), by = c("patient" = "id")) %>%
    mutate(survivor = ifelse(patient %in% survivor_ids, "yes", "no")) %>%
    select(start, start_char, patient, survivor)

  # obtain relevant patient observations with desired lab parameters
  lab_obs <- observations_rc() %>%
    filter(code %in% lab_codes)

  # derive plot data set
  covid_patients_obs <- covid_patients %>%
    left_join(lab_obs, by = "patient") %>%
    filter(!is.na(code)) %>%
    mutate(lab_days = date - start) %>%
    mutate(lab_days2 = as.integer(lab_days)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(loinc_to_display = case_when( 
      code == '48065-7' ~ 'D-dimer',
      code == '2276-4'  ~ 'Serum Ferritin',
      code == '89579-7' ~ 'High Sensitivity Cardiac Troponin I',
      code == '26881-3' ~ 'IL-6',
      code == '731-0'   ~ 'Lymphocytes',
      code == '14804-9' ~ 'Lactate dehydrogenase',
      TRUE      ~ 'Unknown'
    )) %>%
    collect()
  return(covid_patients_obs)
}

derive_encounter_classes <- function(con) {
  tbl(con, "sim_encounters_c19") %>%
    distinct(encounterclass) %>%
    arrange() %>%
    pull()
}

derive_encounter_dates <- function(con) {
  df <- tbl(con, "sim_encounters_c19") %>%
    summarize(
      min_start_date = min(start_date_char, na.rm = TRUE),
      max_stop_date = max(stop_date_char, na.rm = TRUE)
    ) %>%
    collect()

    list(
      start_date = df$min_start_date,
      end_date = df$max_stop_date
    )
}

derive_age_range <- function(con) {

  age_df <- tbl(con, "sim_patients") %>%
    summarize(
      min_age = min(age, na.rm = TRUE),
      max_age = max(age, na.rm = TRUE)
    ) %>%
    collect()

  list(
    min_age = age_df$min_age,
    max_age = age_df$max_age
  )
}

#' derive age from dates
#'
#' @description Calculate age using starting date and optional end date
#' @param start_date Date object for starting date of interval (typically birthdate)
#' @param end_date Optional Date object for ending date of interval
#'   such as death date or simulation end date. If NA, will use current
#'   date of the session.
#' @return age in years
#' @import lubridate
#' @export
compute_age <- function(start_date, end_date = NA, placeholder_date = "2020-12-31") {
  if (is.na(end_date)) end_date <- placeholder_date

  if (is.character(start_date)) start_date <- as.Date(start_date)
  if (is.character(end_date)) end_date <- as.Date(end_date)

  (start_date %--% end_date) / years(1)
}

#' obtain min and max mapping coordinates
compute_default_coordinates <- function(con) {
  # create list of data frames for use in purrr
  df_list <- c("sim_patients", "sim_providers", "sim_organizations") 
  df_coord <- purrr::map_dfr(df_list, ~{
    tbl(con, .x) %>%
      select(id, lat, lon) %>%
      summarize(
        lng1 = min(lon, na.rm = TRUE), 
        lat1 = min(lat, na.rm = TRUE), 
        lng2 = max(lon, na.rm = TRUE), 
        lat2 = max(lat, na.rm = TRUE)
      ) %>%
      collect()
  })

  df_coord2 <- summarize(df_coord, lng1 = min(lng1), lat1 = min(lat1), lng2 = max(lng2), lat2 = max(lat2))
  
  list(
    lng1 = df_coord2$lng1,
    lat1 = df_coord2$lat1,
    lng2 = df_coord2$lng2,
    lat2 = df_coord2$lat2
  )
}

# derive patients diagnosed with Covid 19
diag_c19_patients <- function(con) {
  tbl(con, "sim_conditions") %>%
    filter(code == 840539006) %>%
    distinct(patient) %>%
    pull()
} 

# derive patients with a negative Covid-19 test
# include patients who tested negative up front 
# as well as patients that tested negative after leaving the hospital
neg_c19_test_patients <- function(con) {
  tbl(con, "sim_observations_c19") %>%
    filter(code == '94531-1') %>%
    filter(value == 'Not detected (qualifier value)') %>%
    distinct(patient) %>%
    pull()
}

# derive patients completing isolation at home care plan
iso_care_plan_patients <- function(con) {
  tbl(con, "sim_careplans") %>%
    filter(code == 736376001) %>%
    filter(!is.na(stop)) %>%
    filter(reasoncode == 840539006) %>%
    distinct(patient) %>%
    pull()
}

# derive patients admitted to hospital due to covid 19
inpatient_c19_ids <- function(con) {
  tbl(con, "sim_encounters_c19") %>%
    filter(reasoncode == 840539006) %>%
    filter(code == 1505002) %>%
    distinct(patient) %>%
    pull()
}

derive_outcomes_data <- function(
  con,
  pat_ids = NULL,
  outcome_codes = c("770349000", "65710008", "67782005", "84114007", "76571007", "234466008", "86175003", "40095003"),
  outcome_labels = c("Sepsis", "Respiratory Failure", "ARDS", "Heart Failure", "Septic Shock", "Coagulapathy", "Acute Cardiac Injury", "Acute Kidney Injury")
) {

  if (is.null(pat_ids)) pat_ids <- inpatient_c19_ids(con)

  tbl(con, "sim_conditions") %>%
    filter(patient %in% pat_ids) %>%
    filter(code %in% outcome_codes) %>%
    distinct(patient, code, description) %>%
    mutate(label = case_when(
      code == "770349000" ~ "Sepsis",
      code == "65710008" ~ "Respiratory Failure",
      code == "67782005" ~ "ARDS",
      code == "84114007" ~ "Heart Failure",
      code == "76571007" ~ "Septic Shock",
      code == "234466008" ~ "Coagulapathy",
      code == "86175003" ~ "Acute Cardiac Injury",
      code == "40095003" ~ "Acute Kidney Injury"
    ))
}