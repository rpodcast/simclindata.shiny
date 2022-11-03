## code to prepare synthetic data for workshop app
## goal: small subset within the package to support documentation and quick explorations without a database connection
devtools::load_all()

library(dplyr)
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)
library(arrow)

data_dir <- Sys.getenv("synthea_covid19_dir")

# import raw csv files
# http://hdx.mitre.org/downloads/syntheticmass/100k_synthea_covid19_csv.zip

table_names <- c(
  "conditions",
  "patients",
  "observations",
  "careplans",
  "encounters",
  "devices",
  "supplies",
  "procedures",
  "medications",
  "organizations",
  "immunizations",
  "payers",
  "allergies",
  "providers"
)

all_tables <- purrr::map(table_names, ~read_csv(file.path(data_dir, paste0(.x, ".csv")), name_repair = tolower))

names(all_tables) <- table_names

# derive age and date-related fields for key data sets since SQLite and dates is such a headache
all_tables$patients <- all_tables$patients %>%
  mutate(birthdate_char = as.character(birthdate), deathdate_char = as.character(deathdate)) %>%
  mutate(age = purrr::map2_dbl(birthdate, deathdate, ~compute_age(.x, .y)))

all_tables$observations <- all_tables$observations %>%
  mutate(date_char = as.character(date))

all_tables$supplies <- all_tables$supplies %>%
  mutate(date_char = as.character(date))

all_tables$procedures <- all_tables$procedures %>%
  mutate(date_char = as.character(date))

all_tables$immunizations <- all_tables$immunizations %>%
  mutate(date_char = as.character(date))

all_tables$careplans <- all_tables$careplans %>%
  mutate(start_char = as.character(start), stop_char = as.character(stop))

all_tables$conditions <- all_tables$conditions %>%
  mutate(start_char = as.character(start), stop_char = as.character(stop))

all_tables$devices <- all_tables$devices %>%
  mutate(start_char = as.character(start), stop_char = as.character(stop))

all_tables$medications <- all_tables$medications %>%
  mutate(start_char = as.character(start), stop_char = as.character(stop))

all_tables$allergies <- all_tables$allergies %>%
  mutate(start_char = as.character(start), stop_char = as.character(stop))

all_tables$encounters <- all_tables$encounters %>%
  mutate(
    start_date = lubridate::ymd_hms(start) %>% as.Date(),
    stop_date = lubridate::ymd_hms(stop) %>% as.Date(),
    start_char = as.character(start),
    stop_char = as.character(stop),
    start_date_char = as.character(start_date),
    stop_date_char = as.character(stop_date)
  )

# filter encounters relevant to COVID-19 
# start: 2020-03-01
# end: 2020-06-02 (this is max in the data)

all_tables$encounters_c19 <- all_tables$encounters %>%
  filter(start >= "2020-03-01")

# filter observations to have parameters relevant to COVID-19 
# start: 2020-03-01
# code: lab_codes = c('48065-7', '26881-3', '2276-4', '89579-7', '2532-0', '731-0', '14804-9')
lab_codes = c('48065-7', '26881-3', '2276-4', '89579-7', '2532-0', '731-0', '14804-9')
all_tables$observations_c19 <- all_tables$observations %>%
  filter(date >= "2020-03-01") %>%
  filter(code %in% lab_codes)

# create SQLite database with contents for workshop usage
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  "data-raw/synthea_covid19.sqlite"
)

# write tables to SQLite (except for "full" observations and encounters)

table_names2 <- c(
  "conditions",
  "patients",
  "observations_c19",
  "careplans",
  "encounters_c19",
  "devices",
  "supplies",
  "procedures",
  "medications",
  "organizations",
  "immunizations",
  "payers",
  "allergies",
  "providers"
)

purrr::map(table_names2, ~{
  message(glue::glue("writing {.x} to SQLite database"))
  DBI::dbWriteTable(con, paste0("sim_", .x), all_tables[[.x]], overwrite = TRUE)
})


# derive covid labs data table and save to SQLite
c19_labs <- derive_c19_labs(con)

DBI::dbWriteTable(con, "sim_c19_labs", c19_labs, overwrite = TRUE)
DBI::dbListTables(con)

# derive small versions of the data files based on a subset of patients so it can be included easily into the app during the workshop

con2 <- DBI::dbConnect(
  RSQLite::SQLite(),
  "data-raw/synthea_covid19_small.sqlite"
)

set.seed(761354)
n_patients <- 200

# ensure patients select correspond to covid cases
patient_ids <- tbl(con, "sim_c19_labs") %>%
  slice_sample(n = n_patients) %>%
  distinct(patient) %>%
  pull(patient)

# patient_ids <- all_tables[["patients"]] %>%
#   slice_sample(n = n_patients) %>%
#   pull(id)

# create filtered c19_labs table
sim_c19_labs <- tbl(con, "sim_c19_labs") %>%
  filter(patient %in% patient_ids) %>%
  arrange(patient, description, date) %>%
  collect()

DBI::dbWriteTable(con2, "sim_c19_labs", sim_c19_labs, overwrite = TRUE)
usethis::use_data(sim_c19_labs, overwrite = TRUE)

# create filtered patients table
sim_patients <- tbl(con, "sim_patients") %>%
  filter(id %in% patient_ids) %>%
  collect()

DBI::dbWriteTable(con2, "sim_patients", sim_patients, overwrite = TRUE)
usethis::use_data(sim_patients, overwrite = TRUE)

# perform repeated filters of data sets with patient
# as another key variable labelled "patient"
pat_tables <- c(
  "observations_c19",
  "allergies", 
  "conditions", 
  "careplans", 
  "devices", 
  "immunizations", 
  "medications", 
  "procedures",
  "encounters_c19",
  "supplies"
)

all_pat_tables_small <- purrr::map(pat_tables, ~{
  message(glue::glue("filtering table {.x}"))
  df_name <- paste0("sim_", .x)
  df <- tbl(con, df_name) %>% 
    filter(patient %in% patient_ids) %>%
    collect()
  
  DBI::dbWriteTable(con2, df_name, df, overwrite = TRUE)
  return(df)
})

names(all_pat_tables_small) <- pat_tables

# can't do this in map or a loop, I give up
sim_observations_c19 <- all_pat_tables_small[["observations_c19"]]
usethis::use_data(sim_observations_c19, overwrite = TRUE)

sim_allergies <- all_pat_tables_small[["allergies"]]
usethis::use_data(sim_allergies, overwrite = TRUE)

sim_conditions <- all_pat_tables_small[["conditions"]]
usethis::use_data(sim_conditions, overwrite = TRUE)

sim_careplans <- all_pat_tables_small[["careplans"]]
usethis::use_data(sim_careplans, overwrite = TRUE)

sim_devices <- all_pat_tables_small[["devices"]]
usethis::use_data(sim_devices, overwrite = TRUE)

sim_immunizations <- all_pat_tables_small[["immunizations"]]
usethis::use_data(sim_immunizations, overwrite = TRUE)

sim_medications <- all_pat_tables_small[["medications"]]
usethis::use_data(sim_medications, overwrite = TRUE)

sim_procedures <- all_pat_tables_small[["procedures"]]
usethis::use_data(sim_procedures, overwrite = TRUE)

sim_encounters <- all_pat_tables_small[["encounters"]]
usethis::use_data(sim_encounters, overwrite = TRUE)

sim_supplies <- all_pat_tables_small[["supplies"]]
usethis::use_data(sim_supplies, overwrite = TRUE)

# perform additional filters based on payer and provider ids
encounter_ids <- all_pat_tables_small[["encounters_c19"]] %>%
  pull(id)
payer_ids <- unique(all_pat_tables_small[["encounters_c19"]]$payer)
provider_ids <- unique(all_pat_tables_small[["encounters_c19"]]$provider)

# create filtered payers table
sim_payers <- tbl(con, "sim_payers") %>%
  filter(id %in% payer_ids) %>%
  collect()

DBI::dbWriteTable(con2, "sim_payers", sim_payers, overwrite = TRUE)
usethis::use_data(sim_payers, overwrite = TRUE)

# create filtered providers table
sim_providers <- tbl(con, "sim_providers") %>%
  filter(id %in% provider_ids) %>%
  collect()

DBI::dbWriteTable(con2, "sim_providers", sim_providers, overwrite = TRUE)
usethis::use_data(sim_providers, overwrite = TRUE)

# organizations is small enough already
sim_organizations <- tbl(con, "sim_organizations") %>% collect()
DBI::dbWriteTable(con2, "sim_organizations", sim_organizations, overwrite = TRUE)
usethis::use_data(sim_organizations, overwrite = TRUE)

# close connections
DBI::dbListTables(con2)

DBI::dbDisconnect(con)
DBI::dbDisconnect(con2)
