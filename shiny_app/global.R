library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(dbplyr)
library(polished)
library(RMariaDB)
library(EHRtemporalVariability)
library(pool)
library(glue)
library(stringr)
library(httr)
library(jsonlite)
library(utils)

config <- config::get(file = "shiny_app/config.yml")

conn <- dbPool(
  RMariaDB::MariaDB(),
  user = config$db_user,
  password = config$db_password,
  dbname = config$db_name,
  host = config$db_host,
  port = config$db_port
)




# Function to log changes
log_change <- function(site_name, code, drainage_basin, prov_terr_state, species_name, lat, long, elevation_m, status, measuring_system, lab_location, notes, date_col, user, date_changed, action, edit_note) {
  query <- glue("INSERT INTO {config$audit} (site_name, code, drainage_basin, prov_terr_state, species_name, lat, `long`, elevation_m, status, measuring_system, lab_location, notes, date_col, user, date_changed, action, edit_note) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")

  dbExecute(conn, query,
    params = list(site_name, code, drainage_basin, prov_terr_state, species_name, lat, long, elevation_m, status, measuring_system, lab_location, notes, date_col, user, date_changed, action, edit_note)
  )
}



# Stop database connection when application stops
shiny::onStop(function() {
  pool::poolClose(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)
