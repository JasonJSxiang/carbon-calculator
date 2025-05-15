library(DBI)
library(RSQLite)
library(pool)
library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(shinydashboard)
library(plotly)
library(digest)


# Initialise database ------------------------------------------------------------
con <- dbConnect(SQLite(), "database/database.sqlite")

# table1
dbExecute(
  con,
  "CREATE TABLE IF NOT EXISTS table1
          (
          asset_hash TEXT,
          country TEXT,
          city TEXT,
          asset_name TEXT,
          asset_type TEXT,
          floor_area REAL,
          floor_area_unit TEXT,
          is_subleased INTEGER,
          applicable_emission_sources TEXT,
          lease_expiration_date INTEGER,
          asset_remark TEXT,
          asset_ct INTEGER,
          
          consumption_hash TEXT,
          fuel_type TEXT,
          consumption REAL,
          consumption_unit TEXT,
          reporting_year INTEGER,
          start_date INTEGER,
          end_date INTEGER,
          is_renewable INTEGER,
          renewable_energy_amount REAL,
          renewable_energy_type TEXT,
          consumption_record_remark TEXT,
          consumption_record_ct INTEGER,
          
          emission_record_hash
          LB_emission REAL,
          MB_emission REAL,
          FERA_emission REAL,
          emission_record_remark TEXT,
          emission_record_ct
          
          coal REAL,
          oil REAL,
          gas REAL,
          nuclear REAL,
          renewables REAL,
          grid_mix_remark TEXT
          )"
)

# Global -------------------------------------------------------
#(code only run once when the app starts)

# fuel list for building
fuel_building <- c("Electricity",
                   "Heating",
                   "Steam",
                   "Cooling",
                   "Diesel",
                   "Petrol",
                   "Kerosene")

# vehicle type list for vehicle
vehicle_type <- c("Petrol",
                  "Diesel",
                  "Hybrid",
                  "Electric")

# building fuel unit
building_fuel_unit <- c("kWh", "BTU")

# reporting year
reporting_year <- c(2025:2015)

# renewable energy type
renewable_energy_type <- c("Solar", "Wind", "Hydro", "Biomass")

# country and city list (from maps package)
country_list <- world.cities |> 
  rename("country" = "country.etc") |> # rename the country col
  mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
  dplyr::distinct(country, .keep_all = TRUE) |>  # keep unique country
  dplyr::arrange(country) |> 
  pull(country)

city_df <- world.cities |> 
  rename("country" = "country.etc") |> # rename the country col
  mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
  dplyr::distinct(name, .keep_all = TRUE) |>  # keep unique country
  dplyr::arrange(name)

# static ui components --------------------------------------------------------------

## dashboard header ####
db_header <- dashboardHeader(
  
  title = "Carbon Calculator 张译翔",
  titleWidth = 300
  
)

## dashboard sidebar ####
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",
             tabName = "home_tab",
             icon = icon("home"),
             selected = TRUE),
    menuItem("Asset", 
             tabName = "asset_tab", 
             icon = icon("building")),
    menuItem("Consumption Record",
             tabName = "consumption_record_tab",
             icon = icon("table-list")),
    menuItem("Emission Factor",
             tabName = "emission_factor_tab",
             icon = icon("clipboard")),
    menuItem(
      "Emission Record",
      tabName = "emission_record_tab",
      icon = icon("clipboard-user")
    ),
    menuItem("Inventories",
             tabName = "carbon_inventory_tab",
             icon = icon("dashboard"))
  )
)


# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  
  db_header,
  db_sidebar,
  dashboardBody(uiOutput("db_body"))
  
)

server <- function(input, output, session) {
  
  
  # Establish connection with database --------------------------------------
  pool <- dbPool(
    SQLite(),
    dbname = "database/database.sqlite"
  )
  
  # disconnect the pool when ending the session
  onStop(function() {
    poolClose(pool)
  })
  
  
  
}



shinyApp(ui, server)