library(bslib)
library(DBI)
library(RSQLite)
library(pool)
library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(plotly)



# 1. One-off settings ####

## Initialise database ####
con <- dbConnect(SQLite(), "database/database.sqlite")

# building asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_building
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Type\" TEXT,
          \"Asset Name\" TEXT,
          \"Office Floor Area\" REAL,
          \"Area Unit\" TEXT,
          \"Subleased?\" INTEGER,
          \"Applicable Emission Sources\" TEXT,
          \"Creation Time\" INTEGER)")

# vehicle asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_vehicle
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Type\" TEXT,
          \"Asset Name\" TEXT,
          \"Vehicle Type\" TEXT,
          \"Creation Time\" INTEGER)")

# consumption record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_building
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Name\" TEXT,
          \"Reporting Year\" INTEGER,
          \"Fuel Type\" TEXT,
          Consumption REAL,
          Unit TEXT,
          \"Renewable?\" TEXT,
          \"Renewable Energy Consumption (kWh)\" REAL,
          \"Renewable Energy Type\" TEXT,
          \"Start Date\" INTEGER,
          \"End Date\" INTEGER,
          \"Additional Comment\" TEXT,
          \"Creation Time\" INTEGER)")

# consumption record: vehicle
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_vehicle
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Name\" TEXT,
          \"Reporting Year\" INTEGER,
          \"Fuel Type\" TEXT,
          \"Data Type\" TEXT,
          Amount REAL,
          Unit TEXT,
          \"Start Date\" INTEGER,
          \"End Date\" INTEGER,
          \"Additional Comment\" TEXT,
          \"Creation Time\" INTEGER)")

# emission factor: grid mix
dbExecute(
    con, 
    "CREATE TABLE IF NOT EXISTS ele_grid_mix_table  
    (
    Country TEXT,
    City TEXT,
    Coal REAL,
    Oil REAL,
    Gas REAL,
    Nuclear REAL,
    Renewables REAL,
    Average REAL,
    Remark TEXT)")

# emission record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_building
    (
    Id INTEGER PRIMARY KEY,
    \"Consumption Record Id\",
    \"Asset Name\" TEXT,
    \"Fuel Type\" TEXT,
    \"LB Emission\" REAL,
    \"MB Emission\" REAL,
    \"Start Date\" INTEGER,
    \"End Date\" INTEGER,
    \"Creation Time\" INTEGER)")

# emission record: vehicle
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_vehicle
    (
    Id INTEGER PRIMARY KEY,
    \"Asset Name\" TEXT,
    \"Fuel Type\" TEXT,
    \"Data Type\" TEXT,
    \"LB Emission\" REAL,
    \"MB Emission\" REAL,
    \"Start Date\" INTEGER,
    \"End Date\" INTEGER,
    \"Creation Time\" INTEGER)")

dbDisconnect(con)

## Globals static ####
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

# (testing) a universal emission factor of each energy source
universal_grid_mix_ef <- tibble(
    Coal = 350,
    Oil = 268,
    Gas = 183,
    Nuclear = 0,
    Renewables = 0
)








# UI ####
ui <- page_navbar(
    title = "Carbon Calculator - bslib adoption",
    bg = "lightblue",
    
    # Home page
    nav_panel(title = "Home", 
              layout_column_wrap(
                  width = 1,
                  
                  actionButton(
                      "clear_asset",
                      "Clear Asset Table"
                  ),
                  
                  actionButton(
                      "clear_consumption_record",
                      "Clear Consumption Record Table"
                  ),
                  
                  actionButton(
                      "clear_grid_mix_emission_factor",
                      "Clear Grid Mix Table"
                  ),
                  
                  actionButton(
                      "clear_all",
                      "CLEAR ALL"
                  )
              )
              
    ),
    
    # Asset page
    nav_panel(title = "Asset", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Inputs",
                      navset_card_tab(
                          height = 450,
                          full_screen = TRUE,
                          title = "Placeholder",
                          
                          # Building assets inputs
                          nav_panel(
                              selectInput(
                                  "country_asset_building",
                                  "Country*",
                                  choices = c("Select a country" = "",
                                              country_list)
                              ),
                              selectInput(
                                  "city_asset_building",
                                  "City",
                                  choices = c("Select a city" = "")
                              ),
                              textInput(
                                  "building_asset_name_asset", 
                                  "Asset Name*"
                              ),
                              numericInput(
                                  "office_area_asset",
                                  "Occupied Floor Area",
                                  value = NA,
                                  min = 0
                              ),
                              selectInput(
                                  "area_unit_asset", "Area Unit",
                                  choices = c(
                                      "Select a unit" = "",
                                      "m2",
                                      "ftsq"),
                                  selected = ""),
                              checkboxInput(
                                  "subleased_asset", 
                                  "Subleased Asset?", value = FALSE),
                              selectInput(
                                  "applicable_source_asset",
                                  "Applicable Emission Sources*",
                                  choices = c("Select a unit*" = "", 
                                              fuel_building),
                                  multiple = TRUE),
                              actionButton(
                                  "add_record_building_asset",
                                  "Add Record")  
                          ),
                          
                          # Vehicle assets inputs
                          nav_panel(
                              selectInput(
                                  "country_asset_vehicle",
                                  "Country*",
                                  choices = c("Select a country" = "",
                                              country_list)
                              ),
                              selectInput(
                                  "city_asset_vehicle",
                                  "City",
                                  choices = c("Select a city" = "")
                              ),
                              textInput(
                                  "vehicle_asset_name_asset", "Asset Name*"),
                              selectInput(
                                  "vehicle_type_asset", "Vehicle Type*",
                                  choices = c("Select a vehicle type" = "", 
                                              vehicle_type)),
                              actionButton(
                                  "add_record_vehicle_asset", 
                                  "Add Record")
                          )
                      )
                  )
              )
    ),
    
    # Consumption record page
    nav_panel(title = "Consumption record", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Inputs"
                  ),
                  h3("content")
              )
    ),
    
    # Emission factor page
    nav_panel(title = "Emission factor", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Inputs"
                  ),
                  h3("content")
              )
    ),
    
    # Emission record page
    nav_panel(title = "Emission record", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Inputs"
                  ),
                  h3("content")
              )
    ),
    
    # Inventory page
    nav_panel(title = "Inventory", 
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Inputs"
                  ),
                  h3("content")
              )
    ),
)

server <- function(input, output) {}


shinyApp(ui, server)
