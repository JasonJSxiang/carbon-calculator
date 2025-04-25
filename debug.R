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

# building asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_building
          (Country TEXT,
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
          (Country TEXT,
          City TEXT,
          \"Asset Type\" TEXT,
          \"Asset Name\" TEXT,
          \"Vehicle Type\" TEXT,
          \"Creation Time\" INTEGER)")

# consumption record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_building
          (\"Hash ID\" TEXT,
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
          (\"Hash ID\" TEXT,
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

# emission record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_building
    (\"Hash ID\" TEXT,
    \"Consumption Record Hash ID\" TEXT,
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
    (\"Hash ID\" TEXT,
    \"Consumption Record Hash ID\",
    \"Asset Name\" TEXT,
    \"Fuel Type\" TEXT,
    \"Data Type\" TEXT,
    \"LB Emission\" REAL,
    \"MB Emission\" REAL,
    \"Start Date\" INTEGER,
    \"End Date\" INTEGER,
    \"Creation Time\" INTEGER)")



# emission factor: grid mix
dbExecute(
    con, 
    "CREATE TABLE IF NOT EXISTS grid_mix_emission_factor  
    (Country TEXT,
    City TEXT,
    \"Coal\" REAL,
    \"Oil\" REAL,
    \"Gas\" REAL,
    \"Nuclear\" REAL,
    \"Renewables\" REAL,
    Remark TEXT)")

dbDisconnect(con)

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
        menuItem("Emission Record",
                 tabName = "emission_record_tab",
                 icon = icon("table-list")),
        menuItem("Emission Factor",
                 tabName = "emission_factor_tab",
                 icon = icon("clipboard")),
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
    
    
    # Dynamic objects ---------------------------------------------------------
    
    # function to load all the tables
    load_all <- function() {
        load_asset_building()
        load_asset_vehicle()
        load_consumption_record_building()
        load_consumption_record_vehicle()
        load_emission_record_building()
        load_emission_record_vehicle()
        load_grid_mix_emission_factor()
    }    
    
    
    
    # dashboard body (dynamic) ----------------------------------------------------
    
    # run the dynamic ui once when the app starts
    output$db_body <- renderUI(
        tabItems(
            ## home tab ####
            tabItem(
                tabName = "home_tab",
                h2("Carbon Calculator 张译翔"),
                
                fluidRow(
                    column(
                        width = 2,
                        
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
                )
                
                
                
            ),
            
            ## asset tab ####
            tabItem(
                tabName = "asset_tab",
                
                fluidRow(
                    ### inputs tab box ####
                    column(
                        width = 3,
                        
                        tabBox(
                            title = NULL,
                            id = "asset_inputs_box",
                            width = NULL,
                            
                            #### building ####
                            tabPanel(
                                id = "building_inputs_asset", 
                                title = "Building",
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
                            
                            #### vehicle ####
                            tabPanel(
                                id = "vehicle_inputs_asset",
                                title = "Vehicle",
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
                    ),
                    
                    ### table tab box ####
                    column(
                        width = 9,
                        
                        tabBox(
                            title = NULL,
                            id = "asset_table_asset",
                            width = NULL,
                            
                            #### building ####
                            tabPanel(
                                
                                id = "building_table_asset",
                                title = "Building",
                                div(
                                    style = "overflow-x: auto; min-height: 100px;",  
                                    DTOutput("asset_table_building")
                                )
                                
                            ),
                            
                            #### vehicle ####
                            tabPanel(
                                
                                id = "vehicle_table_asset",
                                title = "Vehciel",
                                div(
                                    style = "overflow-x: auto; min-height: 100px;",  
                                    DTOutput("asset_table_vehicle")
                                )
                                
                            )
                            
                        )
                    )
                )
            ),
            
            ## emission record tab ####
            tabItem(
                tabName = "emission_record_tab",
                
                fluidRow(
                    
                    ### inputs tab box ####
                    column(
                        width = 3,
                        
                        tabBox(
                            title = NULL,
                            id = "consumption_record_inputs",
                            width = NULL,
                            
                            #### building ####
                            tabPanel(
                                title = "Building",
                                id = "building_inputs_consumption_record",
                                
                                selectInput(
                                    "building_asset_consumption_record",
                                    "Select asset*",
                                    choices = ""),
                                selectInput(
                                    "building_year_consumption_record",
                                    "Select a Reporting Year*",
                                    choices = c("Select a year" = "",
                                                reporting_year)),
                                selectInput(
                                    "fuel_select_building_consumption_record",
                                    "Select fuel type*",
                                    choices = c("Select a fuel type" = "")),
                                numericInput(
                                    "building_consumption_consumption_record",
                                    "Energy consumption*",
                                    value = NA,
                                    min = 0),
                                selectInput(
                                    "building_unit_consumption_record",
                                    "Select a unit",
                                    choices = c("Select a unit" = "",
                                                building_fuel_unit)),
                                dateRangeInput(
                                    "building_date_range_consumption_record", 
                                    "Date Range* (yyyy-mm-dd)",
                                    start = NA, end = NA),
                                textInput(
                                    "building_comment_consumption_record",
                                    "Additional Comment",
                                    value = ""),
                                uiOutput(
                                    "renewable_energy_ui"),
                                uiOutput(
                                    "renewable_energy_fields_ui"),
                                actionButton(
                                    "add_building_consumption_record",
                                    "Add record")
                            ),
                            
                            #### vehicle ####
                            tabPanel(
                                title = "Vehicle",
                                id = "vehicle_inputs_consumption_record",
                                
                                selectInput(
                                    "vehicle_asset_consumption_record",
                                    "Select asset*",
                                    choices = ""),
                                selectInput(
                                    "vehicle_year_consumption_record",
                                    "Select a Reporting Year*",
                                    choices = c("Select a year" = "",
                                                reporting_year)),
                                selectInput(
                                    "fuel_select_vehicle_consumption_record",
                                    "Select fuel type*",
                                    choices = c(
                                        "Select a fuel type" = "")),
                                radioButtons(
                                    "fuel_or_mileage_consumption_record",
                                    "Data Submission Type*",
                                    choices = c("Fuel",
                                                "Mileage"),
                                    selected = NA),
                                numericInput(
                                    "vehicle_consumption_consumption_record",
                                    "Consumption / Mileage*",
                                    value = NA,
                                    min = 0),
                                selectInput(
                                    "vehicle_unit_consumption_record",
                                    "Select a unit*",
                                    choices = c("Select a unit" = "")),
                                dateRangeInput(
                                    "vehicle_date_range_consumption_record", 
                                    "Date Range* (yyyy-mm-dd)",
                                    start = NA,
                                    end = NA),
                                textInput(
                                    "vehicle_comment_consumption_record",
                                    "Additional Comment"),
                                actionButton(
                                    "add_vehicle_consumption_record",
                                    "Add record")
                            )
                            
                        )
                    ),
                    
                    ### table tab box ####
                    
                    column(
                        width = 9,
                        
                        tabBox(
                            title = "Consumption Record",
                            id = "consumption_record_table",
                            width = NULL,
                            
                            #### building ####
                            tabPanel(
                                title = "Building",
                                id = "building_consumption_record_table",
                                div(
                                    style = "overflow-x: auto; min-height: 100px;",  
                                    DTOutput("building_table_consumption_record")
                                )
                            ),
                            
                            #### vehicle ####
                            tabPanel(
                                title = "Vehicle",
                                id = "vehicle_consumption_record_table",
                                div(
                                    style = "overflow-x: auto; min_height: 100px:",
                                    DTOutput("vehicle_table_consumption_record")
                                )
                            )
                            
                        ),
                        
                        tabBox(
                            title = "Emission Record",
                            id = "emission_record_table",
                            width = NULL,
                            
                            #### building ####
                            tabPanel(
                                title = "Building",
                                id = "building_emission_record_table",
                                div(
                                    style = "overflow-x: auto; min-height: 100px;",    
                                    DTOutput("emission_record_building")
                                )
                            ),
                            
                            #### vehicle ####
                            tabPanel(
                                title = "Vehicle",
                                id = "vehicle_emission_record_table",
                                div(
                                    style = "overflow-x: auto; min-height: 100px;",
                                    DTOutput("emission_record_vehicle")
                                )
                            )
                        )
                        
                    )
                    
                )
            ),
            
            ## emission factor tab ####
            tabItem(
                tabName = "emission_factor_tab",
                
                fluidRow(
                    column(
                        width = 3,
                        
                        tabBox(
                            width = NULL,
                            
                            tabPanel(
                                title = "Grid Mix",
                                id = "grid_mix_input_emission_factor",
                                
                                radioButtons(
                                    "grid_mix_ui_selection_button",
                                    "Select a submission type",
                                    choices = c("Country-level",
                                                "City-level"),
                                    selected = NA
                                ),
                                
                                uiOutput("grid_mix_ui")
                                
                            )
                        )
                        
                    ),
                    
                    column(
                        width = 9,
                        
                        tabBox(
                            width = NULL,
                            
                            tabPanel(
                                title = "Grid Mix",
                                id = "grid_mix_emission_factor_table",
                                DTOutput("ele_grid_mix_table")
                            ),
                            tabPanel(
                                title = "FERA",
                                id = "FERA_emission_factor_table",
                                DTOutput("FERA_emission_factor_table")
                            ),
                            tabPanel(
                                title = "Scope 1 and 2",
                                id = "s1_2_emission_factor_table",
                                DTOutput("s1_2_emission_factor_table")
                            )
                        ),
                        
                        tabBox(
                            width = NULL,
                            title = "Grid mix default EF",
                            
                            tabPanel(
                                title = "",
                                id = "grid_mix_default_ef_table",
                                DTOutput("default_grid_mix_table")
                            )
                        )
                    )
                    
                )
            ),
            
            ## carbon inventory tab ####
            tabItem(
                tabName = "carbon_inventory_tab",
                
                fluidRow(
                    
                    h2("Placeholder")
                    
                )
                
            )
        )
    )
    
    
    
    # dashboard page content ----------------------------------------------------
    
    ## home tab server ####
    
    # clear asset table
    observeEvent(input$clear_asset, {
        dbExecute(
            pool, "DELETE FROM asset_building"
        )
        
        dbExecute(
            pool, "DELETE FROM asset_vehicle"
        )
        
        dbExecute(pool, "VACUUM")  # 清理空间
        
        load_asset_building()
        load_asset_vehicle()
        
        showNotification("Asset tables cleared!",
                         type = "message")
    })
    
    # clear consumption record table
    observeEvent(input$clear_consumption_record, {
        dbExecute(
            pool, "DELETE FROM consumption_record_building"
        )
        
        dbExecute(
            pool, "DELETE FROM consumption_record_vehicle"
        )
        
        dbExecute(pool, "VACUUM")  # 清理空间
        
        load_consumption_record_vehicle()
        load_consumption_record_building()
        
        showNotification("Consumption tables cleared!",
                         type = "message")
    })
    
    # clear grid mix table
    observeEvent(input$clear_grid_mix_emission_factor, {
        dbExecute(
            pool, "DELETE FROM grid_mix_emission_factor"
        )
        dbExecute(pool, "VACUUM")  # 清理空间
        
        load_grid_mix_emission_factor()
        
        showNotification("Grid Mix table cleared!",
                         type = "message")
    })
    
    # clear ALL
    observeEvent(input$clear_all, {
        # obtain all the table names
        names <- dbListTables(pool)
        
        # clear all the tables through loop
        for (i in names) {
            dbExecute(
                pool,
                sprintf("DELETE FROM %s", i)
            )
        }
        
        load_all()
        
        dbExecute(pool, "VACUUM")
        
        showNotification("All tables cleared!",
                         type = "message")
    })
    
    ## asset table ####
    
    ### update selectinput ####
    
    # building asset
    observeEvent(input$country_asset_building, {
        # create the unique city list of the chosen country
        temp_city_list <- city_df |> 
            filter(country == input$country_asset_building) |> 
            dplyr::distinct(name) |> 
            arrange(name) |> 
            pull(name)
        
        # update the select input
        updateSelectInput(
            session,
            "city_asset_building",
            choices = c(
                "Select a city" = "",
                temp_city_list
            )
        )
    })
    
    # vehicle asset
    observeEvent(input$country_asset_vehicle, {
        # create the unique city list of the chosen country
        temp_city_list <- city_df |> 
            filter(country == input$country_asset_vehicle) |> 
            dplyr::distinct(name) |> 
            arrange(name) |> 
            pull(name)
        
        # update the select input
        updateSelectInput(
            session,
            "city_asset_vehicle",
            choices = c(
                "Select a city" = "",
                temp_city_list
            )
        )
    })
    
    ### add new record ####
    
    #### building ####
    
    # initialise an empty table
    asset_table_building <- reactiveVal(NULL)
    
    # create a function that loads database into R (through updating the
    # existing NULL reactive value created above)
    load_asset_building <- function() {
        # first load the existing data from the database
        data <- dbGetQuery(pool,
                           "SELECT *
                           FROM asset_building") |> 
            mutate(`Creation Time` = as_datetime(
                `Creation Time`, 
                tz = tz(Sys.timezone())
            )
            )
        
        # then pass loaded data to the NULL reactive function just created
        asset_table_building(data)
    }
    
    # initialise the database (by running the above function when the app starts)
    observe({
        load_asset_building()
    })
    
    # record-adding workflow
    observeEvent(input$add_record_building_asset, {
        # Create a new record
        new_record <- tibble(
            Country = input$country_asset_building,
            City = input$city_asset_building,
            "Asset Type" = "Building",
            "Asset Name" = input$building_asset_name_asset,
            "Office Floor Area" = input$office_area_asset,
            "Area Unit" = input$area_unit_asset,
            "Subleased?" = input$subleased_asset,
            "Applicable Emission Sources" = paste(
                input$applicable_source_asset, collapse = ";"),
            "Creation Time" = Sys.time()
        )
        
        
        # Check for incomplete record
        if (
            !nzchar(new_record$Country) |
            !nzchar(new_record$`Asset Name`) |
            !nzchar(new_record$`Applicable Emission Sources`)
        ) {
            showNotification("Incomplete record!", 
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
        
        
        
        # Check for duplicate 
        existing_data <- asset_table_building()
        duplicate <- any(existing_data$`Asset Name` == new_record$`Asset Name`)
        
        if (duplicate) {
            showNotification("Building asset already exists!", 
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # update the database by appending the new record
        dbWriteTable(pool, "asset_building", new_record, append = TRUE)
        showNotification("New building record added",
                         type = "message",
                         closeButton = TRUE)
        
        # refresh the table showing in R by running the loading function again
        load_asset_building()
        
        # clear inputs
        updateSelectInput(
            session,
            "country_asset_building",
            selected = ""
        )
        
        updateSelectInput(
            session,
            "city_asset_building",
            selected = ""
        )
        
        updateTextInput(session,
                        "building_asset_name_asset",
                        value = NA)
        
        updateNumericInput(session, 
                           "office_area_asset",
                           value = numeric())
        
        updateSelectInput(session,
                          "area_unit_asset",
                          selected = "")
        
        updateCheckboxInput(session,
                            "subleased_asset",
                            value = FALSE)
        
        updateSelectInput(session,
                          "applicable_source_asset",
                          selected = "")
        
    })
    
    #### vehicle ####
    
    # empty table
    asset_table_vehicle <- reactiveVal(NULL)
    
    # create a function that loads database into R (through updating the
    # existing NULL reactive value created above)
    load_asset_vehicle <- function() {
        # first load the existing data from the database
        data <- dbGetQuery(pool,
                           "SELECT *
                           FROM asset_vehicle") |> 
            mutate(`Creation Time` = as_datetime(
                `Creation Time`, 
                tz = tz(Sys.timezone())
            )
            )
        
        # then pass loaded data to the NULL reactive function just created
        asset_table_vehicle(data)
    }
    
    # initialise the database (by running the above function when the app starts)
    observe({
        load_asset_vehicle()
    })
    
    
    # observe event to add new record
    observeEvent(input$add_record_vehicle_asset, {
        # Create a new record
        new_record <- tibble(
            Country = input$country_asset_vehicle,
            City = input$city_asset_vehicle,
            "Asset Type" = "Vehicle",
            "Asset Name" = input$vehicle_asset_name_asset,
            "Vehicle Type" = input$vehicle_type_asset,
            "Creation Time" = Sys.time())
        
        # check for incomplete record
        if (
            !nzchar(new_record$Country) |
            !nzchar(new_record$`Asset Name`) |
            !nzchar(new_record$`Vehicle Type`) 
        ) {
            showNotification("Incomplete record!",
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
        
        # Check for duplicate
        existing_data <- asset_table_vehicle()
        duplicate <- any(
            existing_data$`Asset Name` == new_record$`Asset Name` &
                existing_data$`Vehicle Type` == new_record$`Vehicle Type`)
        
        if (duplicate) {
            showNotification("Vehicle asset already exists!", 
                             type = "warning",
                             closeButton = TRUE)
            return()
        }
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # update the database by appending the new record
        dbWriteTable(pool, "asset_vehicle", new_record, append = TRUE)
        showNotification("New vehicle record added",
                         type = "message",
                         closeButton = TRUE)
        
        # refresh the table showing in R by running the loading function again
        load_asset_vehicle()
        
        # clear inputs
        updateSelectInput(
            session,
            "country_asset_vehicle",
            selected = ""
        )
        
        updateSelectInput(
            session,
            "city_asset_vehicle",
            selected = ""
        )
        
        updateTextInput(session,
                        "vehicle_asset_name_asset",
                        value = NA)
        
        updateSelectInput(session,
                          "vehicle_type_asset",
                          selected = "")
        
    })
    
    ## emission record sidebar ####
    
    ### update input fields ####
    
    #### asset names ####
    
    # building
    observe({
        
        building_asset_list <- asset_table_building() |>
            distinct(`Asset Name`) |>
            pull(`Asset Name`)
        
        updateSelectInput(session,
                          "building_asset_consumption_record",
                          choices = c("Select an asset" = "",
                                      building_asset_list))
    })
    
    
    # vehicle
    observe({
        
        vehicle_asset_list <- asset_table_vehicle() |>
            distinct(`Asset Name`) |>
            pull(`Asset Name`)
        
        updateSelectInput(session,
                          "vehicle_asset_consumption_record",
                          choices = c("Select an asset" = "",
                                      vehicle_asset_list))
    })
    
    #### start and end dates ####
    
    # building
    observeEvent(input$building_year_consumption_record, {
        req(nzchar(input$building_year_consumption_record))
        
        updateDateRangeInput(session,
                             "building_date_range_consumption_record",
                             min = 
                                 as.Date(
                                     paste0(input$building_year_consumption_record,
                                            "-01-01")),
                             max = as.Date(
                                 paste0(input$building_year_consumption_record,
                                        "-12-31"),
                                 start = NA,
                                 end = NA)
        )
        
    })
    
    # vehicle
    observeEvent(input$vehicle_year_consumption_record, {
        req(nzchar(input$vehicle_year_consumption_record))
        
        updateDateRangeInput(session,
                             "vehicle_date_range_consumption_record",
                             min = 
                                 as.Date(
                                     paste0(input$vehicle_year_consumption_record,
                                            "-01-01")),
                             max = as.Date(
                                 paste0(input$vehicle_year_consumption_record,
                                        "-12-31"))
        )
        
    })
    
    
    #### emission source ####
    
    # building
    observeEvent(input$building_asset_consumption_record, {
        
        emi_list <- asset_table_building() |> 
            filter(`Asset Name` == input$building_asset_consumption_record) |> 
            distinct(`Applicable Emission Sources`) |> 
            pull(`Applicable Emission Sources`) |> 
            str_split(";") |> 
            unlist() |> 
            sort()
        
        # update the emission sources drop down menu with the selected asset
        updateSelectInput(session, "fuel_select_building_consumption_record",
                          choices = c("Select a fuel type" = "",
                                      emi_list))
        
    })
    
    # vehicle
    observeEvent(input$vehicle_asset_consumption_record, {
        
        # note down the vehicle types relevant to that asset
        vehicle_type_list <- asset_table_vehicle() |>
            filter(`Asset Name` == input$vehicle_asset_consumption_record) |>
            distinct(`Vehicle Type`) |>
            pull(`Vehicle Type`)
        
        # Initialise an empty list to store fuel types
        fuel_list <- c("")
        
        # If "Electric" is in, add "Electricity"
        if ("Electric" %in% vehicle_type_list) {
            fuel_list <- c(fuel_list, "Electricity")
        }
        
        # If "Diesel" is in, add "Diesel"
        if ("Diesel" %in% vehicle_type_list) {
            fuel_list <- c(fuel_list, "Diesel")
        }
        
        # If "Petrol" is in, add "Petrol"
        if ("Petrol" %in% vehicle_type_list) {
            fuel_list <- c(fuel_list, "Petrol")
        }
        
        # If "Hybrid" is in, add "Electricity", "Petrol", and "Diesel"
        if ("Hybrid" %in% vehicle_type_list) {
            fuel_list <- c(fuel_list, "Electricity", "Petrol", "Diesel")
        }
        
        fuel_list <- unique(fuel_list)
        
        # Update the select input with the new fuel list
        updateSelectInput(session, "fuel_select_vehicle_consumption_record",
                          choices = fuel_list)
    })
    
    
    #### vehicle submission data type ####
    observeEvent(input$fuel_or_mileage_consumption_record, {
        
        if(input$fuel_or_mileage_consumption_record == "Fuel") {
            
            updateSelectInput(session,
                              "vehicle_unit_consumption_record",
                              choices = c("Select a unit" = "",
                                          "liters",
                                          "m3", 
                                          "US gallons",
                                          "imperial gallons")
            )
        } else
            if(input$fuel_or_mileage_consumption_record == "Mileage") {
                
                updateSelectInput(session,
                                  "vehicle_unit_consumption_record",
                                  choices = c("Select a unit" = "", 
                                              "miles", 
                                              "km")
                )
            }
        
    })
    
    
    #### renewable energy ui ####
    
    # pop up RE yes no question when electricity is selected as the fuel type
    output$renewable_energy_ui <- renderUI({
        req(input$fuel_select_building_consumption_record == "Electricity")
        
        radioButtons("renewable_yes_no_consumption_record",
                     "Is the energy from renewable source?*",
                     choices = c("Yes",
                                 "No"),
                     selected = "No")
        
    })
    
    
    # pop up additional fields if selected yes to the previous field
    output$renewable_energy_fields_ui <- renderUI({
        req(input$renewable_yes_no_consumption_record == "Yes")
        
        tagList(
            numericInput("renewable_energy_consumption_consumption_record",
                         "Renewable Energy Constumption (kWh)*",
                         min = 0,
                         value = NA),
            selectInput("renewable_energy_type_consumption_record",
                        "Renewable Energy Type*",
                        choices = c("Select an energy type" = "",
                                    renewable_energy_type))
        )
        
    })
    
    ## consumption record / emission record table ####
    
    ### building ####
    
    # initial tables
    building_table_consumption_record <- reactiveVal(NULL)
    emission_record_building <- reactiveVal(NULL)
    
    # function to cache database
    
    load_consumption_record_building <- function() {
        data <- dbGetQuery(
            pool,
            "SELECT *
                           FROM consumption_record_building") |> 
            mutate(
                `Creation Time` = 
                    as_datetime(
                        `Creation Time`, 
                        tz = tz(Sys.timezone())
                    ),
                `Start Date` = as_date(`Start Date`),
                `End Date` = as_date(`End Date`)
            )
        
        building_table_consumption_record(data)
    }
    
    load_emission_record_building <- function() {
        data <- dbGetQuery(
            pool,
            "SELECT *
            FROM emission_record_building") |> 
            mutate(
                `Creation Time` = 
                    as_datetime(
                        `Creation Time`, 
                        tz = tz(Sys.timezone())
                    ),
                `Start Date` = as_date(`Start Date`),
                `End Date` = as_date(`End Date`)
            )
        
        emission_record_building(data)
    }
    
    # initialise the database at the start
    observe({load_consumption_record_building()})
    observe({load_emission_record_building()})
    
    # Add new record: Building
    observeEvent(input$add_building_consumption_record, {
        # create a new record with the submitted values
        new_record <- tibble(
            "Asset Name" = input$building_asset_consumption_record,
            "Reporting Year" = input$building_year_consumption_record,
            "Fuel Type" = input$fuel_select_building_consumption_record,
            "Consumption" = input$building_consumption_consumption_record,
            "Unit" = input$building_unit_consumption_record,
            "Renewable?" = input$renewable_yes_no_consumption_record,
            "Renewable Energy Consumption (kWh)" = 
                input$renewable_energy_consumption_consumption_record,
            "Renewable Energy Type" = input$renewable_energy_type_consumption_record,
            "Start Date" = input$building_date_range_consumption_record[1],
            "End Date" = input$building_date_range_consumption_record[2],
            "Additional Comment" = input$building_comment_consumption_record,
            "Creation Time" = Sys.time()
        )
        
        # case for electricity 
        if (new_record$`Fuel Type` == "Electricity") {
            
            if (is.na(new_record$`Reporting Year`) |
                is.na(new_record$`Consumption`) |
                is.na(new_record$Unit) |
                is.na(new_record$`Start Date`) |
                is.na(new_record$`End Date`) |
                length(new_record$`Renewable?`) == 0) {
                showNotification("Incomplete record!",
                                 type = "warning",
                                 closeButton = TRUE)
                return()
            }
        } 
        else { # case for non-electricity
            
            if (is.na(new_record$`Reporting Year`) |
                is.na(new_record$`Consumption`) |
                is.na(new_record$Unit) |
                is.na(new_record$`Start Date`) |
                is.na(new_record$`End Date`)) {
                showNotification("Incomplete record!",
                                 type = "warning",
                                 closeButton = TRUE)
                return()
            }
            
        }
        
        
        # check for incomplete submission for renewable energy fields if fuel type
        # is electricity
        if(new_record$`Fuel Type` == "Electricity") { # case for electricity
            if(new_record$`Renewable?` == "Yes") { # case for yes for Renewable?
                if(
                    (is.na(new_record$`Renewable Energy Consumption (kWh)`) |
                     !nzchar(new_record$`Renewable Energy Type`)
                    )
                ) {
                    showNotification(
                        "Incomplete record! (Check renewable energy fields)",
                        type = "warning",
                        closeButton = TRUE)
                    return()
                }
            } 
        }
        else {} # case for non-electricity, no operations needed, proceed to 
        # the next step
        
        
        # check for duplicate record
        existing_table <- building_table_consumption_record()
        
        duplicate <- any(
            existing_table$`Asset Name` == new_record$`Asset Name` &
                existing_table$`Fuel Type` == new_record$`Fuel Type` &
                existing_table$`Start Date` == new_record$`Start Date` &
                existing_table$`End Date` == new_record$`End Date`
            
        )
        
        # warning message for duplicate record exists
        if (duplicate) {
            showNotification("Record already exists!", 
                             type = "warning")
            
            return()
        }
        
        # check end date strictly greater than or equal to start date
        if(new_record$`End Date` < new_record$`Start Date`) {
            showNotification(
                "Start date must be smaller than or equal to end date",
                type = "warning")
            
            return()
        }
        
        # check for duration overlaps
        existing_table <- building_table_consumption_record() |> 
            filter(`Asset Name` == new_record$`Asset Name`,
                   `Fuel Type` == new_record$`Fuel Type`)
        
        overlap <- any(
            
            (new_record$`Start Date` <= existing_table$`Start Date` &
                 new_record$`End Date` >= existing_table$`Start Date`),
            (new_record$`Start Date` >= existing_table$`Start Date` &
                 new_record$`End Date` <= existing_table$`End Date`),
            (new_record$`Start Date` <= existing_table$`End Date` &
                 new_record$`End Date` >= existing_table$`End Date`)
            
        )
        
        
        if (overlap) {
            showNotification("Overlapping duration!", 
                             type = "warning",
                             closeButton = TRUE)
            
            return()
        }
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # create a Hash ID for the record
        new_record$`Hash ID` <- apply(new_record, 
                                      1,
                                      digest, 
                                      algo = "murmur32")
        
        # save the new consumption record for later emission calculation
        new_building_consumption_record <- new_record
        
        # update the reactive value with new record
        dbWriteTable(pool,
                     "consumption_record_building",
                     new_record, append = TRUE)
        
        # refresh the table
        load_consumption_record_building()
        
        showNotification("New building consumption record added", 
                         type = "message",
                         closeButton = TRUE)
        
        # clear the inputs field
        updateNumericInput(session,
                           "building_consumption_consumption_record",
                           value = NA)
        
        updateSelectInput(session,
                          "fuel_select_building_consumption_record",
                          selected = "")
        
        updateSelectInput(session,
                          "building_unit_consumption_record",
                          selected = "")
        
        updateRadioButtons(session,
                           "renewable_yes_no_consumption_record",
                           selected = NA)
        
        updateNumericInput(session,
                           "renewable_energy_consumption_consumption_record",
                           value = NA)
        
        updateSelectInput(session,
                          "renewable_energy_type_consumption_record",
                          selected = "")
        
        updateDateRangeInput(session,
                             "building_date_range_consumption_record",
                             start = NA,
                             end = NA)
        
        updateTextInput(session,
                        "building_comment_consumption_record",
                        value = "")
        
        # procedure to auto-calculate the emission
        
        # compile the new record
        new_record <- tibble(
            `Consumption Record Hash ID` =
                new_building_consumption_record$`Hash ID`,
            `Asset Name` = new_building_consumption_record$`Asset Name`,
            `Fuel Type` = new_building_consumption_record$`Fuel Type`,
            `LB Emission` = 
                new_building_consumption_record$Consumption *
                default_grid_mix_table$Average,
            `MB Emission` = 0,
            `Start Date` = new_building_consumption_record$`Start Date`,
            `End Date` = new_building_consumption_record$`End Date`,
            `Creation Time` = Sys.time()
        )
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # create a Hash ID for the record
        new_record$`Hash ID` <- apply(new_record, 
                                      1,
                                      digest, 
                                      algo = "murmur32")
        
        # update the reactive value with new record
        dbWriteTable(pool,
                     "emission_record_building",
                     new_record,
                     append = TRUE)
        
        # refresh the table
        load_emission_record_building()
    })
    
    
    ### vehicle ####
    
    # initial table for vehicle consumption record
    vehicle_table_consumption_record <- reactiveVal(NULL)
    emission_record_vehicle <- reactiveVal(NULL)
    
    # create function to cache database 
    load_consumption_record_vehicle <- function() {
        data <- dbGetQuery(pool,
                           "SELECT *
                           FROM consumption_record_vehicle") |> 
            mutate(`Creation Time` = 
                       as_datetime(
                           `Creation Time`, 
                           tz = tz(Sys.timezone())
                       ),
                   `Start Date` = as_date(`Start Date`),
                   `End Date` = as_date(`End Date`)
            )
        
        vehicle_table_consumption_record(data)
    }
    
    load_emission_record_vehicle <- function() {
        
        # extract the table from the database
        data <- dbGetQuery(
            pool,
            "SELECT *
            FROM emission_record_vehicle"
        )
        
        # load the table into the empty reactive value
        emission_record_vehicle(data)
    }
    
    # initialise the database
    observe({load_consumption_record_vehicle()})
    observe({load_emission_record_vehicle()})
    
    # Add new record: Vehicle
    observeEvent(input$add_vehicle_consumption_record, {
        # create a new record with the submitted values
        new_record <- tibble(
            "Asset Name" = input$vehicle_asset_consumption_record,
            "Reporting Year" = input$vehicle_year_consumption_record,
            "Fuel Type" = input$fuel_select_vehicle_consumption_record,
            "Amount" = input$vehicle_consumption_consumption_record,
            "Data Type" = input$fuel_or_mileage_consumption_record,
            "Unit" = input$vehicle_unit_consumption_record,
            "Start Date" = input$vehicle_date_range_consumption_record[1],
            "End Date" = input$vehicle_date_range_consumption_record[2],
            "Additional Comment" = input$vehicle_comment_consumption_record,
            "Creation Time" = Sys.time()
        )
        
        # check for incomplete submission
        if (is.na(new_record$`Reporting Year`) |
            is.na(new_record$`Fuel Type`) |
            is.null(new_record$`Data Type`) |
            is.na(new_record$`Amount`) |
            is.na(new_record$`Start Date`) |
            is.na(new_record$`End Date`)) {
            
            showNotification("Incomplete record!",
                             type = "warning",
                             closeButton = TRUE)
            return()
            
        }
        
        
        # check for duplicate record
        existing_table <- vehicle_table_consumption_record()
        
        duplicate <- any(
            
            existing_table$`Asset Name` == new_record$`Asset Name` &
                existing_table$`Fuel Type` == new_record$`Fuel Type` &
                existing_table$`Start Date` == new_record$`Start Date` &
                existing_table$`End Date` == new_record$`End Date`
            
        )
        
        # warning message
        if (duplicate) {
            showNotification("Record already exists!", 
                             type = "warning",
                             closeButton = TRUE)
            
            return()
        }
        
        # check end date strictly greater than or equal to start date
        if(new_record$`End Date` < new_record$`Start Date`) {
            showNotification(
                "Start date must be smaller than or equal to end date",
                type = "warning")
            
            return()
        }
        
        # check for duration overlaps
        existing_table <- vehicle_table_consumption_record() |> 
            filter(`Asset Name` == new_record$`Asset Name`,
                   `Fuel Type` == new_record$`Fuel Type`)
        
        overlap <- any(
            
            (new_record$`Start Date` <= existing_table$`Start Date` &
                 new_record$`End Date` >= existing_table$`Start Date`),
            (new_record$`Start Date` >= existing_table$`Start Date` &
                 new_record$`End Date` <= existing_table$`End Date`),
            (new_record$`Start Date` <= existing_table$`End Date` &
                 new_record$`End Date` >= existing_table$`End Date`)
            
        )
        
        
        if (overlap) {
            showNotification("Overlapping duration!", 
                             type = "warning",
                             closeButton = TRUE)
            
            return()
        }
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # create a Hash ID for the record
        new_record$`Hash ID` <- apply(new_record, 
                                      1,
                                      digest, 
                                      algo = "murmur32")
        
        # save the Hash ID in the global envi.
        new_vehicle_consumption_record <- new_record
        
        # update the reactive value with new record
        dbWriteTable(pool,
                     "consumption_record_vehicle",
                     new_record,
                     append = TRUE)
        
        # refresh the table in the database by running the function 
        load_consumption_record_vehicle()
        
        # show msg that record has been added
        showNotification("New vehicle consumption record added",
                         type = "message",
                         closeButton = TRUE)
        
        # clear input fields
        updateNumericInput(session,
                           "vehicle_consumption_consumption_record",
                           value = NA)
        
        updateSelectInput(session,
                          "fuel_select_vehicle_consumption_record",
                          selected = "")
        
        updateSelectInput(session,
                          "vehicle_unit_consumption_record",
                          selected = "")
        
        updateDateRangeInput(session,
                             "vehicle_date_range_consumption_record",
                             start = NA,
                             end = NA)
        
        updateTextInput(session,
                        "vehicle_comment_consumption_record",
                        value = "")
        
        # for emission record below
        new_record <- tibble(
            `Consumption Record Hash ID` =
                new_vehicle_consumption_record$`Hash ID`,
            `Asset Name` = new_vehicle_consumption_record$`Asset Name`,
            `Fuel Type` = new_vehicle_consumption_record$`Fuel Type`,
            `LB Emission` = 0,
            `MB Emission` = 0,
            `Start Date` = new_vehicle_consumption_record$`Start Date`,
            `End Date` = new_vehicle_consumption_record$`End Date`,
            `Creation Time` = Sys.time()
        )
        
        # convert POSIXct and Date variable as numeric
        new_record <- new_record |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # create a Hash ID for the record
        new_record$`Hash ID` <- apply(new_record, 
                                      1,
                                      digest, 
                                      algo = "murmur32")
        
        # update the reactive value with new record
        dbWriteTable(pool,
                     "emission_record_vehicle",
                     new_record,
                     append = TRUE)
        
        # refresh the table
        load_emission_record_vehicle()
        
        
    })
    
    ## emission factor table ####
    
    ## update ui ####
    
    # update city selectInput choices in the ui
    observe({
        req(
            input$grid_mix_ui_selection_button == "City-level" |
                nzchar(input$country_emission_factor)
        )
        
        # get the unique city names from of the chosen country
        temp_city_list <- city_df |> 
            filter(country == input$country_emission_factor) |> 
            distinct(name)
        
        updateSelectInput(session,
                          "city_emission_factor",
                          choices = c("Select a city" = "",
                                      temp_city_list),
                          selected = ""
        )
    })
    
    # emission factors are grouped by country, city, and emission source
    
    # 1. Electricity grid mix
    
    # render different ui depending on whether the user is submitting the 
    # grid mix at country level or city level
    
    output$grid_mix_ui <- renderUI({
        req(
            input$grid_mix_ui_selection_button
        ) # only initiate when the button is clicked
        
        if(input$grid_mix_ui_selection_button == "Country-level") {
            
            tagList(
                
                selectInput(
                    "country_emission_factor",
                    "Select a country*",
                    choices = c("Select a country" = "",
                                country_list)
                ),
                
                numericInput(
                    "coal_mix_emission_factor",
                    "Share of coal generation (%)",
                    value = NA,
                    min = 0
                ),
                
                numericInput(
                    "oil_mix_emission_factor",
                    "Share of oil generation (%)",
                    value = NA,
                    min = 0
                ),
                
                numericInput(
                    "gas_mix_emission_factor",
                    "Share of gas generation (%)",
                    value = NA,
                    min = 0
                ),
                
                numericInput(
                    "nuclear_mix_emission_factor",
                    "Share of nuclear generation (%)",
                    value = NA,
                    min = 0
                ),
                
                numericInput(
                    "renewables_mix_emission_factor",
                    "Share of renewables generation (%)",
                    value = NA,
                    min = 0
                ),
                
                textInput(
                    "remark_emission_factor",
                    "Remark",
                    value = NA
                ),
                
                actionButton(
                    "add_record_grid_mix_emission_factor",
                    "Add record"
                ),
                
                textOutput("grid_mix_sum_left")
            )            
        } else
            
            if(input$grid_mix_ui_selection_button == "City-level") {
                
                tagList(
                    
                    selectInput(
                        "country_emission_factor",
                        "Select a country*",
                        choices = c("Select a country" = "",
                                    country_list)
                    ),
                    
                    selectInput(
                        "city_emission_factor",
                        "Select a city*",
                        choices = c("Select a city" = "")
                    ),
                    
                    numericInput(
                        "coal_mix_emission_factor",
                        "Share of coal generation (%)",
                        value = NA,
                        min = 0
                    ),
                    
                    numericInput(
                        "oil_mix_emission_factor",
                        "Share of oil generation (%)",
                        value = NA,
                        min = 0
                    ),
                    
                    numericInput(
                        "gas_mix_emission_factor",
                        "Share of gas generation (%)",
                        value = NA,
                        min = 0
                    ),
                    
                    numericInput(
                        "nuclear_mix_emission_factor",
                        "Share of nuclear generation (%)",
                        value = NA,
                        min = 0
                    ),
                    
                    numericInput(
                        "renewables_mix_emission_factor",
                        "Share of renewables generation (%)",
                        value = NA,
                        min = 0
                    ),
                    
                    textInput(
                        "remark_emission_factor",
                        "Remark",
                        value = NA
                    ),
                    
                    actionButton(
                        "add_record_grid_mix_emission_factor",
                        "Add record"
                    ),
                    
                    textOutput("grid_mix_sum_left")
                )
            }        
        
    })
    
    # initialise an empty table
    ele_grid_mix_table <- reactiveVal(NULL)
    
    # create function that loads database
    load_grid_mix_emission_factor <- function() {
        # first load the existing data
        data <- dbGetQuery(pool, "SELECT *
                           FROM grid_mix_emission_factor")
        
        # then pass the existing data to a reactive function in R
        ele_grid_mix_table(data)
        
    }
    
    
    
    # initialise the database
    observe({
        load_grid_mix_emission_factor()
    })
    
    # add record workflow
    observeEvent(input$add_record_grid_mix_emission_factor, {
        
        if(input$grid_mix_ui_selection_button == "Country-level") {
            
            # check country input is selected
            if(
                !nzchar(input$country_emission_factor)
            ) {
                showNotification("Incomeplete record!",
                                 type = "warning")
                
                return()
            }
            
            # check the sum of the mix must not exceed 1
            if(
                sum(
                    c(input$coal_mix_emission_factor,
                      input$oil_mix_emission_factor,
                      input$gas_mix_emission_factor,
                      input$nuclear_mix_emission_factor,
                      input$renewables_emission_factor),
                    na.rm = TRUE
                ) / 100 > 1
            ) {
                showNotification("The sum of mix cannot exceed 1!",
                                 type = "warning")
                
                return()
            }
            
            new_table <<- tibble(
                Country = input$country_emission_factor,
                City = NA_character_,
                Coal = input$coal_mix_emission_factor,
                Oil = input$oil_mix_emission_factor,
                Gas = input$gas_mix_emission_factor,
                Nuclear = input$nuclear_mix_emission_factor,
                Renewables = input$renewables_mix_emission_factor,
                Remark = input$remark_emission_factor
            )
            
        } else
            
            if(input$grid_mix_ui_selection_button == "City-level") {
                
                # check both country and city inputs are selected
                if(
                    !nzchar(input$country_emission_factor) |
                    !nzchar(input$city_emission_factor)
                ) {
                    showNotification("Incomplete record!",
                                     type = "warning")
                    
                    return()
                } 
                
                # check the sum of the mix must not exceed 1
                if(
                    sum(
                        c(input$coal_mix_emission_factor,
                          input$oil_mix_emission_factor,
                          input$gas_mix_emission_factor,
                          input$nuclear_mix_emission_factor,
                          input$renewables_emission_factor),
                        na.rm = TRUE
                    ) / 100 > 1
                ) {
                    showNotification("The sum of mix cannot exceed 1!",
                                     type = "warning")
                    
                    return()
                }
                
                new_table <- tibble(
                    Country = input$country_emission_factor,
                    City = input$city_emission_factor,
                    Coal = input$coal_mix_emission_factor,
                    Oil = input$oil_mix_emission_factor,
                    Gas = input$gas_mix_emission_factor,
                    Nuclear = input$nuclear_mix_emission_factor,
                    Renewables = input$renewables_mix_emission_factor
                )
                # create the object in Global envi
                new_table <<- new_table 
                
            }
        
        
        
        # convert POSIXct and Date variable as numeric
        new_table <- new_table |>
            mutate(across(  # 对多列同时进行修改
                # 选择所有日期时间列
                .cols = where(~ inherits(., "POSIXct") | inherits(., "Date")), 
                .fns = as.numeric  # 把这些列转换成数字
            ))
        
        # update the data with the new table
        dbWriteTable(pool, "grid_mix_emission_factor", new_table, append = TRUE)
        
        # refresh the table
        load_grid_mix_emission_factor()
        
        # clear the input fields
        updateSelectInput(
            session,
            "country_emission_factor",
            selected = ""
        )
        
        updateSelectInput(
            session,
            "city_emission_factor",
            selected = ""
        )
        
        updateNumericInput(
            session,
            "coal_mix_emission_factor",
            value = NA
        )
        
        updateNumericInput(
            session,
            "oil_mix_emission_factor",
            value = NA
        )
        
        updateNumericInput(
            session,
            "gas_mix_emission_factor",
            value = NA
        )
        
        updateNumericInput(
            session,
            "nuclear_mix_emission_factor",
            value = NA
        )
        
        updateNumericInput(
            session,
            "renewables_mix_emission_factor",
            value = NA
        )
        
        updateTextInput(
            session,
            "remark_emission_factor",
            value = NA
        )
        
        
    })
    
    # 1.2 Default grid mix emission factor
    default_grid_mix_table <- tibble(
        Country = "China",
        City = NA,
        Coal = 348.84,
        Oil = 263.88,
        Gas = round(56.1*1000000/277778, 2),
        Nuclear = 12,
        Renewables = 0,
        Average = (new_table$Coal * 
                       Coal + new_table$Oil * 
                       Oil + new_table$Gas *
                       Gas + new_table$Nuclear *
                       Nuclear + new_table$Renewables *
                       Renewables) / 100,
        Remark = "https://data.ncsc.org.cn/factoryes/indexMod/indexModIlibrary?
        templateName=%E8%A1%8C%E4%B8%9A%E4%BC%81%E4%B8%9A%E6%8E%92%E6%94%BE%E5%
        9B%A0%E5%AD%90"
    )
    
    # 2. FERA values for scope 1 and 2 all fuels
    
    # 3. emission factor for scope 1 and 2 all fuels 
    
    
    # render ------------------------------------------------------------------
    
    # display message to ask user to select an asset type
    
    ## asset ####
    
    # building table
    output$asset_table_building <- renderDT({
        datatable(
            asset_table_building() |> 
                select(-c(`Creation Time`)
                ),
            selection = "single")
    })
    
    # vehicle table
    output$asset_table_vehicle <- renderDT({
        datatable(
            asset_table_vehicle() |> 
                select(-c(`Creation Time`)
                ), 
            selection = "single")
    })
    
    
    ## consumption record ####
    
    # building table
    output$building_table_consumption_record <- renderDT({
        datatable(building_table_consumption_record() |> 
                      select(-c(`Creation Time`)),
                  selection = "single")
    })
    
    # vehicle table 
    output$vehicle_table_consumption_record <- renderDT({
        datatable(
            vehicle_table_consumption_record() |> 
                select(
                    -c(`Creation Time`)
                ),
            selection = "single")
    })
    
    ## emission record ####
    
    # building
    output$emission_record_building <- renderDT({
        datatable(
            emission_record_building() |> 
                select(
                    -c(`Hash ID`, `Creation Time`)
                ),
            selection = "single"
        )
    })
    
    # vehicle
    output$emission_record_vehicle <- renderDT({
        datatable(
            emission_record_vehicle() |> 
                select(
                    -c(`Creation Time`)
                ),
            selection = "single"
        )
    })
    
    
    ## emission factor ####
    
    # grid mix table
    output$ele_grid_mix_table <- renderDT({
        datatable(ele_grid_mix_table(),
                  selection = "single",
                  options = list(dom = 't'))
    })
    
    # default grid mix emission factor
    output$default_grid_mix_table <- renderDT({
        datatable(default_grid_mix_table,
                  selection = "single",
                  options = list(dom = "t")
        )
    })
    
    # grid mix sum text output
    output$grid_mix_sum_left <- renderText({
        
        paste("(Remaining share:", 
              round(
                  100 - sum(
                      c(input$coal_mix_emission_factor,
                        input$oil_mix_emission_factor,
                        input$gas_mix_emission_factor,
                        input$nuclear_mix_emission_factor,
                        input$renewables_mix_emission_factor),
                      na.rm = TRUE
                  ),
                  2
              ),
              "%)"
        )
    })
    
    # FERA table
    output$FERA_emission_factor_table <- renderDT({
        datatable(city_df,
                  selection = "single",
                  options = list(dom = 't'))
    })
    
    # S1 and S2 table
    output$s1_2_emission_factor_table <- renderDT({
        datatable(city_df,
                  selection = "single",
                  options = list(dom = 't'))
    })
    
    
    
}




# Run app -----------------------------------------------------------------
shinyApp(ui, server)

