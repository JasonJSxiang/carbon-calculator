library(DBI)
library(RSQLite)
library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(shinydashboard)
library(uuid)
library(plotly)


# Global variables -------------------------------------------------------
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

# # vehicle fuel / mileage unit
# vehicle_fuel_mileage_unit <- c("liters", "m3", "US gallons", 
#                                "imperial gallons", "miles", "km")

# reporting year
reporting_year <- c(2025:2015)

# renewable energy type
renewable_energy_type <- c("Solar", "Wind", "Hydro", "Biomass")

# country and city list (from maps package)
world_cities <- world.cities |> 
    rename("country" = "country.etc") |> # rename the country col
    mutate(name = str_replace(name, "^'", "")) # remove the leading ' in the col


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
    
    
    # dashboard body (dynamic) ----------------------------------------------------
    
    output$db_body <- renderUI(
        tabItems(
            ## home tab ####
            tabItem(tabName = "home_tab",
                    h2("Carbon Calculator 张译翔")
            ),
            
            ## asset tab ####
            tabItem(tabName = "asset_tab",
                    
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
                                    textInput(
                                        "building_asset_name_asset", "Asset Name*"
                                    ),
                                    numericInput(
                                        "office_area_asset", "Occupied Floor Area",
                                        value = NA,
                                        min = 0
                                    ),
                                    selectInput(
                                        "area_unit_asset", "Area Unit",
                                        choices = c("Select a unit" = "",
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
                        column(width = 9,
                               
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
            tabItem(tabName = "emission_record_tab",
                    
                    fluidRow(
                        
                        ### inputs tab box ####
                        column(width = 3,
                               
                               tabBox(
                                   title = NULL,
                                   id = "emission_record_inputs",
                                   width = NULL,
                                   
                                   #### building ####
                                   tabPanel(
                                       title = "Building",
                                       id = "building_inputs_emission_record",
                                       
                                       selectInput(
                                           "building_asset_emission_record",
                                           "Select asset*",
                                           choices = ""),
                                       selectInput(
                                           "building_year_emission_record",
                                           "Select a Reporting Year*",
                                           choices = c("Select a year" = "",
                                                       reporting_year)),
                                       selectInput(
                                           "fuel_select_building_emission_record",
                                           "Select fuel type*",
                                           choices = c("Select a fuel type" = "")),
                                       numericInput(
                                           "building_consumption_emission_record",
                                           "Energy consumption*",
                                           value = NA,
                                           min = 0),
                                       selectInput(
                                           "building_unit_emission_record",
                                           "Select a unit",
                                           choices = c("Select a unit" = "",
                                                       building_fuel_unit)),
                                       dateRangeInput(
                                           "building_date_range_emission_record", 
                                           "Date Range*",
                                           start = NA, end = NA),
                                       textInput(
                                           "building_comment_emission_record",
                                           "Additional Comment",
                                           value = ""),
                                       uiOutput(
                                           "renewable_energy_ui"),
                                       uiOutput(
                                           "renewable_energy_fields_ui"),
                                       actionButton(
                                           "add_building_record_emission_record",
                                           "Add record")
                                   ),
                                   
                                   #### vehicle ####
                                   tabPanel(
                                       title = "Vehicle",
                                       id = "vehicle_inputs_emission_record",
                                       
                                       selectInput(
                                           "vehicle_asset_emission_record",
                                           "Select asset*",
                                           choices = ""),
                                       selectInput(
                                           "vehicle_year_emission_record",
                                           "Select a Reporting Year*",
                                           choices = c("Select a year" = "",
                                                       reporting_year)),
                                       selectInput(
                                           "fuel_select_vehicle_emission_record",
                                           "Select fuel type*",
                                           choices = c(
                                               "Select a fuel type" = "")),
                                       radioButtons(
                                           "fuel_or_mileage_emission_record",
                                           "Data Submission Type*",
                                           choices = c("Fuel",
                                                       "Mileage"),
                                           selected = NA),
                                       numericInput(
                                           "vehicle_consumption_emission_record",
                                           "Consumption / Mileage*",
                                           value = NA,
                                           min = 0),
                                       selectInput(
                                           "vehicle_unit_emission_record",
                                           "Select a unit*",
                                           choices = c("Select a unit" = "")),
                                       dateRangeInput(
                                           "vehicle_date_range_emission_record", 
                                           "Date Range*"),
                                       textInput(
                                           "vehicle_comment_emission_record",
                                           "Additional Comment"),
                                       actionButton(
                                           "add_vehicle_record_emission_record",
                                           "Add record")
                                   )
                                   
                               )
                        ),
                        
                        ### table tab box ####
                        
                        column(width = 9,
                               
                               tabBox(
                                   title = NULL,
                                   id = "emission_record_table",
                                   width = NULL,
                                   
                                   #### building ####
                                   tabPanel(
                                       title = "Building",
                                       id = "building_emission_record_table",
                                       div(
                                           style = "overflow-x: auto; min-height: 100px;",  
                                           DTOutput("building_table_emission_record")
                                       )
                                   ),
                                   
                                   #### vehicle ####
                                   tabPanel(
                                       title = "Vehicle",
                                       id = "vehicle_emission_record_table",
                                       div(
                                           style = "overflow-x: auto; min_height: 100px:",
                                           DTOutput("vehicle_table_emission_record")
                                       )
                                   )
                                   
                               )
                               
                        )
                        
                    )
            ),
            
            ## emission factor tab ####
            tabItem(tabName = "emission_factor_tab",
                    
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
                        
                        column(width = 9,
                               
                               tabBox(width = 12,
                                      tabPanel(
                                          title = "Grid Mix",
                                          id = "grix_mix_emission_factor_table",
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
                               )
                        )
                        
                    )
            ),
            
            ## carbon inventory tab ####
            tabItem(tabName = "carbon_inventory_tab",
                    
                    fluidRow(
                        
                        h2("Placeholder")
                        
                    )
                    
            )
        )
    )
    
    
    
    # dashboard page content ----------------------------------------------------
    
    ## asset table ####
    
    ### add new record ####
    
    #### building ####
    
    # empty table
    asset_table_building <- reactiveVal(
        tibble(
            "ID" = character(0), 
            "Asset Name" = character(0),
            "Asset Type" = character(0),
            "Office Floor Area" = numeric(0),
            "Area Unit" = character(0),
            "Subleased?" = logical(0),
            "Applicable Emission Sources" = character(0),
            "Creation Time" = as.POSIXct(NA)
        )
    )
    
    observeEvent(input$add_record_building_asset, {
        # Create a new record
        new_record <- tibble(
            "ID" = UUIDgenerate(),
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
        
        # update the reactive table
        asset_table_building(bind_rows(existing_data, new_record))
        showNotification("New building record added",
                         type = "message",
                         closeButton = TRUE)
        
        # clear inputs
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
    asset_table_vehicle <- reactiveVal(
        tibble(
            "ID" = character(0),
            "Asset Name" = character(0),
            "Asset Type" = character(0),
            "Vehicle Type" = character(0),
            "Creation Time" = as.POSIXct(NA)
        )
    )
    
    # observe event to add new record
    observeEvent(input$add_record_vehicle_asset, {
        # Create a new record
        new_record <- tibble(
            "ID" = UUIDgenerate(),
            "Asset Type" = "Vehicle",
            "Asset Name" = input$vehicle_asset_name_asset,
            "Vehicle Type" = input$vehicle_type_asset,
            "Creation Time" = Sys.time())
        
        # check for incomplete record
        if (
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
        
        # update the reactive table
        asset_table_vehicle(bind_rows(existing_data, new_record))
        showNotification("New vehicle record added", 
                         type = "message",
                         closeButton = TRUE)
        
        # clear inputs
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
                          "building_asset_emission_record",
                          choices = c("Select an asset" = "",
                                      building_asset_list))
    })
    
    
    # vehicle
    observe({
        
        vehicle_asset_list <- asset_table_vehicle() |>
            distinct(`Asset Name`) |>
            pull(`Asset Name`)
        
        updateSelectInput(session,
                          "vehicle_asset_emission_record",
                          choices = c("Select an asset" = "",
                                      vehicle_asset_list))
    })
    
    #### start and end dates ####
    
    # building
    observeEvent(input$building_year_emission_record, {
        req(nzchar(input$building_year_emission_record))
        
        updateDateRangeInput(session,
                             "building_date_range_emission_record",
                             min = 
                                 as.Date(
                                     paste0(input$building_year_emission_record,
                                            "-01-01")),
                             max = as.Date(
                                 paste0(input$building_year_emission_record,
                                        "-12-31"),
                                 start = NA,
                                 end = NA)
        )
        
    })
    
    # vehicle
    observeEvent(input$vehicle_year_emission_record, {
        req(nzchar(input$vehicle_year_emission_record))
        
        updateDateRangeInput(session,
                             "vehicle_date_range_emission_record",
                             min = 
                                 as.Date(
                                     paste0(input$vehicle_year_emission_record,
                                            "-01-01")),
                             max = as.Date(
                                 paste0(input$vehicle_year_emission_record,
                                        "-12-31"))
        )
        
    })
    
    
    #### emission source ####
    
    # building
    observeEvent(input$building_asset_emission_record, {
        
        emi_list <- asset_table_building() |> 
            filter(`Asset Name` == input$building_asset_emission_record) |> 
            distinct(`Applicable Emission Sources`) |> 
            pull(`Applicable Emission Sources`) |> 
            str_split(";") |> 
            unlist() |> 
            sort()
        
        # update the emission sources drop down menu with the selected asset
        updateSelectInput(session, "fuel_select_building_emission_record",
                          choices = c("Select a fuel type" = "",
                                      emi_list))
        
    })
    
    # vehicle
    observeEvent(input$vehicle_asset_emission_record, {
        
        # note down the vehicle types relevant to that asset
        vehicle_type_list <- asset_table_vehicle() |>
            filter(`Asset Name` == input$vehicle_asset_emission_record) |>
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
        updateSelectInput(session, "fuel_select_vehicle_emission_record",
                          choices = fuel_list)
    })
    
    
    #### vehicle submission data type ####
    observeEvent(input$fuel_or_mileage_emission_record, {
        
        if(input$fuel_or_mileage_emission_record == "Fuel") {
            
            updateSelectInput(session,
                              "vehicle_unit_emission_record",
                              choices = c("Select a unit" = "",
                                          "liters",
                                          "m3", 
                                          "US gallons",
                                          "imperial gallons")
            )
        } else
            if(input$fuel_or_mileage_emission_record == "Mileage") {
                
                updateSelectInput(session,
                                  "vehicle_unit_emission_record",
                                  choices = c("Select a unit" = "", 
                                              "miles", 
                                              "km")
                )
            }
        
    })
    
    
    #### renewable energy ui ####
    
    # pop up RE yes no question when electricity is selected as the fuel type
    output$renewable_energy_ui <- renderUI({
        req(input$fuel_select_building_emission_record == "Electricity")
        
        radioButtons("renewable_yes_no_emission_record",
                     "Is the energy from renewable source?*",
                     choices = c("Yes",
                                 "No"),
                     selected = "No")
        
    })
    
    
    # pop up additional fields if selected yes to the previous field
    output$renewable_energy_fields_ui <- renderUI({
        req(input$renewable_yes_no_emission_record == "Yes")
        
        tagList(
            numericInput("renewable_energy_consumption_emission_record",
                         "Renewable Energy Constumption (kWh)*",
                         min = 0,
                         value = NA),
            selectInput("renewable_energy_type_emission_record",
                        "Renewable Energy Type*",
                        choices = c("Select an energy type" = "",
                                    renewable_energy_type))
        )
        
    })
    
    ## emission record table ####
    
    ### building ####
    
    # initial table for building emission record
    building_table_emission_record <- reactiveVal(
        tibble(
            "ID" = character(0),
            "Asset Name" = character(0),
            "Reporting Year" = character(0),
            "Fuel Type" = character(0),
            "Consumption" = numeric(0),
            "Unit" = character(0),
            "Renewable?" = character(0),
            "Renewable Energy Consumption (kWh)" = numeric(0),
            "Renewable Energy Type" = character(0),
            "Start Date" = as.Date(numeric(0)),
            "End Date" = as.Date(numeric(0)),
            "Additional Comment" = character(0),
            "Creation Time" = as.POSIXct(NA)
        )
    )
    
    
    # Add new record: Building
    observeEvent(input$add_building_record_emission_record, {
        # create a new record with the submitted values
        new_record <- tibble(
            "ID" = UUIDgenerate(),
            "Asset Name" = input$building_asset_emission_record,
            "Reporting Year" = input$building_year_emission_record,
            "Fuel Type" = input$fuel_select_building_emission_record,
            "Consumption" = input$building_consumption_emission_record,
            "Unit" = input$building_unit_emission_record,
            "Renewable?" = input$renewable_yes_no_emission_record,
            "Renewable Energy Consumption (kWh)" = 
                input$renewable_energy_consumption_emission_record,
            "Renewable Energy Type" = input$renewable_energy_type_emission_record,
            "Start Date" = input$building_date_range_emission_record[1],
            "End Date" = input$building_date_range_emission_record[2],
            "Additional Comment" = input$building_comment_emission_record,
            "Creation Time" = Sys.time()
        )
        
        # # conditionally add the renewable col if electricity is selected
        # if(input$fuel_select_building_emission_record == "Electricity") {
        #   new_record <- new_record |> 
        #     mutate("Renewable?" = input$renewable_yes_no_emission_record)
        # }
        
        # check for incomplete submission 
        # (pre checking before renewable energy fields)
        
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
        else {} # case for non-electricity, no operations needed, proceed to the next step
        
        
        # check for duplicate record
        existing_table <- building_table_emission_record()
        
        duplicate <- any(
            existing_table$`Asset Name` == new_record$`Asset Name` &
                existing_table$`Fuel Type` == new_record$`Fuel Type` &
                existing_table$`Start Date` == new_record$`Start Date` &
                existing_table$`End Date` == new_record$`End Date`
            
        )
        
        # warning message
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
        existing_table <- building_table_emission_record() |> 
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
        
        
        # update the reactive value with new record
        updated_table <- building_table_emission_record() |> 
            bind_rows(new_record)
        
        building_table_emission_record(updated_table)
        
        showNotification("New building emission record added", 
                         type = "message",
                         closeButton = TRUE)
        
        # clear the inputs field
        updateNumericInput(session,
                           "building_consumption_emission_record",
                           value = NA)
        
        updateSelectInput(session,
                          "fuel_select_building_emission_record",
                          selected = "")
        
        updateSelectInput(session,
                          "building_unit_emission_record",
                          selected = "")
        
        updateRadioButtons(session,
                           "renewable_yes_no_emission_record",
                           selected = NA)
        
        updateNumericInput(session,
                           "renewable_energy_consumption_emission_record",
                           value = NA)
        
        updateSelectInput(session,
                          "renewable_energy_type_emission_record",
                          selected = "")
        
        updateDateRangeInput(session,
                             "building_date_range_emission_record",
                             start = NA,
                             end = NA)
        
        updateTextInput(session,
                        "building_comment_emission_record",
                        value = "")
        
        
    })
    
    
    ### vehicle ####
    
    # initial table for vehicle emission record
    vehicle_table_emission_record <- reactiveVal(
        tibble(
            "ID" = character(0),
            "Asset Name" = character(0),
            "Reporting Year" = character(0),
            "Fuel Type" = character(0),
            "Data Type" = character(0),
            "Amount" = numeric(0),
            "Unit" = character(0),
            "Start Date" = as.Date(numeric(0)),
            "End Date" = as.Date(numeric(0)),
            "Additional Comment" = character(0),
            "Creation Time" = as.POSIXct(NA)
        )
    )
    
    
    # Add new record: Vehicle
    observeEvent(input$add_vehicle_record_emission_record, {
        # create a new record with the submitted values
        new_record <- tibble(
            "ID" = UUIDgenerate(),
            "Asset Name" = input$vehicle_asset_emission_record,
            "Reporting Year" = input$vehicle_year_emission_record,
            "Fuel Type" = input$fuel_select_vehicle_emission_record,
            "Amount" = input$vehicle_consumption_emission_record,
            "Data Type" = input$fuel_or_mileage_emission_record,
            "Unit" = input$vehicle_unit_emission_record,
            "Start Date" = input$vehicle_date_range_emission_record[1],
            "End Date" = input$vehicle_date_range_emission_record[2],
            "Additional Comment" = input$vehicle_comment_emission_record,
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
        existing_table <- vehicle_table_emission_record()
        
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
        existing_table <- vehicle_table_emission_record() |> 
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
        
        # update the reactive value with new record
        updated_table <- vehicle_table_emission_record() |> 
            bind_rows(new_record)
        
        vehicle_table_emission_record(updated_table)
        
        showNotification("New vehicle emission record added",
                         type = "message",
                         closeButton = TRUE)
        
        # clear input fields
        updateNumericInput(session,
                           "vehicle_consumption_emission_record",
                           value = NA)
        
        updateSelectInput(session,
                          "fuel_select_vehicle_emission_record",
                          selected = "")
        
        updateSelectInput(session,
                          "vehicle_unit_emission_record",
                          selected = "")
        
        updateDateRangeInput(session,
                             "vehicle_date_range_emission_record",
                             start = NA,
                             end = NA)
        
        updateTextInput(session,
                        "vehicle_comment_emission_record",
                        value = "")
        
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
        temp_city_list <- world_cities |> 
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
                                distinct(world_cities,
                                         country) |>
                                    arrange(country)),
                    selected = ""
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
                
                actionButton(
                    "add_record_grid_mix_emission_factor",
                    "Add record"
                )
            )            
        } else
            
            if(input$grid_mix_ui_selection_button == "City-level") {
                
                tagList(
                    
                    selectInput(
                        "country_emission_factor",
                        "Select a country*",
                        choices = c("Select a country" = "",
                                    distinct(world_cities,
                                             country) |>
                                        arrange(country)),
                        selected = ""
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
                    
                    actionButton(
                        "add_record_grid_mix_emission_factor",
                        "Add record"
                    )
                )
            }        
        
    })
    
    # initialise an empty table
    ele_grid_mix_table <- reactiveVal({
        
        tibble(
            Country = character(0),
            City = character(0),
            Coal = numeric(0),
            Oil = numeric(0),
            Gas = numeric(0),
            Nuclear = numeric(0),
            Renewables = numeric(0)
        )
        
        
    })
    
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
                Nuclear = input$nuclear_emission_factor,
                Renewables = input$renewables_emission_factor
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
                
                new_table <<- tibble(
                    Country = input$country_emission_factor,
                    City = input$city_emission_factor,
                    Coal = input$coal_mix_emission_factor,
                    Oil = input$oil_mix_emission_factor,
                    Gas = input$gas_mix_emission_factor,
                    Nuclear = input$nuclear_emission_factor,
                    Renewables = input$renewables_emission_factor
                )
            }
        
        
        # pass the new table to the reactive function
        ele_grid_mix_table(new_table)
        
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
        
        
    })
    
    
    
    
    # 2. FERA values for scope 1 and 2 all fuels
    
    # 3. emission factor for scope 1 and 2 all fuels 
    
    #
    
    
    
    
    
    
    
    
    
    # render ------------------------------------------------------------------
    
    # display message to ask user to select an asset type
    
    ## asset tables ####
    
    # building table
    output$asset_table_building <- renderDT({
        datatable(
            asset_table_building() |> 
                select(-c(ID, `Creation Time`)
                ),
            selection = "single")
    })
    
    # vehicle table
    output$asset_table_vehicle <- renderDT({
        datatable(
            asset_table_vehicle() |> 
                select(-c(ID, `Creation Time`)
                ), 
            selection = "single")
    })
    
    
    ## emission record table ####
    
    # building table
    output$building_table_emission_record <- renderDT({
        datatable(building_table_emission_record() |> 
                      select(-c(ID, `Creation Time`)),
                  selection = "single")
    })
    
    # vehicle table 
    output$vehicle_table_emission_record <- renderDT({
        datatable(vehicle_table_emission_record() |> 
                      select(-c(ID, `Creation Time`)),
                  selection = "single")
    })
    
    ## emission factor table ####
    
    # grid mix table
    output$ele_grid_mix_table <- renderDT({
        datatable(ele_grid_mix_table(),
                  selection = "single",
                  options = list(dom = 't'))
    })
    
    ## FERA table
    output$FERA_emission_factor_table <- renderDT({
        datatable(world_cities,
                  selection = "single",
                  options = list(dom = 't'))
    })
    
    ## S1 and S2 table
    output$s1_2_emission_factor_table <- renderDT({
        datatable(world_cities,
                  selection = "single",
                  options = list(dom = 't'))
    })
    
}




# Run app -----------------------------------------------------------------
shinyApp(ui, server)

