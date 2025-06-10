# sidebar -----------------------------------------------------------------

## update input fields ####

# country

# building
observe({
    
    building_country_list <- asset_table_building() |> 
        distinct(Country) |> 
        pull(Country)
    
    updateSelectInput(
        session,
        "building_country_consumption_record",
        choices = c(
            "Select a country" = "",
            building_country_list
        )
    )
})

# vehicle
observe({
    
    vehicle_country_list <- asset_table_vehicle() |> 
        distinct(Country) |> 
        pull(Country)
    
    updateSelectInput(
        session,
        "vehicle_country_consumption_record",
        choices = c(
            "Select a country" = "",
            vehicle_country_list
        )
    )
})


# asset names

# building
observeEvent(input$building_country_consumption_record, {
    req(nzchar(input$building_country_consumption_record))
    
    # filtered asset list
    filtered_list <- asset_table_building() |> 
        filter(Country == input$building_country_consumption_record) |> 
        distinct(`Asset Name`) |> 
        pull(`Asset Name`)
    
    updateSelectInput(session,
                      "building_asset_consumption_record",
                      choices = c("Select an asset" = "",
                                  filtered_list))
})


# vehicle
observeEvent(input$vehicle_country_consumption_record, {
    req(nzchar(input$vehicle_country_consumption_record))
    
    # filtered asset list
    filtered_list <- asset_table_vehicle() |> 
        filter(Country == input$vehicle_country_consumption_record) |> 
        distinct(`Asset Name`) |> 
        pull(`Asset Name`)
    
    updateSelectInput(session,
                      "vehicle_asset_consumption_record",
                      choices = c(
                          "Select an asset" = "",
                          filtered_list
                      )
    )
})


# start and end dates

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
                             start = Sys.Date(),
                             end = Sys.Date())
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
                                    "-12-31")),
                         start = Sys.Date(),
                         end = Sys.Date()
    )
    
})


# emission source

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


# vehicle submission data type
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


## renewable energy ui ####

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



# table -------------------------------------------------------------------

# building

# initial tables
building_table_consumption_record <- reactiveVal(NULL)

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

# initialise the database at the start
observe({load_consumption_record_building()})

# Add new record: Building
observeEvent(input$add_building_consumption_record, {
    # create a new record with the submitted values
    new_record <- tibble(
        "Country" = input$building_country_consumption_record,
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
                         start = Sys.Date(),
                         end = Sys.Date())
    
    updateTextInput(session,
                    "building_comment_consumption_record",
                    value = "")
})


# vehicle

# initial table for vehicle consumption record
vehicle_table_consumption_record <- reactiveVal(NULL)


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



# initialise the database
observe({load_consumption_record_vehicle()})

# Add new record: Vehicle
observeEvent(input$add_vehicle_consumption_record, {
    # create a new record with the submitted values
    new_record <- tibble(
        "Country" = input$vehicle_country_consumption_record,
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
    
    updateDateRangeInput(session,
                         "vehicle_date_range_consumption_record",
                         start = Sys.Date(),
                         end = Sys.Date())
    
    updateTextInput(session,
                    "vehicle_comment_consumption_record",
                    value = "")
    
})
