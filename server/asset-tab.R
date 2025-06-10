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

# add new record 

# building 

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

# vehicle 

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
