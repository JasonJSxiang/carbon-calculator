
# building ----------------------------------------------------------------


# update the select input field to update the choices
# with a list of existing consumption record Id
observeEvent(building_table_consumption_record(), {
    # make sure that there are records in the building consumption record table
    req(
        nrow(building_table_consumption_record()) > 0 
    )
    
    # get the Id list of building consumption records
    id_list <- building_table_consumption_record() |>
        pull(Id)
    
    updateSelectInput(
        session,
        "id_emission_record_building",
        choices = c(
            "Select an Id" = "",
            id_list
        )
    )
})

# observe the selected Id to display the asset name of the selected Id
selected_building_asset <- reactiveVal(NULL)

observeEvent(input$id_emission_record_building, {
    # get the asset name of the selected Id
    asset_name <- building_table_consumption_record() |> 
        filter(Id == input$id_emission_record_building) |> 
        pull(`Asset Name`)
    
    selected_building_asset(asset_name)
})

# initial table
emission_record_building <- reactiveVal(NULL)

# function to cache database
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
observe({load_emission_record_building()})

# workflow to calculate emission for the selected consumption record
observeEvent(input$add_emission_record_building, {
    req(nzchar(input$id_emission_record_building))
    
    # extract the row with the selected Id
    new_record <<- building_table_consumption_record() |> 
        filter(Id == input$id_emission_record_building)
    
    # extract the country of that consumption record
    new_record_country <- new_record |> 
        pull(Country)
    
    # get a country list in the ele grid mix table
    country_list <- emission_factor_grid() |> 
        dplyr::distinct(Country) |> 
        pull(Country)
    
    # check if the consumption record's country is in the country list
    if(!(new_record_country %in% country_list)) {
        
        showNotification(
            "No Grid Mix Info of the Selected Country",
            type = "warning"
        )
        
        return()
    } else {}
    
    # get the grid mix ef of the selected country
    selected_country_grid_ef <- emission_factor_grid() |> 
        filter(Country == new_record_country) |> 
        pull(`Emission Factor`)
    
    # get the consumption of the selected record
    selected_record_consumption <- new_record$Consumption
    
    # calculate the LB emission
    LBEmission <- selected_country_grid_ef*selected_record_consumption / 1000
    
    # format the new_record to fit the emission record table in the DB
    formatted_new_record <- new_record |> 
        select(Id, `Asset Name`, `Fuel Type`) |> 
        mutate(`LB Emission` = LBEmission,
               `Start Date` = new_record$`Start Date`,
               `End Date` = new_record$`End Date`,
               `Creation Time` = Sys.time()) |> 
        rename("Consumption Record Id" = "Id")
    
    # check if a record with the same consumption record Id already exists
    if(formatted_new_record$`Consumption Record Id` %in% 
       emission_record_building()$`Consumption Record Id`) {
        showNotification(
            "Consumption record already exists",
            type = "warning"
        )
        
        return()
        
    } else {}
    
    # update the reactive value with new record
    dbWriteTable(pool,
                 "emission_record_building",
                 formatted_new_record, append = TRUE)
    
    # refresh the table
    load_emission_record_building()
    
    # clear the input fields
    updateSelectInput(
        session,
        "id_emission_record_building",
        selected = ""
    )
    
    showNotification("New building emission record added", 
                     type = "message",
                     closeButton = TRUE)
    
})


