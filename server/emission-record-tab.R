# building table

# update the select input field to match the list of consumption record Id
observeEvent(building_table_consumption_record(), {
    req(
        nrow(
            building_table_consumption_record()
        ) > 0 # make sure nrow is greater than 0
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
    new_record <- building_table_consumption_record() |> 
        filter(Id == input$id_emission_record_building)
    
    # extract the country of that consumption record
    new_record_country <- new_record |> 
        pull(Country)
    
    # get a country list in the ele grid mix table
    country_list <- ele_grid_mix_table() |> 
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
    
    # get the grid mix of the selected country
    selected_country_grid_mix <- ele_grid_mix_table() |> 
        filter(Country == new_record_country)
    
    # format the new_record to fit the emission record table in the DB
    formatted_new_record <- new_record |> 
        select(Id, `Asset Name`, `Fuel Type`) |> 
        mutate(`LB Emission` = LBEmission) |> 
        rename("Consumption Record Id" = "Id")
    
    # update the reactive value with new record
    dbWriteTable(pool,
                 "emission_record_building",
                 formatted_new_record, append = TRUE)
    
    # refresh the table
    load_emission_record_building()
    
    showNotification("New building emission record added", 
                     type = "message",
                     closeButton = TRUE)
    
})


