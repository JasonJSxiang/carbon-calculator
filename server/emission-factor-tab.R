# initialise an empty table
ele_grid_mix_table <- reactiveVal(NULL)

# create function that loads database
load_grid_mix_emission_factor <- function() {
    # first load the existing DB into data object
    data <- dbGetQuery(pool, "SELECT *
                           FROM ele_grid_mix_table")
    
    # then pass the data object to the reactive function defined above
    ele_grid_mix_table(data)
}



# initialise the database
observe({
    load_grid_mix_emission_factor()
})

# add new country-specific grid mix table record workflow
observeEvent(input$add_record_emission_factor, {
    
    # check country input is selected
    if(
        !nzchar(input$country_emission_factor)
    ) {
        showNotification("Incomeplete record!",
                         type = "warning")
        
        return()
    }
    
})

