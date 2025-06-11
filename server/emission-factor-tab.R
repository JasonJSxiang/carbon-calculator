
# initialise an empty table -----------------------------------------------


emission_factor_grid <- reactiveVal(NULL)


# create loading function -------------------------------------------------


# create a function that loads database into R (through updating the
# existing NULL reactive value created above)
load_emission_factor_grid <- function() {
    # first load the existing data from the database
    data <- dbGetQuery(pool,
                       "SELECT *
                           FROM emission_factor_grid") |> 
        mutate(`Creation Time` = as_datetime(
            `Creation Time`, 
            tz = tz(Sys.timezone())
        )
        )
    
    # then pass loaded data to the NULL reactive function just created
    emission_factor_grid(data)
}


# initialise database -----------------------------------------------------


# initialise the database (by running the above function when the app starts)
observe({load_emission_factor_grid()})




# adding new record -------------------------------------------------------


observeEvent(input$add_record_emission_factor, {
    # Create a new record
    new_record <- tibble(
        Country = input$country_emission_factor,
        `Emission Factor` = input$ef_emission_factor,
        "Creation Time" = Sys.time()
    )
    
    
    # Check for incomplete record
    if (
        !nzchar(new_record$Country) |
        new_record$`Emission Factor` <= 0
    ) {
        showNotification("Incomplete record!", 
                         type = "warning",
                         closeButton = TRUE)
        return()
    }
    
    
    
    # Check for duplicate 
    existing_data <- emission_factor_grid()
    duplicate <- any(existing_data$Country == new_record$Country)
    
    if (duplicate) {
        showNotification("Record already exists!", 
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
    dbWriteTable(pool, "emission_factor_grid", new_record, append = TRUE)
    showNotification("New emission factor record added",
                     type = "message",
                     closeButton = TRUE)
    
    # refresh the table showing in R by running the loading function again
    load_emission_factor_grid()
    
    # clear inputs
    updateSelectInput(
        session,
        "country_emission_factor",
        selected = ""
    )
    
    updateNumericInput(
        session,
        "ef_emission_factor",
        value = 0
    )
    
    
    
})

