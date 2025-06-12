
# Combine emission and consumption tables ---------------------------------


merged_table <- reactive({
    
    emission_record_building() |> 
        left_join(
            building_table_consumption_record(),
            by = c("Consumption Record Id" = "Id"),
            suffix = c("_emi", "_cons")
        )
})

# Update input fields -----------------------------------------------------

observeEvent(emission_record_building(), {
    
    # get the unique list of countries in the merged_table
    country_list <- merged_table() |>
        distinct(Country) |>
        filter(Country != "NA") |> 
        pull(Country)
    
    updateSelectInput(
        session,
        "country_inventory",
        choices = c("Select a country" = "",
                    country_list)
    )
})

observeEvent(input$country_inventory, {
    
    # get the unique list of reporting year in the merged table
    year_list <- merged_table() |> 
        filter(Country == input$country_inventory) |> 
        distinct(`Reporting Year`) |> 
        filter(`Reporting Year` != "NA") |> 
        pull(`Reporting Year`)
    
    updateSelectInput(
        session,
        "year_inventory",
        choices = c("Select a reporting year" = "",
                    year_list)
    )
})


# tables ------------------------------------------------------------------

monthly_inventory <- reactive({
    req(nzchar(input$country_inventory) & nzchar(input$year_inventory))
    
    df <- merged_table() |> 
        filter(Country == input$country_inventory,
               `Reporting Year` == input$year_inventory) |> 
        mutate(Month = factor(month(`Start Date_emi`))) |> 
        group_by(Country, Month) |> 
        summarise(`LB Emission` = sum(`LB Emission`, na.rm = TRUE))
    
    plot <- ggplot(df, aes(x = Month, y = `LB Emission`)) +
        geom_col(aes(fill = Month)) +  # Fill moved inside aes() for consistency
        geom_line(aes(group = 1), color = "darkred", linewidth = 0.5) +  # Added group=1 for single line
        theme_minimal() +
        theme(legend.position = "none") +
        labs(x = "Month", y = "Location-Based Emission (kgCO2e)") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add space for labels
    
    ggplotly(plot)
})


# map ---------------------------------------------------------------------

map_inventory <- reactive({
    
    # create the df for mapping
    df <- merged_table() |> 
        filter(Country == input$country_inventory,
               `Reporting Year` == input$year_inventory) |> 
        group_by(Country, City) |> 
        summarise(`LB Emission` = sum(`LB Emission`, na.rm = TRUE))
    
    # calculate the total annual emission to be used as label in the map
    total_emission <- paste0(sum(df$`LB Emission`), "kgCO2e")
    
    # extract the city name 
    if(
        nzchar(input$country_inventory) &
        nzchar(input$year_inventory)
    ) {
        city_name <- df |> 
            distinct(City) |> 
            pull(City)
        
        leaflet(world.cities |> 
                    dplyr::filter(
                        country.etc == input$country_inventory,
                        name == city_name
                    )
        ) |> 
            addTiles() |> 
            addScaleBar() |> 
            addSearchOSM() |> 
            addMarkers(lat = ~lat, 
                       lng  = ~long,
                       label = total_emission)
        
    } else {
        leaflet(world.cities) |> 
            addTiles() |> 
            addScaleBar() |> 
            addSearchOSM()
    }
})
