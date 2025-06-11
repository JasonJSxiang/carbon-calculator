nav_panel(
    title = "Asset",
    
    layout_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            page_navbar(
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    
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
                nav_panel(
                    title = "Vehicle",
                    icon = icon("car"),
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
        
        # body content
        card(
            page_navbar(
                # building table
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    DTOutput("asset_table_building")
                ),
                # vehicle table
                nav_panel(
                    title = "Vehicle",
                    icon = icon("car"),
                    DTOutput("asset_table_vehicle")
                )
            )
        )
    )
)