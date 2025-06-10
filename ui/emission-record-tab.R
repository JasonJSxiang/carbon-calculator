nav_panel(
    title = "Emission",
    icon = icon("smog"),
    
    page_sidebar(
        sidebar = sidebar(
            page_navbar(
                # building
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    selectInput(
                        "id_emission_record_building",
                        "Select an Id",
                        choices = 
                            c("Select an Id" = "")
                    ),
                    
                    actionButton(
                        "add_emission_record_building",
                        "Calculate"
                    )
                ),
                
                # vehicle
                nav_panel(
                    title = "Vehicle",
                    icon = icon("car"),
                    selectInput(
                        "id_session_record_vehicle",
                        "Select an Id",
                        choices =
                            c("Select an Id" = "")
                    ),
                    
                    actionButton(
                        "add_emission_record_vehicle",
                        "Calculate"
                    )
                )
            )
        ),
        
        card(
            page_navbar(
                # building
                nav_panel(
                    title = "Building",
                    icon = icon("building"),
                    DTOutput("emission_record_building")
                ),
                # vehicle
                nav_panel(
                    title = "Vehicle",
                    icon = icon("car"),
                    DTOutput("emission_record_vehicle")
                )
            )
        )
    )
)