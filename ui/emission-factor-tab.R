nav_panel(
    title = "Emission Factor",
    icon = icon("clipboard"),
    
    page_sidebar(
        sidebar = sidebar(
            page_navbar(
                nav_panel(
                    title = "Grid Mix",
                    
                    selectInput(
                        "country_emission_factor",
                        "Select a country*",
                        choices = c("Select a country" = "",
                                    country_list)
                    ),
                    
                    numericInput(
                        "starting_year_emission_factor",
                        "Enter the starting year",
                        value = 2025
                    ),
                    
                    numericInput(
                        "ending_year_emission_factor",
                        "Enter the ending year",
                        value = NA
                    ),
                    
                    textInput(
                        "remark_emission_factor",
                        "Remark",
                        value = NA
                    ),
                    
                    actionButton(
                        "add_record_emission_factor",
                        "Add record"
                    )
                )
            )
        ),
        
        card(
            DTOutput("ele_grid_mix_table")
        )
    )
)