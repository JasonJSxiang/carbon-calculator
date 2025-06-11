nav_panel(
    title = "Emission Factor",
    
    page_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            
            selectInput(
                "country_emission_factor",
                "Select a country*",
                choices = c("Select a country" = "",
                            country_list)
            ),
            numericInput(
                "ef_emission_factor",
                "Emission Factor (g/KWh)*",
                value = 0,
                min = 0
            ),
            actionButton(
                "add_record_emission_factor",
                "Add record"
            )
        ),
        
        card(
            DTOutput("emission_factor_grid")
        )
        
    )
)