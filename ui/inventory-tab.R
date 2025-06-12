nav_panel(
    title = "Inventory",
    icon = icon("dashboard"),
    
    page_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            
            selectInput(
                "country_inventory",
                label = NULL,
                choices = c("Select a country" = "")
            ),
            
            
            selectInput(
                "year_inventory",
                label = NULL,
                choices = c("Select a reporting year" = "")
            )
        ),
        card(
            DTOutput("annual_inventory"),
            full_screen = TRUE
        ),
        
        layout_column_wrap(
            width = 1 / 2,
            card(
                card_header("Location(s) on map"),
                leafletOutput("map_inventory"),
                full_screen = TRUE
            ),
            card(
                plotlyOutput("monthly_inventory"),
                full_screen = TRUE
            ),
            
        )
    )
)
