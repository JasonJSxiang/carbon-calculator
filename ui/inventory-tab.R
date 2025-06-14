nav_panel(
    title = "Inventory",
    icon = icon("dashboard"),
    
    page_sidebar(
        sidebar = sidebar(
            title = "Inputs",
            
            selectInput(
                "country_inventory",
                label = "Select a country",
                choices = c("Select a country" = "")
            ),
            
            selectInput(
                "year_inventory",
                label = "Select a reporting year",
                choices = c("Select a reporting year" = "")
            ),
            
            selectInput(
                "asset_inventory",
                label = "Select an asset",
                choices = c("Select an asset" = "")
            )
        ),
        card(
            DTOutput("annual_inventory"),
            full_screen = TRUE
        ),
        
        layout_column_wrap(
            width = 1 / 2,
            card(
                id = "map_card_inventory",
                card_header("Location(s) on map"),
                leafletOutput("map_inventory", height = "100%"),
                full_screen = TRUE
            ),
            card(
                plotlyOutput("monthly_inventory"),
                full_screen = TRUE
            ),
            
        )
    )
)
