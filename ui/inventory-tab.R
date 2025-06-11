nav_panel(
    title = "Inventory",
    icon = icon("dashboard"),
    
    page_fluid(
        card(
            fg = "lightpink",
            
            layout_column_wrap(
                width = 1 / 2,
                max_height = "100px",
                card_body(
                    selectInput(
                        "country_inventory",
                        label = NULL,
                        choices = c("Select a country" = "")
                    ),
                ),
                card_body(
                    selectInput(
                        "year_inventory",
                        label = NULL,
                        choices = c("Select a reporting year" = "")
                    )
                )
            )
        ),
        card(
            h2("Placeholder for a table")
        )
    )
)
