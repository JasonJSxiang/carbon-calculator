nav_panel(
    title = "Home",
    icon = icon("home"),
    
    layout_column_wrap(
        width = 1 / 4,
        card(
            fill = FALSE,
            actionButton(
                "clear_asset",
                "Clear Asset Table"
            ),
            
            actionButton(
                "clear_consumption_record",
                "Clear Consumption Record Table"
            ),
            
            actionButton(
                "clear_emission_factor_grid",
                "Clear Emission Factor Table"
            ),
            
            actionButton(
                "clear_emission_record_table",
                "Clear Emission Record Table"
            ),
            
            actionButton(
                "clear_all",
                "CLEAR ALL"
            )
        )
    )
)