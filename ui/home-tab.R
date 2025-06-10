nav_panel(
    title = "Home",
    icon = icon("home"),
    
    layout_column_wrap(
        width = 1,
        actionButton(
            "clear_asset",
            "Clear Asset Table"
        ),
        
        actionButton(
            "clear_consumption_record",
            "Clear Consumption Record Table"
        ),
        
        actionButton(
            "clear_grid_mix_emission_factor",
            "Clear Grid Mix Table"
        ),
        
        actionButton(
            "clear_all",
            "CLEAR ALL"
        )
    )
)