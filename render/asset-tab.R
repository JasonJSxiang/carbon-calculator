# building table ----------------------------------------------------------
output$asset_table_building <- renderDT({
    datatable(
        asset_table_building(),
        selection = "single")
})


# vehicle table -----------------------------------------------------------
output$asset_table_vehicle <- renderDT({
    datatable(
        asset_table_vehicle(), 
        selection = "single")
})

