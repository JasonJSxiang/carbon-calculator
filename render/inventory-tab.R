
# monthly -----------------------------------------------------------------

output$monthly_inventory <- renderPlotly({
    monthly_inventory()
})


# annual ------------------------------------------------------------------

output$annual_inventory <- renderDT({
    datatable(
        merged_table(),
        selection = "single"
    )
})



# map ---------------------------------------------------------------------

output$map_inventory <- renderLeaflet({
    map_inventory()
})
