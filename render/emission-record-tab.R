
# building ----------------------------------------------------------------


output$selected_building_asset_emission_record <- renderText({
    c("Selected asset:", selected_building_asset())
})


output$emission_record_building <- renderDT({
    datatable(
        emission_record_building(),
        selection = "single"
    )
})


# vehicle -----------------------------------------------------------------


output$emission_record_vehicle <- renderDT({
    datatable(
        emission_record_vehicle(),
        selection = "single"
    )
})