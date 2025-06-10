
# building ----------------------------------------------------------------


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