
# grid mix ----------------------------------------------------------------


output$emission_factor_grid <- renderDT({
    datatable(emission_factor_grid(),
              selection = "single")
})

