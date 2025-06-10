
# grid mix table ----------------------------------------------------------


output$ele_grid_mix_table <- renderDT({
    datatable(ele_grid_mix_table(),
              selection = "single",
              options = list(dom = 't'))
})


# default grid mix emission factor ----------------------------------------


output$default_grid_mix_ef_table <- renderDT({
    datatable(default_grid_mix_ef_table(),
              selection = "single",
              options = list(dom = "t")
    )
})

