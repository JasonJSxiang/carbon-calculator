# clear asset table
observeEvent(input$clear_asset, {
    dbExecute(
        pool, "DELETE FROM asset_building"
    )
    
    dbExecute(
        pool, "DELETE FROM asset_vehicle"
    )
    
    dbExecute(pool, "VACUUM")  # 清理空间
    
    load_asset_building()
    load_asset_vehicle()
    
    showNotification("Asset tables cleared!",
                     type = "message")
})

# clear consumption record table
observeEvent(input$clear_consumption_record, {
    dbExecute(
        pool, "DELETE FROM consumption_record_building"
    )
    
    dbExecute(
        pool, "DELETE FROM consumption_record_vehicle"
    )
    
    dbExecute(pool, "VACUUM")  # 清理空间
    
    load_consumption_record_vehicle()
    load_consumption_record_building()
    
    showNotification("Consumption tables cleared!",
                     type = "message")
})

# clear grid mix table
observeEvent(input$clear_grid_mix_emission_factor, {
    dbExecute(
        pool, "DELETE FROM ele_grid_mix_table"
    )
    dbExecute(pool, "VACUUM")  # 清理空间
    
    load_grid_mix_emission_factor()
    
    showNotification("Grid Mix table cleared!",
                     type = "message")
})

# clear ALL
observeEvent(input$clear_all, {
    # obtain all the table names
    names <- dbListTables(pool)
    
    # clear all the tables through loop
    for (i in names) {
        dbExecute(
            pool,
            sprintf("DELETE FROM %s", i)
        )
    }
    
    load_all()
    
    dbExecute(pool, "VACUUM")
    
    showNotification("All tables cleared!",
                     type = "message")
})
