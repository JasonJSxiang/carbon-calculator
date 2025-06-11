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
observeEvent(input$clear_emission_factor_grid, {
    dbExecute(
        pool, "DELETE FROM emission_factor_grid"
    )
    dbExecute(pool, "VACUUM")  # 清理空间
    
    load_emission_factor_grid()
    
    showNotification("Emission factor table cleared!",
                     type = "message")
})

# clear emission record table
observeEvent(input$clear_emission_record_table, {
    dbExecute(
        pool, "DELETE FROM emission_record_building"
    )
    
    dbExecute(
        pool, "DELETE FROM emission_record_vehicle"
    )
    
    dbExecute(pool, "VACUUM")
    
    load_emission_record_building()
    # load_emission_record_vehicle()
    
    showNotification("Emission record table cleared!",
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
