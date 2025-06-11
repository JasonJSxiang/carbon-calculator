## DB connection setting ####
pool <- dbPool(
    SQLite(),
    dbname = "database.sqlite"
)

# disconnect the pool when ending the session
onStop(function() {
    poolClose(pool)
})


## Globals (dynamic) ####

# function to load all the tables
load_all <- function() {
    load_asset_building()
    load_asset_vehicle()
    load_consumption_record_building()
    load_consumption_record_vehicle()
    load_emission_factor_grid()
    load_emission_record_building()
}    