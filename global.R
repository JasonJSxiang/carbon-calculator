## Initialise database ####
con <- dbConnect(SQLite(), "database.sqlite")

# building asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_building
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Type\" TEXT,
          \"Asset Name\" TEXT,
          \"Office Floor Area\" REAL,
          \"Area Unit\" TEXT,
          \"Subleased?\" INTEGER,
          \"Applicable Emission Sources\" TEXT,
          \"Creation Time\" INTEGER)")

# vehicle asset
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS asset_vehicle
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Type\" TEXT,
          \"Asset Name\" TEXT,
          \"Vehicle Type\" TEXT,
          \"Creation Time\" INTEGER)")

# consumption record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_building
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Name\" TEXT,
          \"Reporting Year\" INTEGER,
          \"Fuel Type\" TEXT,
          Consumption REAL,
          Unit TEXT,
          \"Renewable?\" TEXT,
          \"Renewable Energy Consumption (kWh)\" REAL,
          \"Renewable Energy Type\" TEXT,
          \"Start Date\" INTEGER,
          \"End Date\" INTEGER,
          \"Additional Comment\" TEXT,
          \"Creation Time\" INTEGER)")

# consumption record: vehicle
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS consumption_record_vehicle
          (
          Id INTEGER PRIMARY KEY,
          Country TEXT,
          City TEXT,
          \"Asset Name\" TEXT,
          \"Reporting Year\" INTEGER,
          \"Fuel Type\" TEXT,
          \"Data Type\" TEXT,
          Amount REAL,
          Unit TEXT,
          \"Start Date\" INTEGER,
          \"End Date\" INTEGER,
          \"Additional Comment\" TEXT,
          \"Creation Time\" INTEGER)")

# emission factor 
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_factor_grid
    (
    Id INTEGER PRIMARY KEY,
    Country TEXT,
    \"Emission Factor\" REAL,
    \"Creation Time\" INTEGER
    )"
)

# emission record: building
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_building
    (
    Id INTEGER PRIMARY KEY,
    \"Consumption Record Id\",
    \"Asset Name\" TEXT,
    \"Fuel Type\" TEXT,
    \"LB Emission\" REAL,
    \"MB Emission\" REAL,
    \"Start Date\" INTEGER,
    \"End Date\" INTEGER,
    \"Creation Time\" INTEGER)")

# emission record: vehicle
dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS emission_record_vehicle
    (
    Id INTEGER PRIMARY KEY,
    \"Asset Name\" TEXT,
    \"Fuel Type\" TEXT,
    \"Data Type\" TEXT,
    \"LB Emission\" REAL,
    \"MB Emission\" REAL,
    \"Start Date\" INTEGER,
    \"End Date\" INTEGER,
    \"Creation Time\" INTEGER)")

dbDisconnect(con)

## Globals static ####
#(code only run once when the app starts)

# fuel list for building
fuel_building <- c("Electricity",
                   "Heating",
                   "Steam",
                   "Cooling",
                   "Diesel",
                   "Petrol",
                   "Kerosene")

# vehicle type list for vehicle
vehicle_type <- c("Petrol",
                  "Diesel",
                  "Hybrid",
                  "Electric")

# building fuel unit
building_fuel_unit <- c("kWh", "BTU")

# reporting year
reporting_year <- c(2025:2015)

# renewable energy type
renewable_energy_type <- c("Solar", "Wind", "Hydro", "Biomass")

# country and city list (from maps package)
country_list <- world.cities |> 
    rename("country" = "country.etc") |> # rename the country col
    mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
    dplyr::distinct(country, .keep_all = TRUE) |>  # keep unique country
    dplyr::arrange(country) |> 
    pull(country)

city_df <- world.cities |> 
    rename("country" = "country.etc") |> # rename the country col
    mutate(name = str_replace(name, "^'", "")) |> # remove the leading ' in the col
    dplyr::distinct(name, .keep_all = TRUE) |>  # keep unique country
    dplyr::arrange(name)