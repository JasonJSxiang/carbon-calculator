library(DBI)
library(RSQLite)
library(pool)
library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(bslib)
library(plotly)
library(leaflet)
library(leaflet.extras)


# 1. Preliminary setting --------------------------------------------------
source("global.R")

# 2. ui -------------------------------------------------------------------
ui <- page_navbar(
    theme = bs_theme(bootswatch = "minty"),
    
    # static dashboard header
    title = "Carbon Calculator by Yixiang Zhang",
    nav_item(input_dark_mode()),
    nav_spacer(),
    
    # home tab 
    source("ui/home-tab.R", local = TRUE)$value,
    # asset tab
    source("ui/asset-tab.R", local = TRUE)$value,
    # consumption record tab
    source("ui/consumption-record-tab.R", local = TRUE)$value,
    # emission factor tab
    source("ui/emission-factor-tab.R", local = TRUE)$value,
    # emission record tab
    source("ui/emission-record-tab.R", local = TRUE)$value,
    # inventory tab
    source("ui/inventory-tab.R", local = TRUE)$value
)


# 3. server ---------------------------------------------------------------
server <- function(input, output, session) {
    # server config
    source("server/config.R", local = TRUE)
    # home tab
    source("server/home-tab.R", local = TRUE)
    # asset tab
    source("server/asset-tab.R", local = TRUE)
    # consumption record tab
    source("server/consumption-record-tab.R", local = TRUE)
    # emission factor tab
    source("server/emission-factor-tab.R", local = TRUE)
    # emission record tab
    source("server/emission-record-tab.R", local = TRUE)
    # inventory tab
    source("server/inventory-tab.R", local = TRUE)
    
    
    # 4. render ---------------------------------------------------------------
    
    # asset tab
    source("render/asset-tab.R", local = TRUE)
    # consumption record tab
    source("render/consumption-record-tab.R", local = TRUE)
    # emission factor tab
    source("render/emission-factor-tab.R", local = TRUE)
    # emission record tab
    source("render/emission-record-tab.R", local = TRUE)
    # inventory tab
    source("render/inventory-tab.R", local = TRUE)
}



# 5. Run app ####
shinyApp(ui, server)

