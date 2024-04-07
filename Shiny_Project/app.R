#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(tmap)
library(arrow)
library(lubridate)
library(tidyverse)
library(sp)
library(raster)
library(spatstat)
library(classInt)
library(viridis)
library(spNetwork)
library(spatstat)
library(ggplot2)
library(ggmap)
library(tidymodels)
library(glmnet)
library(readxl)
library(shinydashboard)

#####################################

osm_basemap <- tm_basemap(server = "OpenStreetMap.HOT")
imagery_basemap <- tm_basemap(server = "Esri.WorldImagery")

hk_census <- read_excel("data/aspatial/hkcensus.xlsx")

district_18 <- st_read(dsn = "data/geospatial/hk_18Districts/",
                      layer = "HKDistrict18" )

sf_district_18 <- st_transform(district_18, crs = 2326)






#####################################

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "HK Recycling Stations Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("KDE", tabName = "kde", icon = icon("map")),
      menuItem("NKDE", tabName = "nkde", icon = icon("map-marked"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab content
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Overview Map", status = "primary", solidHeader = TRUE,
                    tmapOutput("d18_map", height = "600px")) # Adjust height as needed
              )),
      # KDE tab content
      tabItem(tabName = "kde",
              fluidRow(
                box(title = "Kernel Density Estimation", status = "warning", solidHeader = TRUE
                    # Placeholder for KDE output. Replace with your actual output UI element.
                   
              ))),
      # NKDE tab content
      tabItem(tabName = "nkde",
              fluidRow(
                box(title = "Normalized Kernel Density Estimation", status = "info", solidHeader = TRUE
                    # Placeholder for NKDE output. Replace with your actual output UI element.
                    
              ))
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$d18_map <- renderTmap({
      
      map <- tm_shape(sf_district_18) +
        tm_fill(col = "ENAME", title = "District", legend.show = FALSE) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_text("ID", size = 0.5, col = "black") +
        tm_layout(frame = FALSE)+ 
        tmap_options(check.and.fix = TRUE)
      
      map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
