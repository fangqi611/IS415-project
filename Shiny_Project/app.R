#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
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


#####################################

osm_basemap <- tm_basemap(server = "OpenStreetMap.HOT")
imagery_basemap <- tm_basemap(server = "Esri.WorldImagery")

hk_census <- read_excel("data/aspatial/hkcensus.xlsx")

district_18 <- st_read(dsn = "data/geospatial/hk_18Districts/",
                      layer = "HKDistrict18" )

sf_district_18 <- st_transform(district_18, crs = 2326)






#####################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("lumen"),
                
                # -----Navigation Bar
                navbarPage("Project Daylight", fluid=TRUE, windowTitle="Simple Geo-Spatial Analysis using R and Shiny ", selected="overview",
                           
                           
                           tabPanel('Overview', value = "overview", fluid = TRUE, icon = icon("map"),
                                    sidebarLayout(position = "left",fluid = TRUE, 
                                                  sidebarPanel(width=3, fluid = TRUE, 
                                                               conditionalPanel(
                                                                 'input.EDAset === "Bivariate Analysis"',
                                                                 selectInput(inputId="EdaLod",
                                                                             label="EDA Level",
                                                                             choices=varLod,
                                                                             selected="LAD",
                                                                             multiple=FALSE,
                                                                             width="100%"
                                                                 ),
                                                                 selectInput(inputId="EdaMeasureY",
                                                                             label="Select Variable Y",
                                                                             choices=varMeasure1,
                                                                             selected="h_nutrients_calories",
                                                                             multiple=FALSE,
                                                                             width="100%"
                                                                 ),
                                                                 selectInput(inputId="EdaMeasureX",
                                                                             label="Select Variable X",
                                                                             choices=varMeasure1,
                                                                             selected="energy_carb",
                                                                             multiple=FALSE,
                                                                             width="100%"
                                                                 )
                                                              )
                                                              ), 
                                                  mainPanel(width = 9)
                                                
                             
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
