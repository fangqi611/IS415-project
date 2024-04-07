#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


library(shiny)
library(sf)
library(tmap)
library(bslib)
library(tidyverse)
library(leaflet)
library(spatstat)

# Codes to plot out the data
## EDA
cp <- read_csv("data/aspatial/hkrecyclepoints.csv") 

cp_sf <- st_as_sf(cp, 
                  coords = c("lgt","lat"), 
                  crs = 4326) %>%
  st_transform(crs=2326)

cp_sf <- cp_sf %>%
  st_union()

## KDE
cp_sf <- cp_sf %>% st_transform(crs = 2326)

cp_ppp <- as.ppp(cp_sf)

#hk_cp_ppp = cp_ppp[district_18_owin]

#hk_ppp.km <- rescale(hk_cp_ppp, 1000, "km")

# Define UI for application
ui <- fluidPage(
  
  # App title ----
  headerPanel("Hong Kong Recycling Points Geographical Analysis"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("geographical_analysis", "Geographical Analysis:", 
                c("Exploratory Data Analysis" = "EDA",
                  "Kernal Density Estimation" = "KDE",
                  "Emerging Hotspot Analysis" = "EHSA",
                  "Network Constrained Spatial Point Pattern Analysis" = "NKDE")),
    
    # Input: Checkbox for whether outliers should be included ----
    checkboxInput("outliers", "Show outliers", TRUE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Plot
    plotOutput("plot")
  )
)


# Define server logic
server <- function(input, output) {
  
  # Generate outputs based on user selection
  observeEvent(input$geographical_analysis, {
    analysis <- input$geographical_analysis
    
    # Example: Logic to generate output for EDA
    if (analysis == "EDA") {
      
      edaPlot <- tm_shape(cp_sf) +
        tm_dots() +
        tm_layout(main.title = "Exploratory Data Analysis")
      
      output$plot <- renderLeaflet({
        tmap_leaflet(edaPlot)
      })
    }
    
    # Example: Logic to generate output for KDE
    else if (analysis == "KDE") {
      output$plot <- renderPlot({
        #Code for KDE plot generation goes here
        par(mfrow=c(2,2))
        plot(density(hk_ppp.km, 
                     sigma=bw.ppl, 
                     edge=TRUE, 
                     kernel="gaussian"), 
             main="Gaussian")
        plot(density(hk_ppp.km, 
                     sigma=bw.ppl, 
                     edge=TRUE, 
                     kernel="epanechnikov"), 
             main="Epanechnikov")
        plot(density(hk_ppp.km, 
                     sigma=bw.ppl, 
                     edge=TRUE, 
                     kernel="quartic"), 
             main="Quartic")
        plot(density(hk_ppp.km, 
                     sigma=bw.ppl, 
                     edge=TRUE, 
                     kernel="disc"), 
             main="Disc")
      })
    }
    
    # Example: Logic to generate output for EHSA
    else if (analysis == "EHSA") {
      output$plot <- renderPlot({
        # Your code for EHSA plot generation goes here
        plot(rnorm(100), rnorm(100), main = "EHSA Plot")
      })
    }
    
    # Example: Logic to generate output for NKDE
    else if (analysis == "NKDE") {
      output$plot <- renderPlot({
        # Your code for NKDE plot generation goes here
        plot(rnorm(100), rnorm(100), main = "NKDE Plot")
      })
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
