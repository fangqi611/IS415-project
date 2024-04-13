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
library(DT)
library(stringr)


#####################################

osm_basemap <- tm_basemap(server = "OpenStreetMap.HOT")
imagery_basemap <- tm_basemap(server = "Esri.WorldImagery")

hk_census <- read_excel("data/aspatial/hkcensus.xlsx")

district_18 <- st_read(dsn = "data/geospatial/hk_18Districts/",
                      layer = "HKDistrict18" )

sf_district_18 <- st_transform(district_18, crs = 2326)

cp <- read_csv("data/aspatial/hkrecyclepoints.csv")
cp_sf <- st_as_sf(cp, 
                  coords = c("lgt","lat"), 
                  crs = 4326) %>% 
  st_transform(crs= 2326)

cp_sf_1 <- cp_sf %>%
  mutate(district_id = toupper(str_replace_all(district_id, "_", " ")))

cp_sf_1_expanded <- cp_sf_1 %>%
  separate_rows(waste_type, sep = ",") %>%
  mutate(waste_type = trimws(waste_type))


recycling_bins <- subset(cp_sf_1, legend == "Recycling Bins at Public Place")
recycling_spots <- subset(cp_sf_1, legend == "Recycling Spots")
private_collection_points <- subset(cp_sf_1, legend == "Private Collection Points (e.g. housing estates, shopping centres)")
ngo_collection_points <- subset(cp_sf_1, legend == "NGO Collection Points")
recycling_stations <- subset(cp_sf_1, legend == "Recycling Stations/Recycling Stores")
street_corner_recycling_shops <- subset(cp_sf_1, legend == "Street Corner Recycling Shops")
smart_bins <- subset(cp_sf_1, legend == "Smart Bin")


recycling_spots_cp <- st_join(sf_district_18, recycling_spots)
ngo_cp <- st_join(sf_district_18, ngo_collection_points)
pcp_joined_data <- st_join(sf_district_18, private_collection_points)
recycling_bins_cp <- st_join(sf_district_18, recycling_bins)
recycling_stations_cp <- st_join(sf_district_18, recycling_stations)
street_corner_cp <- st_join(sf_district_18, street_corner_recycling_shops)
smart_bins_cp <- st_join(sf_district_18, smart_bins)

private_collection_points_by_district <- pcp_joined_data %>%
  group_by(ENAME) %>%
  summarize(total_pcp = n())

ngo_cp_by_district <- ngo_cp %>%
  group_by(ENAME) %>%
  summarize(total_ngo_cp = n())

recycling_spots_by_district <- recycling_spots_cp %>%
  group_by(ENAME) %>%
  summarize(total_recycling_spots = n())

recycling_bins_by_district <- recycling_bins_cp %>%
  group_by(ENAME) %>%
  summarize(total_recycling_bins = n())

recycling_stations_by_district <- recycling_stations_cp %>%
  group_by(ENAME) %>%
  summarize(total_recycling_stations = n())

street_corner_shops_by_district <- street_corner_cp %>%
  group_by(ENAME) %>%
  summarize(total_street_corner = n())

smart_bins_by_district <- smart_bins_cp %>%
  group_by(ENAME) %>%
  summarize(total_smart_bins = n())


roads_in_hk <- read_rds("data/rds/sf_roads_in_hk.rds")

private_cp_shatin <- read_rds("data/rds/private_cp_shatin.rds")
public_cp_shatin <- read_rds("data/rds/public_cp_shatin.rds")
cp_shatin <- read_rds("data/rds/cp_shatin.rds")




######################################
#Drop-down Selection 
districts <- c(
  "All",
  "WONG TAI SIN", 
  "KOWLOON CITY",
  "KWUN TONG", 
  "SAI KUNG", 
  "NORTH", 
  "CENTRAL & WESTERN" = "CENTRAL WESTERN", 
  "WAN CHAI", 
  "EASTERN", 
  "TUEN MUN", 
  "YUEN LONG", 
  "SOUTHERN", 
  "ISLANDS", 
  "SHAM SHUI PO", 
  "YAU TSIM MONG", 
  "KWAI TSING",
  "TUSEN WAN", 
  "TAI PO", 
  "SHA TIN"
)

districts_2 <- c(
  "WAN CHAI", 
  "TUEN MUN", 
  "SHAM SHUI PO", 
  "YAU TSIM MONG", 
  "TUSEN WAN", 
  "SHA TIN"
)

types_of_cp <- c(
  "Recycling Bins at Public Place",
  "Recycling Spots",
  "Private Collection Points (e.g. housing estates, shopping centres)",
  "NGO Collection Points",
  "Recycling Stations/ Recycling Stores",
  "Street Corner Recycling Shops",
  "Smart Bin"
)

types_of_waste <- c(
  "Metals",
  "Other Plastic",
  "Paper",
  "Glass Bottles",                          
  "Fluorescent Lamp",                          
  "Rechargeable Batteries",                   
  "Regulated Electrical Equipment",            
  "Small Electrical and Electronic Equipment",
  "Tetra Pak",                                 
  "Clothes",                                  
  "Other Description",                         
  "Barbeque Fork",                            
  "Printer Cartridges",                        
  "Computers",                                
  "Others",                                    
  "Plastic Bottle"
  
)




#####################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("lumen"),
                
                # -----Navigation Bar
                navbarPage("Project Daylight", 
                           
                           tabPanel("Home", 
                                    h2(div(style = "text-align: center;", "Welcome to Project Daylight!")),
                                    div(style = "text-align: center;", imageOutput("logo")), 
                                    br(),
                                    h3("Project Description:"), 
                                    br(), 
                                    h3("User Guide of Shiny:")
                                    ), 
                           
                           tabPanel("Overview", icon = icon("map"), 
                                    fluidRow(
                                      sidebarLayout(
                                        sidebarPanel (
                                          selectInput("District", h3("Select a District:"), 
                                                      choices = districts), 
                                          checkboxGroupInput("cp_type", h3("Select type of recycling facilities (optional)"), 
                                                      choices = types_of_cp),
                                          checkboxGroupInput("waste_type", h3("Select waste type (optional)"), 
                                                             choices = types_of_waste)
                                        ), 
                                        mainPanel(
                                          h2(p("Overview of Recycling points in Hong Kong")),
                                          tmapOutput("cp_map"), 
                                          uiOutput("noDataMessage"),
                                          br(),
                                          h3("List of Recycling Points in Hong Kong"),
                                          DTOutput("cp_data_table")
                                        )
                                       )), 
                                      
                                      ), 
                           tabPanel("NKDE", icon = icon("globe"), 
                                    h2(p("Network Spatial Point Analysis")), 
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("District_1", h3("Select a District:"), 
                                                    choices = districts_2),
                                        actionButton("go", "Run Analysis"),
                                        htmlOutput("loadingMessage")
                                      ),
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("NetSPPA Kernal Density Estimation",
                                                   tmapOutput("district_nkde_map")
                                                 )
                                      )
                                    )
                                  
                                    )
                                      
                           )
                           
                )    
                        
                           
      )
                



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  tmap_mode("view")
    
    output$d18_map <- renderTmap({
      
      map <- tm_shape(sf_district_18) +
        tm_fill(col = "ENAME", title = "District", legend.show = FALSE) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_text("ID", size = 0.5, col = "black") +
        tm_layout(frame = FALSE)+ 
        tmap_options(check.and.fix = TRUE)
      
      map
    })
    
    output$logo <- renderImage({
      list(src = "images/Logo.png",
          style = "max-width: 100%; height: auto; display: block; margin-left: auto; 
          margin-right: auto; margin-top: 0px; margin-bottom: 0px;")
    }, deleteFile = FALSE)
    
    
    
    output$shatin_if <- renderImage({
      list(src = "images/shatin_if.png",
           style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
    output$wanchai_if <- renderImage({
      list(src = "images/wanchai_if.png",
           style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
    },deleteFile = FALSE)
    
    
    output$ytm_if <- renderImage({
      list(src = "images/ytm_if.png",
           style = "max-width: 100%; height: auto; display: block; margin-left: auto; 
          margin-right: auto; margin-top: 0px; margin-bottom: 0px;")
    }, deleteFile = FALSE)
  
############### Overiview Page ###############
    
### All the recycling points in hk map   
   
     filteredData <- reactive({
      
      cp_selected <- cp_sf_1_expanded 
      
      # Filter by district if a specific district is selected
      if (input$District != "All") {
        cp_selected <- cp_selected %>%
          filter(district_id == input$District)
      }
      
      # Filter by type of collection points
      if (length(input$cp_type) > 0) {
        cp_selected <- cp_selected %>%
          filter(legend %in% input$cp_type)
      }
      
      # Filter by waste type if specified
      if (length(input$waste_type) > 0) {
        cp_selected <- cp_selected %>%
          filter(waste_type %in% input$waste_type)
      }
      
      cp_selected
    })
    
    
    # Render the map based on the filtered dataset
    output$cp_map <- renderTmap({
     
      # Access the filtered dataset
      map_data <- filteredData()
      
      if (nrow(map_data) == 0 && input$District != "All") {
        map_data <- cp_sf_1_expanded %>% filter(district_id == input$District)
      }
      
      #bbox <- if (nrow(map_data) > 0) st_bbox(map_data) else st_bbox(sf_district_18)
      
      
      # Create the map with tmap, adjusting as necessary for your data
      cp_map <- tm_basemap("OpenStreetMap") +
        tm_shape(sf_district_18) + # Correct way to add sf_district_18 geometries
        tm_borders() + # Add borders for sf_district_18 without specifying color or width
        tm_shape(map_data) +
        tm_dots() +
        tm_layout(title = "Recycling Bin Locations") +
        #tm_view(bbox = bbox) + # Use the calculated bounding box to set the map view
        tmap_options(check.and.fix = TRUE)
      
      # Return the tmap object for rendering
      cp_map
    })
    
    output$noDataMessage <- renderUI({
      # Access the filtered dataset
      map_data <- filteredData()
      
      # Check if the dataset is empty and the district is not 'All'
      if (!any(is.na(map_data)) && input$District != "All") {
        # Return a UI element with the message
        return(HTML("<div style='color: red; padding: 10px;'><strong>No data available for the selected criteria in the specified district.</strong></div>"))
      }
    })
    
####### Data Table for all recycling points inhk     
    
    filteredDT <- reactive({
      
      cp_selected_1 <- cp_sf_1
      
      # Filter by district if a specific district is selected
      if (input$District != "All") {
        cp_selected_1 <- cp_selected_1 %>%
          filter(district_id == input$District)
      }
      
      # Filter by type of collection points
      if (length(input$cp_type) > 0) {
        cp_selected_1 <- cp_selected_1 %>%
          filter(legend %in% input$cp_type)
      }
      
      if (length(input$waste_type) > 0) {
        # Create a regex pattern from the user's selection, escaping special characters
        pattern <- paste(lapply(input$waste_type, regex), collapse = "|")
        
        # Filter rows where waste_type contains any of the selected waste types
        cp_selected_1 <- cp_selected_1 %>%
          filter(str_detect(waste_type, pattern))
      }
      
      cp_selected_1 <- cp_selected_1 
    })
    
    output$cp_data_table <- renderDT({
      datatable(filteredDT(), options = list(pageLength = 10))
    })
    
################# NKDE Plot Graph ###############
    
    
    density_data <- reactive({
      
      selected_district <- input$District_1
      
      # Initial checks for input validity
      if(is.null(selected_district) || selected_district == ""){
        stop("Selected district is null or empty")
      }
      
      district_recycling_points <- cp_sf_1 %>%
        filter(district_id == selected_district & 
                 (legend == "Private Collection Points (e.g. housing estates, shopping centres)" |
                    legend == "Recycling Bins at Public Place"))
      
      if(nrow(district_recycling_points) == 0) {
        stop("No district recycling points found for the selected district.")
      }
      
      district_selected <- sf_district_18 %>%
        filter(ENAME == selected_district)
      
      if(nrow(district_selected) == 0) {
        stop("Selected district not found in sf_district_18.")
      }
      
      roads_in_district <- st_intersection(roads_in_hk, district_selected)
      cp_district <- st_intersection(district_recycling_points, district_selected)
      
      if(nrow(roads_in_district) == 0 || nrow(cp_district) == 0) {
        stop("No intersection found for roads or recycling points in the selected district.")
      }
      
      # Filter and cast to LINESTRING for compatibility with subsequent spatial operations
      roads_in_district <- roads_in_district %>%
        filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING")) %>%
        st_cast("LINESTRING")
      
      # Spatial operations and density calculation
      lixels_district <- lixelize_lines(roads_in_district, 5000, mindist = 2500)
      samples_district <- lines_center(lixels_district)
      densities_districts <- nkde(roads_in_district, events = cp_district,
                                  w = rep(1, nrow(cp_district)), samples = samples_district,
                                  kernel_name = "quartic", bw = 300, div= "bw", method = "simple",
                                  digits = 1, tol = 1, grid_shape = c(1,1), max_depth = 8,
                                  agg = 5, sparse = TRUE, verbose = FALSE)
      
      # Assign and rescale densities
      samples_district$density_cp <- densities_districts * 1000
      lixels_district$density_cp <- densities_districts * 1000
      
      list(lixels = lixels_district, samples = samples_district, cp = cp_district)
    })
    
    
    
    output$district_nkde_map <- renderTmap({
      
      data <- density_data()
      
      district_density_map <- osm_basemap +
        tm_shape(data$lixels) +
        tm_lines(col = "density_cp") +
        tm_shape(data$cp) +
        tm_layout(
          legend.position = c("left", "top"),
          legend.text.size = 0.5,  # Adjust this value as needed for smaller legend text
          legend.title.size = 0.6  # Adjust this value as needed for smaller legend title
        )+
        tm_dots(col = "legend", palette = c("blue", "red"), border.col = "black", size = 0.05) 
      
      district_density_map
    })
    
    observeEvent(input$go, {
      # Show loading message immediately when the button is clicked
      output$loadingMessage <- renderText({
        "Please wait, this process might take a few minutes..."
      })
      
      # Simulate a long-running process
      Sys.sleep(5)  # Placeholder for the actual operation
      
      # Optionally, update or hide the message after the operation is complete
      output$loadingMessage <- renderText({
        "Calculation completed!"
      })
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)












################# NKDE Plot Graph ###############

output$shatin_map <- renderTmap({
  
  roads_lines_shatin <- roads_lines_shatin
  
  lixels_shatin <- lixelize_lines(roads_in_shatin, 5000, mindist = 2500)
  samples_shatin <- lines_center(lixels_shatin)
  densities_shatin_ppcp <- densities_shatin_ppcp 
  
  samples_shatin$density_ppcp <- densities_shatin_ppcp
  lixels_shatin$density_ppcp <- densities_shatin_ppcp
  
  samples_shatin$density_ppcp <- samples_shatin$density_ppcp*1000
  lixels_shatin$density_ppcp <- lixels_shatin$density_ppcp*1000
  
  
  shatin_density_ppcp <- osm_basemap +
    tm_shape(lixels_shatin) +
    tm_lines(col = "density_ppcp") +
    tm_shape(cp_shatin) +
    tm_layout(
      legend.position = c("left", "top"),
      legend.text.size = 0.5,  # Adjust this value as needed for smaller legend text
      legend.title.size = 0.6  # Adjust this value as needed for smaller legend title
    )+
    tm_dots(col = "legend", palette = c("blue", "red"), border.col = "black", size = 0.05) 
  
  shatin_density_ppcp
})

output$kfun_shatin <- renderPlot(
  kfun_shatin_ppcp$plotk
  
)


output$ytm_map <- renderTmap({
  
  roads_lines_ytm <- roads_lines_ytm
  
  lixels_ytm <- lixelize_lines(roads_lines_ytm, 5000, mindist = 2500)
  samples_ytm <- lines_center(lixels_ytm)
  densities_ytm_ppcp <- densities_ytm_ppcp
  
  samples_ytm$density_ppcp <- densities_ytm_ppcp
  lixels_ytm$density_ppcp <- densities_ytm_ppcp
  
  samples_ytm$density_ppcp <- samples_ytm$density_ppcp*1000
  lixels_ytm$density_ppcp <- lixels_ytm$density_ppcp*1000
  
  ytm_density_ppcp <-osm_basemap + 
    tm_shape(lixels_ytm)+
    tm_lines(col="density_ppcp")+
    tm_shape(cp_ytm)+
    tm_dots(col = "legend", palette = c("blue", "red"), border.col = "black", size = 0.05)
  
  
  ytm_density_ppcp
})

output$kfun_ytm <- renderPlot(
  kfun_ytm_ppcp$plotk
  
)



output$wanchai_map <- renderTmap({
  
  roads_lines_wanchai <- roads_lines_wanchai
  
  
  # Apply lixelize_lines with mindist
  lixels_wanchai <- lixelize_lines(roads_lines_wanchai,5000, mindist = 2500)
  samples_wanchai <- lines_center(lixels_wanchai)
  
  densities_wanchai_ppcp <- densities_wanchai_ppcp
  
  samples_wanchai$density_ppcp <- densities_wanchai_ppcp
  lixels_wanchai$density_ppcp <- densities_wanchai_ppcp
  
  samples_wanchai$density_ppcp <- samples_wanchai$density_ppcp*1000
  lixels_wanchai$density_ppcp <- lixels_wanchai$density_ppcp*1000
  
  
  wanchai_density_ppcp <- osm_basemap +
    tm_shape(lixels_wanchai) +
    tm_lines(col = "density_ppcp") +
    tm_shape(cp_wanchai) +
    tm_layout(
      legend.position = c("left", "top"),
      legend.text.size = 0.5,  # Adjust this value as needed for smaller legend text
      legend.title.size = 0.6  # Adjust this value as needed for smaller legend title
    )+
    tm_dots(col = "legend", palette = c("blue", "red"), border.col = "black", size = 0.05) 
  
  wanchai_density_ppcp
})

output$kfun_wanchai <- renderPlot(
  kfun_wanchai_ppcp$plotk
  
)
