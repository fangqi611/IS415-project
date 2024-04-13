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
library(shinycssloaders)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


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
roads_in_shatin <- read_rds("data/rds/roads_in_shatin.rds")
roads_lines_shatin <- read_rds("data/rds/roads_lines_shatin.rds")
densities_shatin_ppcp <- read_rds("data/rds/densities_shatin_ppcp.rds")
kfun_shatin_ppcp <-  read_rds("data/rds/kfun_shatin_ppcp.rds")


private_cp_ytm <- read_rds("data/rds/private_cp_ytm.rds")
public_cp_ytm <- read_rds("data/rds/public_cp_ytm.rds")
cp_ytm <-  read_rds("data/rds/cp_ytm.rds")
roads_in_ytm <- read_rds("data/rds/roads_in_ytm.rds")
roads_lines_ytm <- read_rds("data/rds/roads_lines_ytm.rds")
densities_ytm_ppcp <- read_rds("data/rds/densities_ytm_ppcp.rds")
kfun_ytm_ppcp <-  read_rds("data/rds/kfun_ytm_ppcp.rds")

private_cp_wanchai <- read_rds("data/rds/private_cp_wanchai.rds")
public_cp_wanchai <- read_rds("data/rds/public_cp_wanchai.rds")
cp_wanchai <-  read_rds("data/rds/cp_wanchai.rds")
roads_in_wanchai <- read_rds("data/rds/roads_in_wanchai.rds")
roads_lines_wanchai <- read_rds("data/rds/roads_lines_wanchai.rds")
densities_wanchai_ppcp <- read_rds("data/rds/densities_wanchai_ppcp.rds")
kfun_wanchai_ppcp <-  read_rds("data/rds/kfun_wanchai_ppcp.rds")




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
ui <- fluidPage(tags$head(
  tags$style(HTML("
            .navbar, .navbar-default {
                background-color: #228B22 !important;  /* Forest Green */
                border-color: #228B22 !important;
            }
            .navbar-default .navbar-nav > li > a, .navbar-default .navbar-brand {
                color: #fff !important;  /* White text for contrast */
            }
            .navbar-default .navbar-nav > .active > a, 
            .navbar-default .navbar-nav > .active > a:hover, 
            .navbar-default .navbar-nav > .active > a:focus {
                background-color: #8DAF8D !important;  /* Darker shade of #9EC89E for active tabs */
                color: #fff !important;
            }
        "))
),
                
                # -----Navigation Bar
                navbarPage("Project Daylight", 
                           
                           tabPanel("Home", 
                                    h2(div(style = "text-align: center;", "Welcome to Project Daylight!")),
                                    div(style = "text-align: center;", imageOutput("logo")), 
                                    br(),
                                    h4(strong("Project Description")),
                                    p("We would be using spatial point patterns analysis to study the distribution of recycling collection points in Hong Kong is random or clustered. We believe 
                                      using this analysis is very useful to investigate whether there are any dependency relationships between them and the area’s density. From these results, 
                                      we can make a fair comparison and conclusion."),
                                    p("We will also be performing network constrained spatial point patterns analysis to analyse the spatial point patterns to delve deeper insights, and understand
                                      if these events are affected by a network or there are spatial points events occurring alongside a network in our Shiny application."),
                                    p("For spatial point patterns analysis(SPPA), we would like to find out if the collection points in Hong Kong are randomly distributed throughout the country 
                                      and if not, where are the locations with higher concentrations of collection points."),
                                    p("For network constrained spatial point patterns analysis, we would like to discover whether the distribution of the collection points are affected by the 
                                      road network in Singapore. Through these analyses, we can investigate whether the distribution of Airbnb locations in Singapore are affected by point events 
                                      or the road network."),
                                    br(),
                                    h4(strong("Project Motivation")),
                                    p("Our team want to develop an innovative application designed to assist government officials in addressing recycling challenges at national and district 
                                      levels in Hong Kong. Hong Kong faces a significant waste management challenge due to limited landfill space, exacerbated by the COVID-19 pandemic and a lack 
                                      of widespread recycling habits among its residents."),
                                    p("Through our initiative, we aim not only to conserve Hong Kong's valuable resources but also to promote environmental sustainability when restructuring 
                                      their recycling points. Furthermore, we aspire to offer valuable insights and lessons learned that can be applied by government officials beyond Hong Kong, 
                                      guiding the implementation of successful nationwide recycling campaigns worldwide."),
                                    br(),
                                    h4(strong("About our Application")),
                                    p("Our application is focused on recycling collection points in Singapore and will assist users with two methods of Point Pattern Analysis:"),
                                    tags$ul(
                                      tags$li("Spatial Point Patterns Analysis (SPPA)"),
                                      tags$li("Network-Constrained Point Patterns Analysis (NetSPPA)")
                                    ),
                                    br(),
                                    p("For SPPA, users will be able to view the kernel density map of recycling collection points in different districts. We have chosen 3 districts, Wan Chai, Sha Tin and Yau Tsim Mong:"),
                                    tags$ul(
                                      tags$li("Hong Kong"),
                                      tags$li("Wan Chai - the least population in Hong Kong"),
                                      tags$li("Sha Tin - the second population in Hong Kong behind Yau Tsim Mong"),
                                      tags$li("Yau Tsim Mong - the highest population in Hong Kong"),
                                    ),
                                    br(),
                                    p("For NetSPPA, we will be focusing on the street network in Rochor. We chose Rochor as Rochor has a significant number of Airbnbs and each type of point events (Tourist Attractions, Bus Stops, Hotels, Shopping Malls, MRTs, 7-11s and Universities) are greater than 5, which will allow us to draw better statistical conclusions than the other zones with too little points. For example, Kallang only has 1 attraction and 1 university hence we will not be able to draw reliable statistical conclusions using Network Cross K-Function."),
                                    br(),
                                    p("To know more about how to use our application, ", tags$a(href="https://github.com/valtyl/IS415-GAA-Project/tree/master/others", "here"), " is our user guide!"),
                                    br(),
                                    h4(strong("Credits")),
                                    imageOutput("smu_logo"),
                                    p("This project is done for IS415 Geospatial Analytics & Applications, a module in Singapore Management University with the guidance of Professor Kam Tin Seong."),
                                    br(),
                                    p("Done by:"),
                                    tags$ul(
                                      tags$li(tags$a(href="https://www.linkedin.com/in/fang-qi-lim/", "Lim Fang Qi")),
                                      tags$li(tags$a(href="https://www.linkedin.com/in/yashica-k", "Karina Lee Sheung Yan")),
                                    ),
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
                                          column(12,
                                                 h6(strong("Note:")),
                                                 p(em("Please wait a short while for the default map to load.")),
                                                 withSpinner(tmapOutput("cp_map"), type=2)),
                                          uiOutput("noDataMessage"),
                                          br(),
                                          h3("List of Recycling Points in Hong Kong"),
                                          DTOutput("cp_data_table")
                                        )
                                       )), 
                                      
                                      ), 
                           tabPanel("NKDE", icon = icon("globe"), 
                                    h2(p("Network Spatial Point Analysis")), 
                                    tabsetPanel(
                                      tabPanel("Sha Tin",
                                               column(12,
                                                      h6(strong("Note:")),
                                                      p(em("Please wait a short while for the default map to load.")),
                                                     withSpinner(tmapOutput("shatin_map"), type=2)),
                                               br(),
                                               hr(), 
                                               tabsetPanel(
                                                 tabPanel("About Network-Constrained Kernel Density Estimation",
                                                          column(12,
                                                                 h4("What is Network-Constrained Kernel Density Estimation?"),
                                                                 p("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a two-dimensional space, 
                                                          which is not suitable for analysing density of events occuring on a network. Therefore, the modified Network-Constrained 
                                                          Kernel Density Estimation is used to calculate density of events occuring along the edges of a network."),
                                                                 h4("How to interpret the output?"),
                                                                 p("The road segments of darker colour have relatively higher density of point events than the road segments of lighter colour."),
                                                          )), 
                                                 tabPanel("Observations", 
                                                          column(12,
                                                                 h4("What can we observe from the NetSPPA KDE Map?"),
                                                                 p("Overall speaking, Sha Tin District has a good spread of recycling points both public and private recycling points. "), 
                                                                 h4("Interesting Findings"), 
                                                                 imageOutput("shatin_if"),
                                                                 p("This aligns with the planning of the district since these two areas are the city centre of the district, with large shopping malls 
                                                                   and main amenities. Therefore, the network density of these areas are relatively higher.")
                                                                 )
                                                   
                                                 ), 
                                                 tabPanel("NetSPPA K-Function", 
                                                          column(12,
                                                                 h4("What is K-Function?"),
                                                                 p("K-function measures the number of events found up to a given distance of any particular event, and the graph helps illustrate the spatial dependence (clustering or dispersion) of point features over a wide range of distances (m)."),
                                                                 h4("How to interpret the graph?"),
                                                                 plotOutput("kfun_shatin"),
                                                                 p("Null hypothesis: The distribution of point events are uniformly distributed over the street network in Sha Tin District"),
                                                                 p("1) If the empirical network K-function of the point events in Sha Tin District is above the envelope, it indicates that the point events in  Sha Tin District are more clustered than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("2) If the empirical network K-function of the point events in Sha Tin District is below the envelope, it indicates that the point events in  Sha Tin District are more dispersed than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("3) If the empirical network K-function of the point events in Sha Tin District is inside the envelope, it indicates that the point events in  Sha Tin District are uniformly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                          )
                                                          
                                                 )
                                               )), 
                                      
                                      tabPanel("Yau Tsim Mong",
                                               column(12,
                                                      h6(strong("Note:")),
                                                      p(em("Please wait a short while for the default map to load.")),
                                                      withSpinner(tmapOutput("ytm_map"), type=2)), 
                                               hr(), 
                                               br(),
                                               tabsetPanel(
                                                 tabPanel("About Network-Constrained Kernel Density Estimation",
                                                          column(12,
                                                                 h4("What is Network-Constrained Kernel Density Estimation?"),
                                                                 p("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a two-dimensional space, 
                                                          which is not suitable for analysing density of events occuring on a network. Therefore, the modified Network-Constrained 
                                                          Kernel Density Estimation is used to calculate density of events occuring along the edges of a network."),
                                                                 h4("How to interpret the output?"),
                                                                 p("The road segments of darker colour have relatively higher density of point events than the road segments of lighter colour."),
                                                          )), 
                                                 tabPanel("Observations", 
                                                          column(12,
                                                                 h4("What can we observe from the NetSPPA KDE Map?"),
                                                                 p("Overall speaking, it has a high density network across the district, especially in Mong Kok and along the Nathan Road. "), 
                                                                 h4("Interesting Findings"), 
                                                                 div(style = "text-align: center;",imageOutput("ytm_if")),
                                                                 p("It is interesting to find that the density of networks in Yau Tsim Mong district is highly dense, especially along the Nathan 
                                                                   Road. This may be due to the fact that Nathan Road is the longest street in Hong Kong as well as located with different commercial 
                                                                   activities along the street.")
                                                          )), 
                                                 tabPanel("NetSPPA K-Function", 
                                                          column(12,
                                                                 h4("What is K-Function?"),
                                                                 p("K-function measures the number of events found up to a given distance of any particular event, and the graph helps illustrate the spatial dependence (clustering or dispersion) of point features over a wide range of distances (m)."),
                                                                 h4("How to interpret the graph?"),
                                                                 plotOutput("kfun_ytm"),
                                                                 p("Null hypothesis: The distribution of point events are uniformly distributed over the street network in  Yau Tsim Mong District"),
                                                                 p("1) If the empirical network K-function of the point events in Yau Tsim Mong District is above the envelope, it indicates that the point events in Yau Tsim Mong District are more clustered than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("2) If the empirical network K-function of the point events in Yau Tsim Mong District is below the envelope, it indicates that the point events in Yau Tsim Mong District are more dispersed than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("3) If the empirical network K-function of the point events in Yau Tsim Mong District is inside the envelope, it indicates that the point events in Yau Tsim Mong District are uniformly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                          )
                                                          
                                                 ))), 
                                      tabPanel("Wan Chai",
                                               column(12,
                                                      h6(strong("Note:")),
                                                      p(em("Please wait a short while for the default map to load.")),
                                                      withSpinner(tmapOutput("wanchai_map"), type=2)), 
                                               hr(), 
                                               br(),
                                               tabsetPanel(
                                                 tabPanel("About Network-Constrained Kernel Density Estimation",
                                                          column(12,
                                                                 h4("What is Network-Constrained Kernel Density Estimation?"),
                                                                 p("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a two-dimensional space, 
                                                          which is not suitable for analysing density of events occuring on a network. Therefore, the modified Network-Constrained 
                                                          Kernel Density Estimation is used to calculate density of events occuring along the edges of a network."),
                                                                 h4("How to interpret the output?"),
                                                                 p("The road segments of darker colour have relatively higher density of point events than the road segments of lighter colour."),
                                                          )), 
                                                 tabPanel("Observations", 
                                                          column(12,
                                                                 h4("What can we observe from the NetSPPA KDE Map?"),
                                                                 p("There are more private collection points in the highland of Wan Chai. In addition, we can find there are 3 circled areas in the 
                                                                   heartlands of Wan Chai District e.g. Causeway Bay, the centre part of Wan Chai and Happy Valley."), 
                                                                 h4("Interesting Findings"), 
                                                                 div(style = "text-align: center;", imageOutput("wanchai_if")),
                                                                 p("It is interesting to find that the higher area of Wan Chai has more distribution of private recycling facilities. This may be 
                                                                   due to the fact that there are more residential areas in the highland of Wan Chai.")
                                                          )
                                                          
                                                 ), 
                                                 tabPanel("NetSPPA K-Function", 
                                                          column(12,
                                                                 h4("What is K-Function?"),
                                                                 p("K-function measures the number of events found up to a given distance of any particular event, and the graph helps illustrate the spatial dependence (clustering or dispersion) of point features over a wide range of distances (m)."),
                                                                 h4("How to interpret the graph?"),
                                                                 plotOutput("kfun_wanchai"),
                                                                 p("Null hypothesis: The distribution of point events are uniformly distributed over the street network in Wan Chai District"),
                                                                 p("1) If the empirical network K-function of the point events in Wan Chai District is above the envelope, it indicates that the point events in Wan Chai District are more clustered than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("2) If the empirical network K-function of the point events in Wan Chai District is below the envelope, it indicates that the point events in Wan Chai District are more dispersed than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                 p("3) If the empirical network K-function of the point events in Wan Chai District is inside the envelope, it indicates that the point events in Wan Chai District are uniformly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                          )
                                                          
                                                 ))
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
    
################# images ################
    
    output$logo <- renderImage({
      list(src = "images/Logo.png",
           style = "max-width: 100%; height: auto; display: block; margin-left: auto; margin-right: auto;")
    },deleteFile = FALSE)
    
    output$smu_logo <- renderImage({
      list(src = "images/smu_logo.jpg",
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
      if (!any(is.na(map_data))) {
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
    
    output$shatin_map <- renderTmap({
    
      roads_lines_shatin <- roads_lines_shatin
      
      lixels_shatin <- lixelize_lines(roads_lines_shatin, 5000, mindist = 2500)
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
        tm_layout(
          legend.position = c("left", "top"),
          legend.text.size = 0.5,  # Adjust this value as needed for smaller legend text
          legend.title.size = 0.6  # Adjust this value as needed for smaller legend title
        )+
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
 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
