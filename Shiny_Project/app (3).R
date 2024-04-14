#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(sf)
library(tmap)
library(arrow)
library(lubridate)
library(tidyverse)
library(sp)
library(raster)
library(classInt)
library(viridis)
library(spNetwork)
library(ggplot2)
library(ggmap)
library(tidymodels)
library(glmnet)
library(readxl)
library(shinydashboard)
library(shinycssloaders)
library(terra)
library(spatstat)
#pacman::p_load(spatstat)

#####################################

osm_basemap <- tm_basemap(server = "OpenStreetMap.HOT")
imagery_basemap <- tm_basemap(server = "Esri.WorldImagery")

hk_census <- read_excel("./data/aspatial/hkcensus.xlsx")

district_18 <- st_read(dsn = "./data/geospatial/hk_18Districts/",
                      layer = "HKDistrict18" )

sf_district_18 <- st_transform(district_18, crs = 2326)


#####################################
## KDE on whole of HK
cp <- read_csv("./data/aspatial/hkrecyclepoints.csv")

cp_sf <- st_as_sf(cp, 
                  coords = c("lgt","lat"), 
                  crs = 4326) %>%
  st_transform(crs=2326)

cp_sf <- cp_sf %>%
  st_union()

cp_sf <- cp_sf %>% st_transform(crs = 2326)

cp_ppp <- as.ppp(cp_sf)

cp <- as_Spatial(cp_sf)
cp_sp <- as(cp, "SpatialPoints")
cp_ppp <- as(cp_sp, "ppp")
cp_ppp_jit <- rjitter(cp_ppp, 
                          retry=TRUE, 
                          nsim=1, 
                          drop=TRUE)

district_18_transform <- district_18 %>%
  st_transform(crs = 2326)

district_18_sp <- as_Spatial(district_18_transform)

district_18_sp<-as(district_18_sp, "SpatialPolygons")

district_18_owin<-as(district_18_sp, "owin")

cp_ppp_jit <- rjitter(cp_ppp, 
                      retry=TRUE, 
                      nsim=1, 
                      drop=TRUE)

any(duplicated(cp_ppp_jit))

hk_cp_ppp <- cp_ppp_jit[district_18_owin]

hk_ppp.km <- rescale(hk_cp_ppp, 1000, "km")

####################################
#KDE for Wan Chai

wanchai <- district_18 %>%
  filter(ENAME == "WAN CHAI")

wanchai <- wanchai %>%
  st_transform(crs = 2326)

wanchai <- as_Spatial(wanchai)

wanchai_sp<-as(wanchai, "SpatialPolygons")

wanchai_owin<-as(wanchai_sp, "owin")

hk_ppp_jit <- rjitter(hk_cp_ppp, 
                      retry=TRUE, 
                      nsim=1, 
                      drop=TRUE)

hk_wanchai_ppp <- hk_ppp_jit[wanchai_owin]

hk_wanchai_ppp.km = rescale(hk_wanchai_ppp, 1000, "km")

wanchaigaussian <- density(hk_wanchai_ppp.km,
             sigma=bw.ppl,
             edge=TRUE,
             kernel="gaussian")

saveRDS(wanchaigaussian, file = "./data/rds/wanchaigaussian.rds")

wanchaigaussian <- read_rds("./data/rds/wanchaigaussian.rds")

# wanchaiepanechnikov <- density(hk_wanchai_ppp.km, 
#                                     sigma=bw.ppl, 
#                                     edge=TRUE, 
#                                     kernel="epanechnikov")
# 
# saveRDS(wanchaiepanechnikov, file = "./data/rds/wanchaiepanechnikov.rds")

wanchaiepanechnikov <- read_rds("./data/rds/wanchaiepanechnikov.rds")

# wanchaiquartic <- density(hk_wanchai_ppp.km, 
#                                sigma=bw.ppl, 
#                                edge=TRUE, 
#                                kernel="quartic")
# 
# saveRDS(wanchaiquartic, file = "./data/rds/wanchaiquartic.rds")

wanchaiquartic <- read_rds("./data/rds/wanchaiquartic.rds")

# wanchaidisc <- density(hk_wanchai_ppp.km, 
#                           sigma=bw.ppl, 
#                           edge=TRUE, 
#                           kernel="disc")
# 
# saveRDS(wanchaidisc, file = "./data/rds/wanchaidisc.rds")

wanchaidisc <- read_rds("./data/rds/wanchaidisc.rds")

####################################
#KDE for Sha Tin

shatin <- district_18 %>%
  filter(ENAME == "SHA TIN")

shatin <- shatin %>%
  st_transform(crs = 2326)

shatin <- as_Spatial(shatin)

shatin_sp <- as(shatin, "SpatialPolygons")

shatin_owin <- as(shatin_sp, "owin")

hk_ppp_jit <- rjitter(hk_cp_ppp, 
                      retry=TRUE, 
                      nsim=1, 
                      drop=TRUE)

hk_shatin_ppp = hk_ppp_jit[shatin_owin]

hk_shatin_ppp.km = rescale(hk_shatin_ppp, 1000, "km")

# shatingaussian <- density(hk_shatin_ppp.km, 
#                            sigma=bw.ppl, 
#                            edge=TRUE, 
#                            kernel="gaussian")
# 
# saveRDS(shatingaussian, file = "./data/rds/shatingaussian.rds")

shatingaussian <- read_rds("./data/rds/shatingaussian.rds")

# shatinepanechnikov <- density(hk_shatin_ppp.km, 
#                                sigma=bw.ppl, 
#                                edge=TRUE, 
#                                kernel="epanechnikov")
# 
# saveRDS(shatinepanechnikov, file = "./data/rds/shatinepanechnikov.rds")

shatinepanechnikov <- read_rds("./data/rds/shatinepanechnikov.rds")

# shatinquartic <- density(hk_shatin_ppp.km, 
#                           sigma=bw.ppl, 
#                           edge=TRUE, 
#                           kernel="quartic")
# 
# saveRDS(shatinquartic, file = "./data/rds/shatinquartic.rds")

shatinquartic <- read_rds("./data/rds/shatinquartic.rds")
# 
# shatindisc <- density(hk_shatin_ppp.km, 
#                        sigma=bw.ppl, 
#                        edge=TRUE, 
#                        kernel="disc")
# 
# saveRDS(shatindisc, file = "./data/rds/shatindisc.rds")

shatindisc <- read_rds("./data/rds/shatindisc.rds")

####################################
#KDE for Yau Tsim Mong

ytm <- district_18 %>%
  filter(ENAME == "YAU TSIM MONG")

ytm <- ytm %>%
  st_transform(crs = 2326)

ytm <- as_Spatial(ytm)

ytm_sp <- as(ytm, "SpatialPolygons")

ytm_owin <- as(ytm_sp, "owin")

hk_ppp_jit <- rjitter(hk_cp_ppp, 
                      retry=TRUE, 
                      nsim=1, 
                      drop=TRUE)

hk_ytm_ppp <- hk_ppp_jit[ytm_owin]

hk_ytm_ppp.km = rescale(hk_ytm_ppp, 1000, "km")

ytmgaussian <- density(hk_ytm_ppp.km,
                          sigma=bw.ppl,
                          edge=TRUE,
                          kernel="gaussian")
# 
# saveRDS(ytmgaussian, file = "./data/rds/ytmgaussian.rds")

ytmgaussian <- read_rds("./data/rds/ytmgaussian.rds")

# ytmepanechnikov <- density(hk_ytm_ppp.km, 
#                               sigma=bw.ppl, 
#                               edge=TRUE, 
#                               kernel="epanechnikov")
# 
# saveRDS(ytmepanechnikov, file = "./data/rds/ytmepanechnikov.rds")

ytmepanechnikov <- read_rds("./data/rds/ytmepanechnikov.rds")

# ytmquartic <- density(hk_ytm_ppp.km, 
#                          sigma=bw.ppl, 
#                          edge=TRUE, 
#                          kernel="quartic")
# 
# saveRDS(ytmquartic, file = "./data/rds/ytmquartic.rds")

ytmquartic <- read_rds("./data/rds/ytmquartic.rds")

# ytmdisc <- density(hk_ytm_ppp.km, 
#                       sigma=bw.ppl, 
#                       edge=TRUE, 
#                       kernel="disc")
# 
# saveRDS(ytmdisc, file = "./data/rds/ytmdisc.rds")

ytmdisc <- read_rds("./data/rds/ytmdisc.rds")

####################################
##G Function
#G_HK = Gest(hk_cp_ppp, correction = "border")
#plot(G_HK, xlim=c(0,500))

#G_HK = Gest(hk_cp_ppp, correction = "border")
#plot(G_HK, xlim=c(0,500))

#G_HK.csr <- envelope(hk_cp_ppp, Gest, nsim = 99)

#G_HK.csr <- read_rds("data/rds/G_HK.csr.rds")

#plot(G_HK.csr)

########################
ui <- fluidPage(
  
  # Navbar
  navbarPage("Project Daylight",
             theme = bs_theme(bootswatch = "lux"),
             
             # Homepage Panel
             tabPanel("Home",
                      imageOutput("logo"),
                      br(),
                      hr(),
                      br(),
                      h4(strong("Project Description")),
                      p("We would be using spatial point patterns analysis to study the distribution of recycling collection points in Hong Kong is random or clustered. We believe using this analysis is very useful to investigate whether there are any dependency relationships between them and the area’s density. From these results, we can make a fair comparison and conclusion."),
                      p("We will also be performing network constrained spatial point patterns analysis to analyse the spatial point patterns to delve deeper insights, and understand if these events are affected by a network or there are spatial points events occurring alongside a network in our Shiny application."),
                      p("For spatial point patterns analysis(SPPA), we would like to find out if the collection points in Hong Kong are randomly distributed throughout the country and if not, where are the locations with higher concentrations of collection points."),
                      p("For network constrained spatial point patterns analysis, we would like to discover whether the distribution of the collection points are affected by the road network in Singapore. Through these analyses, we can investigate whether the distribution of Airbnb locations in Singapore are affected by point events or the road network."),
                      br(),
                      h4(strong("Project Motivation")),
                      p("Our team want to develop an innovative application designed to assist government officials in addressing recycling challenges at national and district levels in Hong Kong."),
                      p("Hong Kong faces a significant waste management challenge due to limited landfill space, exacerbated by the COVID-19 pandemic and a lack of widespread recycling habits among its residents."),
                      p("Through our initiative, we aim not only to conserve Hong Kong's valuable resources but also to promote environmental sustainability when restructuring their recycling points. Furthermore, we aspire to offer valuable insights and lessons learned that can be applied by government officials beyond Hong Kong, guiding the implementation of successful nationwide recycling campaigns worldwide."),
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
                        tags$li("Yau Tsim Mong - the highest population in Hong Kong")
                      ),
                      br(),
                      p("For NetSPPA, we will be focusing on the street network in Rochor. We chose Rochor as Rochor has a significant number of Airbnbs and each type of point events (Tourist Attractions, Bus Stops, Hotels, Shopping Malls, MRTs, 7-11s and Universities) are greater than 5, which will allow us to draw better statistical conclusions than the other zones with too little points. For example, Kallang only has 1 attraction and 1 university hence we will not be able to draw reliable statistical conclusions using Network Cross K-Function."),
                      br(),
                      p("To know more about how to use our application, ", tags$a(href = "https://github.com/valtyl/IS415-GAA-Project/tree/master/others", "here"), " is our user guide!"),
                      br(),
                      h4(strong("Credits")),
                      imageOutput("smulogo"),
                      p("This project is done for IS415 Geospatial Analytics & Applications, a module in Singapore Management University with the guidance of Professor Kam Tin Seong."),
                      br(),
                      p("Done by:"),
                      tags$ul(
                        tags$li(tags$a(href = "https://www.linkedin.com/in/fang-qi-lim/", "Lim Fang Qi")),
                        tags$li(tags$a(href = "https://www.linkedin.com/in/yashica-k", "Lee Sheung Yan"))
                      )
             ), # close tabPanel("Home")
      
             # SPPA Panel
             tabPanel("SPPA",
         #Application title
                    titlePanel("Spatial Point Patterns Analysis"),
         
                      sidebarLayout(
                      sidebarPanel(fluid = TRUE, width = 3,
                        # If KDE tabPanel is clicked, sidebarPanel below will be shown
                            conditionalPanel(
                          'input.SPPA_var === "SPPA Kernel Density Estimation"',
                            selectInput(
                            "SPPA_main_var",
                            "Districts",
                            choices = c("Wan Chai", "Sha Tin", "Yau Tsim Mong"),
                            selected = "Sha Tin",
                            multiple = FALSE
                          ),
                          selectInput(
                            "SPPA_kernel",
                            "Kernel Smoothing Input",
                            choices = c("Gaussian" = "gaussian",
                                        "Epanechnikov" = "epanechnikov",
                                        "Quartic" = "quartic",
                                        "Disc" = "disc"),
                            selected = "gaussian",
                            multiple = FALSE
                          ),
                          actionButton("SPPA_Run_KDE", "Run Analysis")
                        ),
           ), # close sidebarPanel
           mainPanel(width = 9,
                     tabsetPanel(
                       id = "SPPA_var",
                       tabPanel("SPPA Kernel Density Estimation",
                                column(12,
                                       h6(strong("Note:")),
                                       p(em("Please select the district and kernel smoothing method of interest for the graph to load.")),
                                       p(em("Variable: Collection Points in Sha Tin, Kernel: Gaussian, and Bandwidth Method. Select alternative choices and click on 'Run Analysis' to update the map.")),
                                       withSpinner(plotOutput("SPPA_KDE_Map"), type = 2, color.background = "#ffffff"),
                                       # withSpinner(tmapOutput("KDEPlot", width = "100%", height = 400), type=2, color.background = "#ffffff"),
                                       tabsetPanel(
                                         id = "SPPA_KDE_info",
                                         tabPanel("About Spatial Kernel Density Estimation",
                                                  column(12,
                                                         h4("What is Spatial Kernel Density Estimation?"),
                                                         p("Kernel Density Estimation (KDE) is one of the most used density-based measures to estimate local density. It creates a grid in which each cell is assigned the density value of the kernel window centred on that cell. The density value is estimated by counting the number of objects/events in that kernel window."),
                                                         h5("How to interpret the output?"),
                                                         p("The brighter the colour towards yellow, means that the collection points in that district is more heavily concentrated there. The darker shades, in navy blue indicate lower densities of collection points in that area.")
                                                  )))))
                     ) # close tabsetPanel
           ) # close mainPanel
         )) # close sidebarLayout
       )
)# close tabPanel

#NetSPPA goes down here
##########################################
server <- function(input, output, session) {
  
  sppa_var <- function(){
    map <- NULL
    
    if(input$SPPA_main_var == "Wan Chai"){
      if (input$SPPA_kernel == "gaussian") {
        map <- wanchaigaussian }
      else if (input$SPPA_kernel == "epanechnikov") {
        map <- wanchaiepanechnikov }
      else if (input$SPPA_kernel == "quartic") {
        map <- wanchaiquartic }
      else if (input$SPPA_kernel == "disc") {
        map <- wanchaidisc }
    }
    else if (input$SPPA_main_var == "Sha Tin"){
      if (input$SPPA_kernel == "gaussian") {
        map <- shatingaussian }
      else if (input$SPPA_kernel == "epanechnikov") {
        map <- shatinepanechnikov }
      else if (input$SPPA_kernel == "quartic") {
        map <- shatinquartic }
      else if (input$SPPA_kernel == "disc") {
        map <- shatindisc }
    }
    else if (input$SPPA_main_var == "Yau Tsim Mong"){
      if (input$SPPA_kernel == "gaussian") { 
        map <- ytmgaussian }
      else if (input$SPPA_kernel == "epanechnikov") {
        map <- ytmepanechnikov }
      else if (input$SPPA_kernel == "quartic") {
        map <- ytmquartic }
      else if (input$SPPA_kernel == "disc") {
        map <- ytmdisc }
    }
    return(map)
  }
  
  generate_plot <- eventReactive(input$SPPA_Run_KDE, {
    plot(sppa_var(), main=paste(input$SPPA_main_var, "-", input$SPPA_kernel))
  })

  output$SPPA_KDE_Map <- renderPlot({
    generate_plot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
