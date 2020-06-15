#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(dplyr)
library(mlr)
library(ggplot2)
library(rgdal)
library(readr)
library(broom)
library(RColorBrewer)
library(ggmap)
library(tmap)
library(rgdal)
library(raster)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(data.table)
library(lubridate)
library(GISTools)
library(tmap)

## Reading Data: This is on my Machine, so set directory to wherever you've downloaded the dataset.

THOR_Data <- fread("C:/Users/Finnmcoolr/Documents/THOR WWII/THOR_WWII_DATA_CLEAN.csv")

NUTS_Geog <- st_read("C:/Users/Finnmcoolr/Documents/THOR WWII/NUTS_RG_01M_2016_4326_LEVL_3.shp/NUTS_RG_01M_2016_4326_LEVL_3.shp")

##Initial Plot to make sure everything works.

plot(NUTS_Geog["LEVL_CODE"])

##Clean Data to eliminate variables without proper geocoding
THOR.y <-THOR_Data[!(is.na(THOR_Data$LATITUDE) | THOR_Data$LATITUDE==""), ]
THOR.z <-THOR.y[!(is.na(THOR.y$LONGITUDE) | THOR.y$LONGITUDE==""), ]


##Subset our Data to pull only European for matching to our NUTS Shapefile (Note that the entire process would work the same for Pacific or North African Theaters, the "ETO" would simply be changed)
ThorDataEurope <- THOR.z[(THOR.z$THEATER =="ETO"),]

##Set our Raw Coordinates to sf Geometry
ThorPoints <- st_as_sf(ThorDataEurope, crs = 4326, coords = c("LONGITUDE","LATITUDE"))

##Reproject just to be sure (for my purposes, both sets were in WGS84)
ThorReproj <- st_transform(ThorPoints, crs(NUTS_Geog))

##Format our Date (to make for easier subsetting)
ThorPoints$MSNDATE <- as.Date(ThorPoints$MSNDATE , format = "%m/%d/%Y")

##Order our Date for the same Purpose
ThorPoints[order(ThorPoints$MSNDATE ),]


##Filter out Non-European French States in South America and the Pacific  
NUTS_Geog <- subset(NUTS_Geog,NUTS_Geog$NUTS_ID != "FRY30")
NUTS_Geog <- subset(NUTS_Geog,NUTS_Geog$NUTS_ID != "FRY20")
NUTS_Geog <- subset(NUTS_Geog,NUTS_Geog$NUTS_ID != "FRY10")
NUTS_Geog <- subset(NUTS_Geog,NUTS_Geog$NUTS_ID != "FRY40")
NUTS_Geog <- subset(NUTS_Geog,NUTS_Geog$NUTS_ID != "FRY50")

##Subset our Points Dataset by Year for ease of computation
ThorPts39 <- subset(ThorPoints, MSNDATE >= "1939-01-01" & MSNDATE <= "1939-12-31")
year(ThorPts39$MSNDATE) <-0

##Perform a Left Spatial Join to Create a New Polygon Layer with Counts for Yearly Bombings   
wth39 <- st_join(ThorPts39, NUTS_Geog, join = st_within)
count39 <- count(as_tibble(wth39), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog39 <- left_join(NUTS_Geog, count39)

##Repeat For Every Year
ThorPts40 <- subset(ThorPoints, MSNDATE >= "1940-01-01" & MSNDATE <= "1940-12-31")
year(ThorPts40$MSNDATE) <-0
wth40 <- st_join(ThorPts40, NUTS_Geog, join = st_within)
count40 <- count(as_tibble(wth40), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog40 <- left_join(NUTS_Geog, count40)

ThorPts41 <- subset(ThorPoints, MSNDATE >= "1941-01-01" & MSNDATE <= "1941-12-31")
year(ThorPts41$MSNDATE) <-0
wth41 <- st_join(ThorPts41, NUTS_Geog, join = st_within)
count41 <- count(as_tibble(wth41), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog41 <- left_join(NUTS_Geog, count41)

ThorPts42 <- subset(ThorPoints, MSNDATE >= "1942-01-01" & MSNDATE <= "1942-12-31")
year(ThorPts42$MSNDATE) <-0
wth42 <- st_join(ThorPts42, NUTS_Geog, join = st_within)
count42 <- count(as_tibble(wth42), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog42 <- left_join(NUTS_Geog, count42)


ThorPts43 <- subset(ThorPoints, MSNDATE >= "1943-01-01" & MSNDATE <= "1943-12-31")
year(ThorPts43$MSNDATE) <-0
wth43 <- st_join(ThorPts43, NUTS_Geog, join = st_within)
count43 <- count(as_tibble(wth43), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog43 <- left_join(NUTS_Geog, count43)


ThorPts44 <- subset(ThorPoints, MSNDATE >= "1944-01-01" & MSNDATE <= "1944-12-31")
year(ThorPts44$MSNDATE) <-0
wth44 <- st_join(ThorPts44, NUTS_Geog, join = st_within)
count44 <- count(as_tibble(wth44), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog44 <- left_join(NUTS_Geog, count44)


ThorPts45 <- subset(ThorPoints, MSNDATE >= "1945-01-01" & MSNDATE <= "1945-12-31")
year(ThorPts45$MSNDATE) <-0
wth45 <- st_join(ThorPts45, NUTS_Geog, join = st_within)
count45 <- count(as_tibble(wth45), NUTS_ID) %>% rename("ThorPts"= n)
NUTS_Geog45 <- left_join(NUTS_Geog, count45)


##Replace NA values with 0
NUTS_Geog39[is.na(NUTS_Geog39)] = 0
NUTS_Geog40[is.na(NUTS_Geog39)] = 0
NUTS_Geog41[is.na(NUTS_Geog39)] = 0
NUTS_Geog42[is.na(NUTS_Geog39)] = 0
NUTS_Geog43[is.na(NUTS_Geog39)] = 0
NUTS_Geog44[is.na(NUTS_Geog39)] = 0
NUTS_Geog45[is.na(NUTS_Geog39)] = 0

##Double Check everything didn't explode
st_is_valid(NUTS_Geog)


##Begin our Shinyapp!

##Start with UI Values
ui <- fluidPage(
  ##Title Creation
  titlePanel("THOR Data Map"),
  sidebarLayout(
    sidebarPanel(
      ##Buttons to Select Year for Point Map
      radioButtons("year", "Select Year of engagement", c("1939" = "ThorPts39","1940" = "ThorPts40","1941" = "ThorPts41","1942" = "ThorPts42","1943" = "ThorPts43","1944" = "ThorPts44", "1945" = "ThorPts45"),),
      ##Temporal Slider to Select Date for Point Map
      sliderInput("date", "Select Date:",
                  min = as.Date("0000-01-01","%Y-%m-%d"),
                  max = as.Date("0000-12-31","%Y-%m-%d"),
                  value=as.Date("0000-09-27"),
                  timeFormat= "%Y-%m-%d"),
      ##Selection Box to Select Year for Polygon Choropleth
      selectInput("year2", "Select Year for Choropleth", c("1939" = "NUTS_Geog39","1940" = "NUTS_Geog40","1941" = "NUTS_Geog41","1942" = "NUTS_Geog42","1943" = "NUTS_Geog43","1944" = "NUTS_Geog44", "1945" = "NUTS_Geog45"))),
    mainPanel(
      ##Map Outputs
      leafletOutput("mymap"),
      leafletOutput("mymap2"),
      ))) 
##Server Side Values
server <- shinyServer(function(input, output){
  ##Set Reactive Input Values for our Years Inputs
  mapdata <- reactive({get(input$year)})
  mapdata2 <- reactive({get(input$year2)})
  ##Render First Point Map
  output$mymap <- renderLeaflet({
    mydata <- mapdata()
    leaflet()%>%
      ##Set Default View
      setView(15, 47.8, 4)%>%
      ##Load Basemap
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      ##Set Point Parameters
      addCircleMarkers(data = mydata[mydata$MSNDATE == input$date,],
                       ##Set Popup Labels With Attribute Values
                       popup = paste(paste(paste("Country of Origin",mydata$COUNTRY_FLYING_MISSION, sep = ": " ), paste("Plane Model", mydata$AIRCRAFT_NAME, sep = ": "), sep = "<br>"),paste("Target Type", mydata$TGT_TYP, sep = ": "),sep = "<br>")
                       )
  })
  ##Render Second Choropleth Map
  output$mymap2 <- renderLeaflet({
    
    mydata2 <-mapdata2()
    ##Set Palette for Colors
    pal <- colorBin("Blues", domain = mydata2$ThorPts, bins = c(0,1,5, 20, 50, 100,200,500,1000, Inf))
    leaflet(mydata2) %>%
      ##Load Basemap
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      ##Set Polygon Parameters
      addPolygons(
        fillColor = ~pal(ThorPts),
        weight = .5,
        opacity = .5,
        color = "grey",
        highlight = highlightOptions(
          weight = 3,
          color = "blue",
          bringToFront = TRUE
        ),
        ##Set Popup Attribute Labels
        popup = paste(paste("Region Name",mydata2$NUTS_NAME, sep = ": " ), paste("Bombings", mydata2$ThorPts, sep = ": "), sep = "<br>")
      ) %>%
      ##Add Legend
    addLegend(pal = pal, values = ~ThorPts, opacity = 0.7, title = "Bombings per Region",
              position = "bottomright")
    
  })
  
})

shinyApp(ui = ui, server = server)
