library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(pxR)
library(dplyr)
library(sf)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(lubridate)
library(stringi)
library(leaflet.extras)
library(leafem)

#set up working directory, input and output folders

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

HR <- readRDS("EDMapSpatialHR.Rds")
IHA <- readRDS("EDMapSpatialIHA.Rds")
CHN <- readRDS("EDMapSpatialCHN.Rds")

Map <- leaflet(options = leafletOptions(zoomControl = T)) %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addPolygons(data=HR, 
              weight = 2, 
              fillColor = "yellow",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(
                # Highlight stroke parameters
                weight = 3, color = "white",
                # Highlight fill parameters
                fillColor = "blue", 
                fillOpacity = 0.1), 
              popup = paste0("<b>Health Region: </b>", HR$NAME,"<br>",  HR$Report,"<br>"),
              group = "Health Region")%>%
  addPolygons(data=IHA, 
              weight = 2, 
              fillColor = "yellow",
              highlightOptions = highlightOptions(
                # Highlight stroke parameters
                weight = 3, color = "white",
                # Highlight fill parameters
                fillColor = "blue", fillOpacity = 0.1), 
              popup = paste0("<b>IHA: </b>", IHA$IHA.x,"<br>",  IHA$Report,"<br>"),
              group = "IHA")%>%
  addPolygons(data=CHN, 
              weight = 2, 
              fillColor = "yellow",
              highlightOptions = highlightOptions(
                # Highlight stroke parameters
                weight = 3, color = "white",
                # Highlight fill parameters
                fillColor = "blue", fillOpacity = 0.1), 
              popup = paste0("<b>CHN: </b>", CHN$CHN,"<br>",  CHN$Report,"<br>"),
              group = "CHN")%>%
  addLayersControl(
    baseGroups = c("Health Region", "IHA","CHN"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addScaleBar()

saveWidget(Map, file = "index.html", selfcontained = T)

