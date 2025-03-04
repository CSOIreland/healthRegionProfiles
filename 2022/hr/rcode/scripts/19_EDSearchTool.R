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

EDsMap <- st_read(paste0(InputFilesLoc,"/shps/hr/hr_reprojected.shp"))
SAPSHRLookupHROnly <- SAPSHRLookup%>%group_by(Region)%>%
  filter(row_number()==1)%>%filter(Region!="State")
CSVForHTMLWithLookup <- merge(CSVForHTMLToExport,SAPSHRLookupHROnly, by.x = "Health Region", by.y = "Region", all.x = T )
EDsMap <- merge(EDsMap, CSVForHTMLWithLookup, by.x = "NAME",by.y = "Health Region", all.x = T)
EDMapSpatial <- as_Spatial(EDsMap)
saveRDS(EDMapSpatial, file = paste0(OutputFilesLoc,"/EDMapSpatialHR.Rds"))

Map <- leaflet(options = leafletOptions(zoomControl = T)) %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addPolygons(data=EDMapSpatial, 
              weight = 2, 
              fillColor = "yellow",
              highlightOptions = highlightOptions(
                                                   # Highlight stroke parameters
                                                     weight = 3, color = "white",
                                                    # Highlight fill parameters
                                                 fillColor = "blue", fillOpacity = 0.1), 
    popup = paste0("<b>ED NAME: </b>", EDMapSpatial$ED_ENGLISH,"<br>","<b>AC NAME: </b>", EDMapSpatial$AC,"<br>",  EDMapSpatial$Report,"<br>"))%>%
  addScaleBar()

setwd(OutputFilesLoc)
saveWidget(Map, file = "index.html", selfcontained = T)
setwd(RootWD)
