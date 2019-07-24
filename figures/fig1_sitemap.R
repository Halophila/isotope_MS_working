library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(leaflet)
library(mapview)

df <- read_csv("./Howard_et_al_data.csv")
places <- read_csv("./map_place_labels.csv")


sites <- data.frame("long" = df$`Longitude (dd.ddddd)`,
                    "lat" = df$`Latitude (dd.ddddd)`,
                    "region" = df$better_location)

cbPalette <- c("#56B4E9", "#009E73", "#E69F00")

pal <- colorFactor(
  palette = cbPalette,
  domain = sites$region
)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                        attributionControl = FALSE)) %>%   # add ocean basemap
  addProviderTiles(providers$Esri.WorldImagery) %>% # focus map in a certain area / zoom level
  setView(lng = -81.1, lat = (24.4+25.6)/2, zoom = 9) %>%  # add inset map
  addMiniMap(tiles = providers$Esri.WorldStreetMap,
             position = 'topleft', 
             width = 150, height = 100,
             toggleDisplay = FALSE) %>% 
  
  addCircleMarkers(data = sites, ~long, ~lat,
                   weight = 1.5,
                   radius = 7, 
                   fillColor = ~pal(region),
                   fillOpacity = 1, 
                   stroke = T) %>% 
  
  addLegend("bottomright", 
            colors = cbPalette[c(3,1,2)],
            labels= c("Western FL Bay","Eastern FL Bay", "FL Keys"),
            opacity = 1) %>% 
  
  addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(label), data = places, 
                      labelOptions = labelOptions(noHide = T, textsize = "16px", opacity = 0.4,
                                                  direction = 'center', textOnly = FALSE))

map

mapshot(map, file = "fig1_sitemap.png", vwidth = 792,
        vheight = 544, zoom = 3 ) 

