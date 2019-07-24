library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(leaflet)
library(mapview)
library(Hmisc)
library(wesanderson)

df <- read_csv("./Howard_et_al_data.csv")
places <- read_csv("./map_place_labels.csv")

color_map <-  tibble(colors = wes_palettes$IsleofDogs2,
                       code = 1:5)
##map 1

sites <- tibble("long" = df$`Longitude (dd.ddddd)`,
                    "lat" = df$`Latitude (dd.ddddd)`,
                    "Corg" = df$Corg) %>% 
  mutate(groups = cut2(.$Corg,g = 5)) %>% 
  mutate(code = as.numeric(groups)) %>% 
  left_join(y = color_map, by = "code") %>% 
  
  mutate(groups = str_replace_all(groups, ",", "-")) %>% 
  mutate(groups = str_replace_all(groups, "\\[", "(")) %>% 
  mutate(groups = str_replace_all(groups, "\\]", ")")) 



map <- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                        attributionControl = FALSE)) %>%   # add ocean basemap
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -81.0, lat = (24.4+25.6)/2, zoom = 9) %>%   # focus map in a certain area / zoom level
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap,   # add inset map
             position = 'topleft', 
             width = 200, height = 150,
             toggleDisplay = FALSE) %>% 
  
  addCircleMarkers(data = sites, ~long, ~lat,
                   weight = 1.5,
                   radius = 7, 
                  fillColor = sites$colors,
                  color = "black",
                   fillOpacity = 1, 
                   stroke = T) %>% 
  
  addLegend("bottomright", 
            title = "Soil C<sub>org</sub> content </br> (% dry wt.)",
            colors =color_map$colors,
            labels= sites$groups %>% unique() %>% .[c(3,2,1,5,4)],
            opacity = 1) %>% 
  
  addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(label), data = places, 
                      labelOptions = labelOptions(noHide = T, textsize = "16px", opacity = 0.4,
                                                  direction = 'center', textOnly = FALSE))
  
  
mapshot(map, file = "sup1_map.png", vwidth = 792,
        vheight = 544, zoom = 3 ) 


