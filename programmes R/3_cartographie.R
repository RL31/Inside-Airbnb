if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("ggmap")) install.packages("ggmap")
if (!require("tidyverse")) install.packages("tidyverse")
library(ggmap)
library(tidyverse)
library(sf)
library(leaflet)

carte_dynamique <- function(ville,mot){
  leaflet(base_lissee %>% filter(ville==!!ville & mot==!!mot) %>% st_transform(4326)) %>%
    addTiles() %>%
    addPolygons(color = NA, weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                # fillColor = ~colorBin("YlOrRd", pbruit,bins=10)(pbruit),
                #fillColor = ~colorQuantile("YlOrRd", pbruit,n=4)(pbruit),
                fillColor=~colorFactor("YlOrRd", tranche_mot )(tranche_mot),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) 
}

# carte_dynamique("lyon","moustique")


# rectificatif sur les objets GGMAP
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

tuiles_osm <- function(ville){
  limites <- base_lissee %>% filter(ville==!!ville & mot=="bruit") %>% st_transform(4326) %>% st_bbox() %>% as.numeric()
  bb <- c(left = limites[1],  bottom = limites[2],  right = limites[3],  top = limites[4])
  fond <- get_stamenmap(bbox = bb, zoom = 13,maptype = "toner-backg" )
  return(fond)
}

fond_lyon <-tuiles_osm("lyon")
fond_paris <-tuiles_osm("paris")
fond_bordeaux <-tuiles_osm("bordeaux")

carte_statique <- function(ville,mot){
  # Use the function:
  ggmap_bbox(get(paste0("fond_",ville)) ) %>%   
    ggmap()+
    coord_sf(crs = st_crs(3857))+
    geom_sf(data=base_lissee %>%
              filter(ville==!!ville & mot==!!mot) %>% st_transform(3857),
            aes(fill=as.integer(tranche_mot)),
            color=NA,alpha=.7,inherit.aes = FALSE)+
    scale_fill_steps(name="",
                     low="#E8E8E8",high="#E4003A",
                     labels=c("",paste0("- ",mot," +"),""))+
    labs(title = paste0("Evocation de \'",mot,"\' par les locataires Airbnb à ",str_to_title(ville)),
         caption = "Source : Inside Airbnb\nContributions OpenStreetMap")+
    theme_void()+
    theme(legend.position = "bottom",
          plot.title = element_text(color="black",size=15,face="bold",family="Open Sans"),
          text = element_text(family="Open Sans"))
}


#carte_statique("lyon","moustique")
