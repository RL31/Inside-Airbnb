if (!require("tidyverse")) install.packages("tidyverse")
if (!require("btb")) install.packages("btb")
if (!require("cartography")) install.packages("cartography")
if (!require("ggmap")) install.packages("ggmap")
if (!require("leaflet")) install.packages("leaflet")
if (!require("sf")) install.packages("sf")
library(tidyverse)
library(btb)
library(sf)

lissage_mot <- function(ville,mot){
  
  # Preparation des donnees pour le lissage (2154 et sf to df)
  # un logement est bruyant si au moins 10% des commentaires font etat de bruit
   # ville="lyon"
   # mot="moustique"
  
  donnees_2154 <- base_analyse %>%
    filter(ville==!!ville) %>% 
    select(longitude,latitude,contains(!!mot)) %>% 
    rename_with(~str_replace_all(.,mot,"mot")) %>% 
    st_as_sf(coords=c("longitude","latitude"),na.fail = FALSE,crs=4326)  %>%
    st_transform(crs=2154) %>% 
    mutate(x=st_coordinates(.)[,1],
           y=st_coordinates(.)[,2],
           indic_mot=if_else(part_mot > 10,1,0),
           nbObs=1) %>% 
    st_drop_geometry() %>% 
    as.data.frame() %>% 
  select(where(is.numeric))
  
  # lissage par carreaux de 100m, rayon de 500 m 
  lissage <- kernelSmoothing(dfObservations = donnees_2154,
                             sEPSG=2154,
                             iCellSize = 100,
                             iBandwidth = 500)   %>%
    mutate(pmot=indic_mot/nbObs*100,
           tranche_mot=cut(pmot,
                           breaks=cartography::getBreaks(pmot,
                                                         nclass = 5,
                                                         method = "fisher-jenks"),
                           include.lowest=TRUE,
                           label=1:5),
           ville=ville,
           mot=mot)
  return(lissage)
}

villes <- c("paris","lyon","bordeaux")
mots <- c("bruit","moustique","restaurant","voiture","velo","loin","cher")

base_lissee <- map_dfr(villes,
                       function(x) map_dfr(mots,
                                           function(y) lissage_mot(x,y)))
