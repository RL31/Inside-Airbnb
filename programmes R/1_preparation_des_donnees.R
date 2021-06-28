if (!require("aws.s3")) install.packages("aws.s3")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("crossmap")) install.packages("crossmap")
if (!require("cld3")) install.packages("cld3")
if (!require("stringi")) install.packages("stringi")
library(cld3)
library(crossmap)
library(aws.s3)
library(tidyverse)
library(stringi)

# liste des noms que l'on veut
villes <- c("paris","lyon","bordeaux")
fichiers <- c("reviews", "listings")

# fonction qui récupère le fichier sur Minio et le charge dans Rstudio
chargement <- function(ville,fichier){
  
  if(ville == "paris") {
    nom_fichier <- paste0("diffusion/data/", ville,"/2021-04-10/",fichier,".csv.gz")
  } else {
    nom_fichier <- paste0("diffusion/data/", ville,"/2021-04-18/",fichier,".csv.gz")
  }
  
  save_object(object=nom_fichier,
              bucket="projet-funathon",
              file=paste0(ville, "_",fichier,".csv.gz"), 
              region="")
  donnees <- read_csv(paste0(ville,"_",fichier,".csv.gz"))
}

#appel de la fonction et sauvegarde de chaque fichier dans un df séparé
map(villes,
    function(x) map(fichiers,
                    function(y) chargement(x,y)) )%>% 
  flatten() %>%
  set_names(xmap_chr(list(fichiers, villes), ~paste0(.y,"_", .x))) %>% 
  list2env(envir = .GlobalEnv)



# Création de la table qui nous intéresse: pour chaque commentaire,
# sa ville, ses coordonnées geo
dictionnaire <- tribble(~theme,~fr,~en,~de,~es,
                        "bruit","bruit|bruyant","noise|noisy|loud","Larm|laut","ruido",
                        "moustique","moustique","gnat|mosquito","Moskito","mosquito",
                        "restaurant","restaurant","restaurant","Restaurant","restaurant",
                        "voiture","voiture","car","Wagen","carro",
                        "velo","velo","bike|bicycle","Fahrrad","bicicleta",
                        "loin","loin","far","weit","lejos",
                        "cher","cher","expensive","teuer","querido"
) 

lyon_langue <- detect_language(lyon_reviews$comments) %>% tibble() %>% setNames("langue")
paris_langue <- detect_language(paris_reviews$comments) %>% tibble() %>% setNames("langue")
bordeaux_langue <- detect_language(bordeaux_reviews$comments) %>% tibble() %>% setNames("langue")

creation_base <- function(ville,mot){
  # 
  # ville="lyon"
  # mot="bruit"
  
  reviews <- get(paste0(ville,"_reviews")) #%>%  head(100)
  listings <- get(paste0(ville,"_listings"))
  langue <- get(paste0(ville,"_langue")) #%>%  head(100)
  
  frequence_mots <- reviews %>% 
    select(listing_id,comments) %>%
    bind_cols(langue) %>% 
    mutate(comments_asci=stri_trans_general(comments, "Latin-ASCII"),
           # modifier avec case_when selon la langue détectée
           indic_mot= case_when( langue =="fr" & str_detect(comments_asci,
                                                            dictionnaire %>% filter(theme==mot) %>% select(fr) %>% pull()) ~ 1,
                                 langue =="en" & str_detect(comments_asci,
                                                            dictionnaire %>% filter(theme==mot) %>% select(en) %>% pull()) ~ 1,
                                 langue =="de" & str_detect(comments_asci,
                                                            dictionnaire %>% filter(theme==mot) %>% select(de) %>% pull()) ~ 1,
                                 langue =="es" & str_detect(comments_asci,
                                                            dictionnaire %>% filter(theme==mot) %>% select(en) %>% pull()) ~ 1,
                                 TRUE ~0 ))%>% 
    count(listing_id,indic_mot) %>% 
    group_by(listing_id) %>% 
    mutate(n_comm=sum(n),
           part_mot=n/sum(n)*100) %>% 
    slice_max(order_by=indic_mot,n=1) %>% 
    mutate(part_mot=if_else(indic_mot==0,100-part_mot,part_mot),
           n_mot=if_else(is.na(n),n_comm-n,n),
           ville=ville,
           mot=mot) %>% 
    inner_join(listings %>% select(id,longitude,latitude),
               by=c("listing_id"="id")) %>% 
    ungroup() %>% 
    select(-n,-indic_mot,-listing_id) %>% 
    rename_with(~str_replace_all(.,"mot",mot))
  
  return(frequence_mots)
}

mots <- c("bruit","moustique","restaurant","voiture","velo","loin","cher")

base_analyse <- map_dfr(villes,
                        function(x) map(mots,
                                        function(y) creation_base(x,y)) %>% reduce(left_join))
