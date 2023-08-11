#### ::::::::::::::::::::::::::::::::::::::::::
####      EPCI versus pôle AAV sur Nantes
#### ::::::::::::::::::::::::::::::::::::::::::



#### Chargement de l'environnement et des bases ménages
source("R/00a_global.R",encoding = "UTF-8")
lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))


#### Chargement données faire fonctionner les fonctions ========================
qp_contours <- st_read(paste0(chemin_input,"qp/qp_franceentiere_2021.shp"),quiet = T)
qp_contours <- qp_contours %>% filter(str_sub(dep,1,2) != "97" |str_sub(dep,1,3) %in% c("972","974")) 
comQP <- fread(paste0(chemin_input,"qp/data_table_appartenance_geo_QPV_2021.csv"))

scissions <- read_excel(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
                        sheet = "Liste des scissions",skip = 5)
sfcom <- st_read(paste0(chemin_input,"com/commune_franceentiere_2022.gpkg"))
epci <- read_excel(paste0(chemin_input,"epci/Intercommunalite_Metropole_au_01-01-2019.xls"),
                   sheet = "Composition_communale",skip = 5)



#### Récupération des 53 codes EPCI ================

liste_epci <- c("200054781","200046977","200054807","245900410","243100518","243300316",
  "244400404","246700488","243400017","243500139","200040715","200023414",
  "200030195","248300543","243700754","245400676","246300701","244200770",
  "200065597","244500468","244900015","242100410","200066009","200027183",
  "200039915","242900314","200039865","247200132","248000531","200067213",
  "243000643","245901160","200084952","248400251","248719312","246200364",
  "249720061","249740119","200066793","200067254","242500361","200069854",
  "200067106","245900428","200068781","200069110","241700434","200068120",
  "200042174","200069250","244400644","200069409","249740077")

names(liste_epci) <- liste_epci
liste_epci <- rev(liste_epci) # Pour commencer les traitements par les petites EPCI


# Passage en géographie 2022 (uniquement pour les fusions)
com_53epci <- epci %>% filter(EPCI %in% liste_epci)

tabCogHisto <-
  read_excel(
    paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
    sheet = "Table de passage",
    skip = 5)
tabCogHisto <- tabCogHisto %>% 
  filter(CODGEO_INI!=CODGEO_2022) %>% 
  select(CODGEO_INI,CODGEO_2022)

codgeo_acoriger <- com_53epci %>% filter(CODGEO %in% tabCogHisto$CODGEO_INI)
com_53epci <- com_53epci %>% 
  left_join(tabCogHisto,by=c("CODGEO"="CODGEO_INI")) %>% 
  mutate(CODGEO=if_else(is.na(CODGEO_2022),CODGEO,CODGEO_2022)) %>% 
  select(!CODGEO_2022)


#### Contour EPCI - intersections avec QP ===========

sf_com_epci <- sfcom %>% filter(code %in% com_53epci$CODGEO)
sf_com_epci <- sf_com_epci %>% left_join(com_53epci %>% select(CODGEO,EPCI),by=c("code"="CODGEO"))

sf_epci <- sf_com_epci %>% group_by(EPCI) %>% summarize(geometry=st_union(geometry))


# 16mn

sfqp_epci_inters <- sf_epci %>% 
        st_intersection(qp_contours %>% select(code,geometry))%>% 
  rename(aav20=EPCI) #leurre 

# l_sfqp_epci_inters %>% map(
#   ~.x %>% setnames(old="EPCI",new="AAV2020") #leurre pour les fonctions infra
# ) 

# l_sfqp_epci_inters <- l_sf_epci %>% 
#   map(~.x %>% 
#         st_intersection(qp_contours %>% select(code,geometry))
#       ) 
# 
# l_sfqp_epci_inters %>% map(
#   ~.x %>% setnames(old="EPCI",new="AAV2020") #leurre pour les fonctions infra
#   ) 

#### Filtrage ménages des 53 EPCI =======================

lmen_53epci <- lmen %>% map(~.x[depcom %in% unique(com_53epci$CODGEO)])



#### Entrée dans la boucle -----------------------------------------------
#### ---------------------------------------------------------------------

# epci_cible <- "249740077"
# liste_taille_carreaux <- list('200'=200,'1000'=1000)
liste_taille_carreaux <- list(200,300,500,1000,2000)
names(liste_taille_carreaux) <- liste_taille_carreaux

# Liste des EPCI sans QP : uniquement Annecy
#l_sfqp_epci_inters %>% map_lgl(~.x %>% nrow == 0) %>% which()


# liste_epci_test <- liste_epci[1:3]
# 
# deb <- Sys.time()
# cl <- makeCluster(15)
# clusterEvalQ(cl, source("R/00a_global.R"))
# clusterExport(cl, "com_53epci")
# clusterExport(cl, "lmen_53epci")
# clusterExport(cl, "sfqp_epci_inters")
# 
# 
# lmen_poles <- parLapply(cl,
#                         liste_epci_test,
#                         prod_epci) 
# stopCluster(cl)
# fin <- Sys.time()-deb
# 
# 
# 
# 
# prod_epci <- function(epci_cible){
#   print(epci_cible)
#   names(liste_taille_carreaux) <- liste_taille_carreaux
#   
#   # Sélection des ménages de l'EPCI
#   com_cible <- com_53epci %>% filter(EPCI == epci_cible) %>% pull(CODGEO)
#   lmen_epci <- lmen_53epci %>% map(~.x[depcom %in% unique(com_cible)])
#   
#   
#   #### Carroyage et résultats finaux   =====================================
#   
#   lmen_epci <- lapply(lmen_epci,
#                       f_inspire_filtre,
#                       liste_taille_carreaux,
#                       com_53epci,
#                       nbmen_min,
#                       varCom="CODGEO",varCodeZe="EPCI") 
#   
#   res_epci <- lmen_epci %>% 
#     map(
#       ~liste_taille_carreaux %>% 
#         lapply(function(y){
#           f_carroyage(y,
#                       .x,
#                       nb_tranches,
#                       passageComZE = com_53epci,
#                       ZE_ssQP=c("200066793"),
#                       sfqp_pole_inters = sfqp_epci_inters[sfqp_epci_inters$aav20 == epci_cible,],
#                       varCom="CODGEO",varCodeZe="EPCI"
#           )
#         }
#         )
#     )
#   return(res_epci)
#   # saveRDS(res_epci,paste0(chemin_output,"lcouches_epci_",epci_cible,".rds"))
#   
# }








for(epci_cible in liste_epci){
  print(epci_cible)
  
  # Sélection des ménages de l'EPCI
  com_cible <- com_53epci %>% filter(EPCI == epci_cible) %>% pull(CODGEO)
  lmen_epci <- lmen_53epci %>% map(~.x[depcom %in% unique(com_cible)])
  
  
  #### Carroyage et résultats finaux   =====================================
  
  lmen_epci <- lapply(lmen_epci,
                      f_inspire_filtre,
                      liste_taille_carreaux,
                      com_53epci,
                      nbmen_min,
                      varCom="CODGEO",varCodeZe="EPCI") 
  
  res_epci <- lmen_epci %>% 
    map(
      ~liste_taille_carreaux %>% 
        lapply(function(y){
          f_carroyage(y,
                      .x,
                      nb_tranches,
                      passageComZE = com_53epci,
                      ZE_ssQP=c("200066793"),
                      #sfqp_epci_inters[sfqp_epci_inters$EPCI == epci_cible,],
                      sfqp_pole_inters = sfqp_epci_inters[sfqp_epci_inters$aav20 == epci_cible,],
                      varCom="CODGEO",varCodeZe="EPCI"
                      )
        }
        )
    )
  
  saveRDS(res_epci,paste0(chemin_output,"lcouches_epci_",epci_cible,".rds"))
}





