# ::::::::::::::::::::::::::::::::::::::::::::::::::
#          G√©n√©ration des couches carroy√©es
# ::::::::::::::::::::::::::::::::::::::::::::::::::

#' Cr√©ation des donn√©es-r√©sultats de l'investissement
#' Passage des bases m√©nages √† des couches de carreaux et d'indicateurs de s√©gr√©gation
#' Cr√©ation des couches carroy√©es pour la cartographie

source("R/00a_global.R")

#### *********************************************
####            Importation inputs           -----
#### *********************************************

#### Base menages corrig√©es ===========
lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))
#10min

#Restriction au Grand Est
dep_valid_met <- c(paste0("0",8),10,51,52,54,55,57,67,68,88)
dep_valid_dom <- c()
lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met])


#A commenter si inutile ‡ l'Èchelle rÈgionale
### Contours QP ========================
qp_contours <- st_read(paste0(chemin_input,"qp/qp_franceentiere_2021.shp"),quiet = T)
qp_contours <- qp_contours %>% filter(str_sub(dep,1,2) != "97" |str_sub(dep,1,3) %in% c("972","974"))
comQP <- fread(paste0(chemin_input,"qp/data_table_appartenance_geo_QPV_2021.csv"))

#### ZAAV ========================
com_zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                       sheet = "Composition_communale",skip = 5)
zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                   sheet = "AAV2020",skip = 5)
scissions <- read_excel(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
                        sheet = "Liste des scissions",skip = 5)
sfcom <- st_read(paste0(chemin_input,"com/commune_franceentiere_2022.gpkg"))

#### EPCI ========================
com_epci <- read_excel(paste0(chemin_input,"epci/Intercommunalite_Metropole_au_01-01-2019.xls"),
                       sheet = "Composition_communale",skip = 5)
epci <- read_excel(paste0(chemin_input,"epci/Intercommunalite_Metropole_au_01-01-2019.xls"),
                   sheet = "EPCI",skip = 5)

###RÈgion 
com_region <- com_epci %>% filter(REG %in% c("44")) %>% subset(select = -c(EPCI, LIBEPCI))
region <- read.csv("U:/AU33/v_region_2023.csv") %>% filter(REG %in% c("44"))

#### ****************************************************
####      Definition des p√¥les des grandes AAV       ----
#### ****************************************************

###Doute sur Seuil TAAV2017>=3 !

#### Selection +50 000 hab en metropole ou martinique ou Reunion ===========
zaav <- zaav %>% 
  #filter(TAAV2017>=3) %>%
  filter(TAAV2017>=1) %>%
  filter(!str_detect(AAV2020,"\\D") | AAV2020 %in% c("9B1","9D1","9D2"))

#Ajout Antoine EPCI:
epci <- epci %>% 
  filter(! EPCI %in% c("ZZZZZZZZZ"))

epci_me <- epci %>% 
  filter(NATURE_EPCI %in% c("ME"))
epci_cu <- epci %>% 
  filter(NATURE_EPCI %in% c("CU"))
epci_ca <- epci %>% 
  filter(NATURE_EPCI %in% c("CA"))
epci_cc <- epci %>% 
  filter(NATURE_EPCI %in% c("CC"))

com_poles <- com_zaav %>% filter(AAV2020 %in% zaav$AAV2020 & CATEAAV2020 %in% c("11","12"))

#Ajout Antoine
com_poles_secondaires <- com_zaav %>% filter(AAV2020 %in% zaav$AAV2020 & CATEAAV2020 %in% c("13"))
com_couronnes <- com_zaav %>% filter(AAV2020 %in% zaav$AAV2020 & CATEAAV2020 %in% c("20"))
com_hors_zaav <- com_zaav %>% filter(CATEAAV2020 %in% c("30"))

#1681 poles
#25 poles secondaires
#23 858 couronnes
#8928 hors ZAAV


#Ajout EPCI:
com_epci_me <- com_epci %>% filter(EPCI %in% epci_me$EPCI)
com_epci_cu <- com_epci %>% filter(EPCI %in% epci_cu$EPCI)
com_epci_ca <- com_epci %>% filter(EPCI %in% epci_ca$EPCI)
com_epci_cc <- com_epci %>% filter(EPCI %in% epci_cc$EPCI)

#963 ME
#589 CU
#7488 CA
#25926 CC

#A commenter si inutile ‡ l'Èchelle rÈgionale
### D√©tection des p√¥les sans QP (geographique) =======
sf_com_poles <- sfcom %>% filter(code %in% com_poles$CODGEO)
sf_pole <- sf_com_poles  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))

#Ajout Antoine
sf_com_poles_secondaires <- sfcom %>% filter(code %in% com_poles_secondaires$CODGEO)
sf_pole_secondaires <- sf_com_poles_secondaires  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))

sf_com_couronnes <- sfcom %>% filter(code %in% com_couronnes$CODGEO)
sf_couronnes <- sf_com_couronnes  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))

sf_com_hors_zaav <- sfcom %>% filter(code %in% com_hors_zaav$CODGEO)
sf_hors_zaav <- sf_com_hors_zaav  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))
#Ajout EPCI
sf_com_epci_me <- sfcom %>% filter(code %in% com_epci_me$CODGEO)
sf_epci_me <- sf_com_epci_me  %>% group_by(epc) %>% summarize(geometry=st_union(geometry))

sf_com_epci_cu <- sfcom %>% filter(code %in% com_epci_cu$CODGEO)
sf_epci_cu <- sf_com_epci_cu  %>% group_by(epc) %>% summarize(geometry=st_union(geometry))

sf_com_epci_ca <- sfcom %>% filter(code %in% com_epci_ca$CODGEO)
sf_epci_ca <- sf_com_epci_ca  %>% group_by(epc) %>% summarize(geometry=st_union(geometry))

sf_com_epci_cc <- sfcom %>% filter(code %in% com_epci_cc$CODGEO)
sf_epci_cc <- sf_com_epci_cc  %>% group_by(epc) %>% summarize(geometry=st_union(geometry))

sf_com_region <- sfcom %>% filter(code %in% com_region$CODGEO)
sf_region <- sf_com_region  %>% group_by(reg) %>% summarize(geometry=st_union(geometry))



#Reste de QP/Hors-QP
pole_ssQP <- st_join(sf_pole,qp_contours,left=T) %>% filter(is.na(code)) %>% pull(aav20)
com_pole_ssQP <- com_poles %>% filter(AAV2020%in%pole_ssQP) %>% pull(CODGEO)

#A commenter si inutile ‡ l'Èchelle rÈgionale
#QP √† cheval sur pole ?
# sfqp_pole_inters <- sf_pole %>% st_intersection(qp_contours %>% select(code,geometry))

#### Check absence communes scissionn√©es dans les grands p√¥les ================
com_poles[com_poles$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
# => uniquement Saint-Gaudens/ Marmande/ BÈthune/ Bois-Guillaume / Sens 
# Aucun souci car aucune dans le Grand-Est
com_poles_secondaires[com_poles_secondaires$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#0 scission
com_couronnes[com_couronnes$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#ProblËme avec Autreville-sur-la-Renne/ Loisey / RÈcicourt / Kirrwiller
com_hors_zaav[com_hors_zaav$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#ProlËme avec Ch‚tillon-sur-Marne / Val-de-Meuse / Nully / Varennes-sur-Amance

#Check EPCI :
com_epci_me[com_epci_me$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#Aucun problËme
com_epci_cu[com_epci_cu$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#Aucun problËme
com_epci_ca[com_epci_ca$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#ProblËme avec Loisey
com_epci_cc[com_epci_cc$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#ProblËme avec : Ch‚tillon-sur-Marne / Autreville-sur-la-Renne / Val-de-Meuse / Nully / Varennes-sur-Amance / RÈcicourt / Kirrwiller

#Check Region
com_region[com_region$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#ProblËme avec : Ch‚tillon-sur-Marne / Autreville-sur-la-Renne / Val-de-Meuse / Nully / Varennes-sur-Amance / RÈcicourt / Kirrwiller / Loisey

#### *****************************************************************
####            Filtrage m√©nages des p√¥les par les cog           =====
#### *****************************************************************

filtre_par_pole <- function(dtmen,lcog){
  lcog %>% map(~dtmen[depcom %in% unique(.x$CODGEO)])
}

lpole_cog <- split(com_poles,com_poles$AAV2020)
lmen_poles <- lmen %>% map(filtre_par_pole,lpole_cog) # 1mn
lmen_poles <- lmen_poles %>% unlist(recursive = F)

#Ajout Antoine
lpole_secondaires_cog <- split(com_poles_secondaires,com_poles_secondaires$AAV2020)
lmen_poles_secondaires <- lmen %>% map(filtre_par_pole,lpole_secondaires_cog) # 1mn
lmen_poles_secondaires <- lmen_poles_secondaires %>% unlist(recursive = F)

lcouronnes_cog <- split(com_couronnes,com_couronnes$AAV2020)
lmen_couronnes <- lmen %>% map(filtre_par_pole,lcouronnes_cog) # 1mn
lmen_couronnes <- lmen_couronnes %>% unlist(recursive = F)

lhors_zaav_cog <- split(com_hors_zaav,com_hors_zaav$AAV2020)
lmen_hors_zaav <- lmen %>% map(filtre_par_pole,lhors_zaav_cog) # 1mn
lmen_hors_zaav <- lmen_hors_zaav %>% unlist(recursive = F)

# Ajout EPCI:
lepci_me_cog <- split(com_epci_me,com_epci_me$EPCI)
lmen_epci_me <- lmen %>% map(filtre_par_pole,lepci_me_cog) # 1mn
lmen_epci_me <- lmen_epci_me %>% unlist(recursive = F)

lepci_cu_cog <- split(com_epci_cu,com_epci_cu$EPCI)
lmen_epci_cu <- lmen %>% map(filtre_par_pole,lepci_cu_cog) # 1mn
lmen_epci_cu <- lmen_epci_cu %>% unlist(recursive = F)

lepci_ca_cog <- split(com_epci_ca,com_epci_ca$EPCI)
lmen_epci_ca <- lmen %>% map(filtre_par_pole,lepci_ca_cog) # 1mn
lmen_epci_ca <- lmen_epci_ca %>% unlist(recursive = F)

lepci_cc_cog <- split(com_epci_cc,com_epci_cc$EPCI)
lmen_epci_cc <- lmen %>% map(filtre_par_pole,lepci_cc_cog) # 1mn
lmen_epci_cc <- lmen_epci_cc %>% unlist(recursive = F)

#Ajout Region
lregion_cog <- split(com_region,com_region$REG)
lmen_region <- lmen %>% map(filtre_par_pole,lregion_cog) # 1mn
lmen_region <- lmen_region %>% unlist(recursive = F)

# rm(lmen);gc()




#### *****************************************************************
####                          Carroyage                          =====
#### *****************************************************************

#### Association menage-idInspire + filtrage  ===================


deb <- Sys.time()
cl <- makeCluster(15)
clusterEvalQ(cl, source("R/00a_global.R"))
lmen_poles <- parLapply(cl,
                        lmen_poles,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_poles,nbmen_min)


#Ajout Antoine
#Poles secondaires
lmen_poles_secondaires <- parLapply(cl,
                        lmen_poles_secondaires,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_poles_secondaires,nbmen_min)

lmen_couronnes <- parLapply(cl,
                        lmen_couronnes,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_couronnes,nbmen_min) 

lmen_hors_zaav <- parLapply(cl,
                        lmen_hors_zaav,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_hors_zaav,nbmen_min) 

#Ajout EPCI:
lmen_epci_me <- parLapply(cl,
                        lmen_epci_me,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_epci_me,nbmen_min,"CODGEO", varCodeZe="EPCI")

lmen_epci_cu <- parLapply(cl,
                        lmen_epci_cu,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_epci_cu,nbmen_min,"CODGEO", varCodeZe="EPCI")

lmen_epci_ca <- parLapply(cl,
                        lmen_epci_ca,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_epci_ca,nbmen_min,"CODGEO", varCodeZe="EPCI")

lmen_epci_cc <- parLapply(cl,
                        lmen_epci_cc,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_epci_cc,nbmen_min,"CODGEO", varCodeZe="EPCI")

#Ajout Region
lmen_region <- parLapply(cl,
                        lmen_region,
                        f_inspire_filtre,
                        liste_taille_carreaux,com_region,nbmen_min,"CODGEO", varCodeZe="REG")
stopCluster(cl)
fin <- Sys.time()-deb
fin


# Pour les DOM avant 2014
lmen_poles[! lmen_poles %>% map_lgl(~.x %>% is.data.table)] <- NULL
#Ajout Antoine
lmen_poles_secondaires[! lmen_poles_secondaires %>% map_lgl(~.x %>% is.data.table)] <- NULL
lmen_couronnes[! lmen_couronnes %>% map_lgl(~.x %>% is.data.table)] <- NULL
lmen_hors_zaav[! lmen_hors_zaav %>% map_lgl(~.x %>% is.data.table)] <- NULL

#Ajout EPCI
lmen_epci_me[! lmen_epci_me %>% map_lgl(~.x %>% is.data.table)] <- NULL
lmen_epci_cu[! lmen_epci_cu %>% map_lgl(~.x %>% is.data.table)] <- NULL
lmen_epci_ca[! lmen_epci_ca %>% map_lgl(~.x %>% is.data.table)] <- NULL
lmen_epci_cc[! lmen_epci_cc %>% map_lgl(~.x %>% is.data.table)] <- NULL

#Ajout Region
lmen_region[! lmen_region %>% map_lgl(~.x %>% is.data.table)] <- NULL


####   Carroyage  ===================

l_tcar_named <- purrr::set_names(liste_taille_carreaux,liste_taille_carreaux)

# #Modification Antoine:
# pole_ssQP <- com_poles
sfqp_pole_inters <- NULL
deb <- Sys.time()
cl <- makeCluster(20)
clusterExport(cl, "l_tcar_named")
clusterExport(cl, "com_poles")
# clusterExport(cl, "com_poles_secondaires")
# clusterExport(cl, "com_couronnes")
# clusterExport(cl, "com_hors_zaav")
# clusterExport(cl, "com_epci_me")
# clusterExport(cl, "com_epci_cu")
# clusterExport(cl, "com_epci_ca")
# clusterExport(cl, "com_epci_cc")
clusterExport(cl, "com_region")
clusterExport(cl, "pole_ssQP")
clusterExport(cl, "sfqp_pole_inters")
clusterEvalQ(cl, source("R/00a_global.R"))
lmen_poles <- parLapply(cl,lmen_poles,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_poles,ZE_ssQP=pole_ssQP,sfqp_pole_inters)
})

#Ajout Antoine
lmen_poles_secondaires <- parLapply(cl,lmen_poles_secondaires,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_poles_secondaires,ZE_ssQP=pole_ssQP,sfqp_pole_inters)
})

lmen_couronnes <- parLapply(cl,lmen_couronnes,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_couronnes,ZE_ssQP=pole_ssQP,sfqp_pole_inters)
})

lmen_hors_zaav <- parLapply(cl,lmen_hors_zaav,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_hors_zaav,ZE_ssQP=pole_ssQP,sfqp_pole_inters)
})

#Ajout EPCI
lmen_epci_me <- parLapply(cl,lmen_epci_me,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_epci_me,ZE_ssQP=pole_ssQP,sfqp_pole_inters,"CODGEO", varCodeZe="EPCI")
})

lmen_epci_cu <- parLapply(cl,lmen_epci_cu,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_epci_cu,ZE_ssQP=pole_ssQP,sfqp_pole_inters,"CODGEO", varCodeZe="EPCI")
})

lmen_epci_ca <- parLapply(cl,lmen_epci_ca,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_epci_ca,ZE_ssQP=pole_ssQP,sfqp_pole_inters,"CODGEO", varCodeZe="EPCI")
})

lmen_epci_cc <- parLapply(cl,lmen_epci_cc,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_epci_cc,ZE_ssQP=pole_ssQP,sfqp_pole_inters,"CODGEO", varCodeZe="EPCI")
})

#Ajout Region
lmen_region <- parLapply(cl,lmen_region,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_region,ZE_ssQP=pole_ssQP,sfqp_pole_inters,"CODGEO", varCodeZe="REG")
})

stopCluster(cl)
Sys.time()-deb #10min

lcouches <- lmen_poles %>% restructure()
#Ajout Antoine
lcouches_secondaires <- lmen_poles_secondaires %>% restructure()
lcouches_couronnes <- lmen_couronnes %>% restructure()
lcouches_hors_zaav <- lmen_hors_zaav %>% restructure()

#Ajout EPCI
lcouches_epci_me <- lmen_epci_me %>% restructure()
lcouches_epci_cu <- lmen_epci_cu %>% restructure()
lcouches_epci_ca <- lmen_epci_ca %>% restructure()
lcouches_epci_cc <- lmen_epci_cc %>% restructure()

#Ajout Region
lcouches_region <- lmen_region %>% restructure()

# # Sauvegarde de la liste des couches
# saveRDS(lcouches,file = paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))




#### ***********************************************************************
####    Pr√©paration des couches carroy√©es pour cartographie (lissage)  =====
#### ***********************************************************************

#' Cr√©ation des couches carroy√©es en input de l'application de cartogrpahie
#' Pour chaque version (annee-ville-taille des carreaux) : 
#' - On cr√©√© une variable de contribution du carreau √† la s√©gr√©gation totale
#' - On lisse la s√©gr√©gation et la contribution des carreaux (en lissant pr√©alablement les populations des diff√©rents groupes sociaux et en recalculant proprement les indicateurs sur les populations liss√©es) 
#' - On discr√©tise les carreaux par niveaux de revenus et on associe une couleur pour la carto 
#' - Sauvegarde 


# lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_car <- lcouches %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

#Ajout Antoine:
# lcouches_secondaires <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_secondaires_car <- lcouches_secondaires %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

# lcouches_couronnes <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_couronnes_car <- lcouches_couronnes %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

# lcouches_hors_zaav <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_hors_zaav_car <- lcouches_hors_zaav %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

#Ajout EPCI:
# lcouches_epci_me <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_epci_me_car <- lcouches_epci_me %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

# lcouches_epci_cu <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_epci_cu_car <- lcouches_epci_cu %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

# lcouches_epci_ca <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_epci_ca_car <- lcouches_epci_ca %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

# lcouches_epci_cc <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_epci_cc_car <- lcouches_epci_cc %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])

#Ajout Region
# lcouches_region <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
lcouches_region_car <- lcouches_region %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])


deb <- Sys.time()
cl <- makeCluster(15)
clusterExport(cl, "nb_tranches")
clusterEvalQ(cl, source("R/00a_global.R"))
lcentro_smooth <-  parLapply(cl,lcouches_car,fLissage,nb_tranches=nb_tranches)
#Ajout Antoine
lcentro_secondaires_smooth <-  parLapply(cl,lcouches_secondaires_car,fLissage,nb_tranches=nb_tranches)
lcentro_couronnes_smooth <-  parLapply(cl,lcouches_couronnes_car,fLissage,nb_tranches=nb_tranches)
lcentro_hors_zaav_smooth <-  parLapply(cl,lcouches_hors_zaav_car,fLissage,nb_tranches=nb_tranches)
#Ajout EPCI
lcentro_epci_me_smooth <-  parLapply(cl,lcouches_epci_me_car,fLissage,nb_tranches=nb_tranches)
lcentro_epci_cu_smooth <-  parLapply(cl,lcouches_epci_cu_car,fLissage,nb_tranches=nb_tranches)
lcentro_epci_ca_smooth <-  parLapply(cl,lcouches_epci_ca_car,fLissage,nb_tranches=nb_tranches)
lcentro_epci_cc_smooth <-  parLapply(cl,lcouches_epci_cc_car,fLissage,nb_tranches=nb_tranches)

#Ajout Region
lcentro_region_smooth <-  parLapply(cl,lcouches_region_car,fLissage,nb_tranches=nb_tranches)

stopCluster(cl)
duree_parLapply <- Sys.time()-deb #3mn

# saveRDS(lcentro_smooth,paste0(chemin_output,"centro_smooth/lcentro_smooth.rds"))





#### Sauvegarde 1 fichier par ville*taille_carreau (pour Shiny) =============

#lcentro_smooth <- readRDS(paste0(chemin_output,"centro_smooth/lcentro_smooth.rds"))

##############################Poles principaux
lcentro_smooth <- lcentro_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


bind_rows_maison <- function(...){bind_rows(...,.id="version")} 
df_centro_smooth <- do.call(bind_rows_maison,lcentro_smooth)

df_centro_smooth <- df_centro_smooth %>% mutate(annee=str_sub(version,4,5),
                                                aav=str_sub(version,7,9),
                                                tcar=str_extract(version,"\\d+$"))
df_centro_smooth$version <- NULL

l.aav <- df_centro_smooth$aav %>% unique()
l.tcar <- df_centro_smooth$tcar %>% unique()
l=expand.grid(aav=l.aav,tcar=l.tcar,stringsAsFactors = F)
l <- split(l, seq(nrow(l)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_smooth %>% 
#     filter(aav==tab$aav & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l %>% lapply(save_centro)

#######################Poles secondaires
lcentro_secondaires_smooth <- lcentro_secondaires_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


bind_rows_maison <- function(...){bind_rows(...,.id="version")} 
df_centro_secondaires_smooth <- do.call(bind_rows_maison,lcentro_secondaires_smooth)

df_centro_secondaires_smooth <- df_centro_secondaires_smooth %>% mutate(annee=str_sub(version,4,5),
                                                aav=str_sub(version,7,9),
                                                tcar=str_extract(version,"\\d+$"))
df_centro_secondaires_smooth$version <- NULL

l_secondaires.aav <- df_centro_secondaires_smooth$aav %>% unique()
l_secondaires.tcar <- df_centro_secondaires_smooth$tcar %>% unique()
l_secondaires=expand.grid(aav=l_secondaires.aav,tcar=l_secondaires.tcar,stringsAsFactors = F)
l_secondaires <- split(l_secondaires, seq(nrow(l_secondaires)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_secondaires_smooth %>% 
#     filter(aav==tab$aav & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_secondaires %>% lapply(save_centro)

#######################Couronnes
lcentro_couronnes_smooth <- lcentro_couronnes_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_couronnes_smooth <- do.call(bind_rows_maison,lcentro_couronnes_smooth)

df_centro_couronnes_smooth <- df_centro_couronnes_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                       aav=str_sub(version,7,9),
                                                                       tcar=str_extract(version,"\\d+$"))
df_centro_couronnes_smooth$version <- NULL

l_couronnes.aav <- df_centro_couronnes_smooth$aav %>% unique()
l_couronnes.tcar <- df_centro_couronnes_smooth$tcar %>% unique()
l_couronnes=expand.grid(aav=l_couronnes.aav,tcar=l_couronnes.tcar,stringsAsFactors = F)
l_couronnes <- split(l_couronnes, seq(nrow(l_couronnes)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_couronnes_smooth %>% 
#     filter(aav==tab$aav & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_couronnes %>% lapply(save_centro)

#######################Hors ZAAV
lcentro_hors_zaav_smooth <- lcentro_hors_zaav_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_hors_zaav_smooth <- do.call(bind_rows_maison,lcentro_hors_zaav_smooth)

df_centro_hors_zaav_smooth <- df_centro_hors_zaav_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                    aav=str_sub(version,7,9),
                                                                    tcar=str_extract(version,"\\d+$"))
df_centro_hors_zaav_smooth$version <- NULL

l_hors_zaav.aav <- df_centro_hors_zaav_smooth$aav %>% unique()
l_hors_zaav.tcar <- df_centro_hors_zaav_smooth$tcar %>% unique()
l_hors_zaav=expand.grid(aav=l_hors_zaav.aav,tcar=l_hors_zaav.tcar,stringsAsFactors = F)
l_hors_zaav <- split(l_hors_zaav, seq(nrow(l_hors_zaav)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_hors_zaav_smooth %>% 
#     filter(aav==tab$aav & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_hors_zaav %>% lapply(save_centro)

#######################EPCI ME
lcentro_epci_me_smooth <- lcentro_epci_me_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_epci_me_smooth <- do.call(bind_rows_maison,lcentro_epci_me_smooth)

df_centro_epci_me_smooth <- df_centro_epci_me_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                    epci=str_sub(version,7,9),
                                                                    tcar=str_extract(version,"\\d+$"))
df_centro_epci_me_smooth$version <- NULL

l_epci_me.epci <- df_centro_epci_me_smooth$epci %>% unique()
l_epci_me.tcar <- df_centro_epci_me_smooth$tcar %>% unique()
l_epci_me=expand.grid(epci=l_epci_me.epci,tcar=l_epci_me.tcar,stringsAsFactors = F)
l_epci_me <- split(l_epci_me, seq(nrow(l_epci_me)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_epci_me_smooth %>% 
#     filter(epci==tab$epci & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_epci_me %>% lapply(save_centro)

#######################EPCI CU
lcentro_epci_cu_smooth <- lcentro_epci_cu_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_epci_cu_smooth <- do.call(bind_rows_maison,lcentro_epci_cu_smooth)

df_centro_epci_cu_smooth <- df_centro_epci_cu_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                epci=str_sub(version,7,9),
                                                                tcar=str_extract(version,"\\d+$"))
df_centro_epci_cu_smooth$version <- NULL

l_epci_cu.epci <- df_centro_epci_cu_smooth$epci %>% unique()
l_epci_cu.tcar <- df_centro_epci_cu_smooth$tcar %>% unique()
l_epci_cu=expand.grid(epci=l_epci_cu.epci,tcar=l_epci_cu.tcar,stringsAsFactors = F)
l_epci_cu <- split(l_epci_cu, seq(nrow(l_epci_cu)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_epci_cu_smooth %>% 
#     filter(epci==tab$epci & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_epci_cu %>% lapply(save_centro)

#######################EPCI CA
lcentro_epci_ca_smooth <- lcentro_epci_ca_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_epci_ca_smooth <- do.call(bind_rows_maison,lcentro_epci_ca_smooth)

df_centro_epci_ca_smooth <- df_centro_epci_ca_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                epci=str_sub(version,7,9),
                                                                tcar=str_extract(version,"\\d+$"))
df_centro_epci_ca_smooth$version <- NULL

l_epci_ca.epci <- df_centro_epci_ca_smooth$epci %>% unique()
l_epci_ca.tcar <- df_centro_epci_ca_smooth$tcar %>% unique()
l_epci_ca=expand.grid(epci=l_epci_ca.epci,tcar=l_epci_ca.tcar,stringsAsFactors = F)
l_epci_ca <- split(l_epci_ca, seq(nrow(l_epci_ca)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_epci_ca_smooth %>% 
#     filter(epci==tab$epci & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_epci_ca %>% lapply(save_centro)

#######################EPCI CC
lcentro_epci_cc_smooth <- lcentro_epci_cc_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


df_centro_epci_cc_smooth <- do.call(bind_rows_maison,lcentro_epci_cc_smooth)

df_centro_epci_cc_smooth <- df_centro_epci_cc_smooth %>% mutate(annee=str_sub(version,4,5),
                                                                epci=str_sub(version,7,9),
                                                                tcar=str_extract(version,"\\d+$"))
df_centro_epci_cc_smooth$version <- NULL

l_epci_cc.epci <- df_centro_epci_cc_smooth$epci %>% unique()
l_epci_cc.tcar <- df_centro_epci_cc_smooth$tcar %>% unique()
l_epci_cc=expand.grid(epci=l_epci_cc.epci,tcar=l_epci_cc.tcar,stringsAsFactors = F)
l_epci_cc <- split(l_epci_cc, seq(nrow(l_epci_cc)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_epci_cc_smooth %>% 
#     filter(epci==tab$epci & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l_epci_cc %>% lapply(save_centro)

##############################Region
lcentro_region_smooth <- lcentro_region_smooth %>% map(
  ~ .x %>%
    st_drop_geometry() %>%
    dplyr::select(
      idInspire,
      nbmen_carreau,
      segregStandar,
      contribSegreg,
      segregStandar_liss,
      contribSegreg_liss,
      median_carreau,
      segregStandar_lab,
      contribSegreg_lab,
      segregStandar_liss_lab,
      contribSegreg_liss_lab,
      median_carreau_lab,
      couleur,
      long_hg,
      lat_hg
    )
)


bind_rows_maison <- function(...){bind_rows(...,.id="version")} 
df_centro_region_smooth <- do.call(bind_rows_maison,lcentro_region_smooth)

df_centro_region_smooth <- df_centro_region_smooth %>% mutate(annee=str_sub(version,4,5),
                                                reg=str_sub(version,7,9),
                                                tcar=str_extract(version,"\\d+$"))
df_centro_region_smooth$version <- NULL

l_region.reg <- df_centro_region_smooth$reg %>% unique()
l_region.tcar <- df_centro_region_smooth$tcar %>% unique()
l_region=expand.grid(reg=l_region.reg,tcar=l_region.tcar,stringsAsFactors = F)
l_region <- split(l_region, seq(nrow(l_region)))

# save_centro <- function(tab){#nom.aav <- "010"
#   df_centro_smooth %>% 
#     filter(aav==tab$aav & tcar==tab$tcar) %>% 
#     saveRDS(paste0(chemin_output,"carto3d/",tab$aav,"_",tab$tcar,".rds"))
# }
# l %>% lapply(save_centro)




#### Sauvegarde des centro√Ødes de p√¥les pour les cartes =========

lgeom <- split(sf_pole$geometry,seq_along(sf_pole$geometry))
names(lgeom) <- sf_pole$aav20
centroVilles <- lgeom %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

#Ajout Antoine
lgeom_secondaires <- split(sf_pole_secondaires$geometry,seq_along(sf_pole_secondaires$geometry))
names(lgeom_secondaires) <- sf_pole_secondaires$aav20
centroVilles_secondaires <- lgeom_secondaires %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

lgeom_couronnes <- split(sf_couronnes$geometry,seq_along(sf_couronnes$geometry))
names(lgeom_couronnes) <- sf_couronnes$aav20
centroVilles_couronnes <- lgeom_couronnes %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

lgeom_hors_zaav <- split(sf_hors_zaav$geometry,seq_along(sf_hors_zaav$geometry))
names(lgeom_hors_zaav) <- sf_hors_zaav$aav20
centroVilles_hors_zaav <- lgeom_hors_zaav %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

#Ajout EPCI
lgeom_epci_me <- split(sf_epci_me$geometry,seq_along(sf_epci_me$geometry))
names(lgeom_epci_me) <- sf_epci_me$epc
centroVilles_epci_me <- lgeom_epci_me %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

lgeom_epci_cu <- split(sf_epci_cu$geometry,seq_along(sf_epci_cu$geometry))
names(lgeom_epci_cu) <- sf_epci_cu$epc
centroVilles_epci_cu <- lgeom_epci_cu %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

lgeom_epci_ca <- split(sf_epci_ca$geometry,seq_along(sf_epci_ca$geometry))
names(lgeom_epci_ca) <- sf_epci_ca$epc
centroVilles_epci_ca <- lgeom_epci_ca %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

lgeom_epci_cc <- split(sf_epci_cc$geometry,seq_along(sf_epci_cc$geometry))
names(lgeom_epci_cc) <- sf_epci_cc$epc
centroVilles_epci_cc <- lgeom_epci_cc %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))

#Ajout region
lgeom_region <- split(sf_region$geometry,seq_along(sf_region$geometry))
names(lgeom_region) <- sf_region$reg
centroVilles_region <- lgeom_region %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))


# saveRDS(centroVilles,paste0(chemin_output,"carto3d/centroVilles.rds"))



# lgeom <- split(sf_pole$geometry,seq_along(sf_pole$geometry)) 
# names(lgeom) <- sf_pole$aav20
diamVilles <- 
  lgeom %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10}) 

#Ajout Antoine
diamVilles_secondaires <- 
  lgeom_secondaires %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10}) 

diamVilles_couronnes <- 
  lgeom_couronnes %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10}) 

diamVilles_hors_zaav <- 
  lgeom_hors_zaav %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10}) 

#Ajout EPCI
diamVilles_epci_me <- 
  lgeom_epci_me %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

diamVilles_epci_cu <- 
  lgeom_epci_cu %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

diamVilles_epci_ca <- 
  lgeom_epci_ca %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

diamVilles_epci_cc <- 
  lgeom_epci_cc %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

#Ajout Region
diamVilles_region <- 
  lgeom_region %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

# saveRDS(diamVilles,paste0(chemin_output,"carto3d/diamVilles.rds"))







# *************************************************************
#    Liste nomm√©e des p√¥les pour les input des appli Shiny  ----
# *************************************************************

linputVille <- as.list(zaav$AAV2020) %>% set_names(zaav$LIBAAV2020)
#Ajout Antoine:
linputEPCI <- as.list(epci$EPCI) %>% set_names(epci$LIBEPCI)
linputREGION <- as.list(as.character(region$REG)) %>% set_names(region$LIBELLE)


# saveRDS(linputVille,file = paste0(chemin_output,"carto3d/liste_AAV_app.RDS"))


######Testpour comparaison
test_lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
test_lcentro <- readRDS(paste0(chemin_output,"centro_smooth/lcentro_smooth.rds"))
test_carto3d <- readRDS(paste0(chemin_output,"carto3d/001_200.rds"))
test_centroville <- readRDS(paste0(chemin_output,"carto3d/centroVilles.rds"))
test_diamËtres <- readRDS(paste0(chemin_output,"carto3d/diamVilles.rds"))
test_listeaav <- readRDS(paste0(chemin_output,"carto3d/liste_AAV_app.RDS"))



