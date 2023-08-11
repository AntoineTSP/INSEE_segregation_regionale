# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#          Génération des couches carroyées pour le Grand Est
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' Création des données-résultats de l'investissement élargi
#' Passage des bases ménages à des couches de carreaux et d'indicateurs de ségrégation
#' Création des couches carroyées pour la cartographie
#' 
#' @auteur : Antoine Klein


#### *********************************************
####            Importation inputs           -----
#On charge nos variables globales et nos fonctions
source("R/00a_global.R")
#### *********************************************
#### end ####

#### *********************************************
####            Bases ménages corrigées      -----
lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))
#10min
#### *********************************************
#### end ####

#### *********************************************
####            Filtrage au Grand Est      -------
# #Filtrage départemental
# lmen_test <- lmen_test %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met])

#Filtrage territorial avec restriction première aux départements du Grand Est + limitrophes
lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_grand_est_limitrophe])
#En ne gardant que les ménages inclus dans les limites territoriales
# shapefile <- st_read("U:/AU33/Carto/reg_44/reg_reg_44_2022.shp",quiet = T)
shapefile <- st_read("U:/AU33/rural/commune_bdtopo_franceentiere_2022.shp",quiet = T)
shapefile <- shapefile  %>% filter(reg %in% c(code_region)) %>% 
                  st_transform(proj_lambert93) %>% 
                  group_by(reg) %>% 
                  summarize(geometry=st_union(geometry))
# st_write(shapefile, "U:/AU33/Carto/Region_precise.shp")

select_rows <- function(df,shp){
  return(st_as_sf(x = df, 
                 coords = c("x", "y"),
                 crs = proj_lambert93) %>% 
           st_filter(shp) %>% 
           as.data.frame() %>% 
           mutate(
             x= as.data.frame(st_coordinates(geometry))$X,
             y= as.data.frame(st_coordinates(geometry))$Y) %>% 
           select(-geometry)
         
  )
}

deb <- Sys.time()
lmen <- lmen %>% lapply(select_rows,shapefile)
fin <- Sys.time()-deb
fin #25min

# #Statistiques filtre départemental/limite territoriale
# #Compter ceux retenus par les départements et non par le territoire:
# lmen$men04[! lmen$men04$id %in% lmen_test$men04$id] %>% count()
# #Départements concernés
# lmen_test$men04[! lmen_test$men04$id %in% lmen$men04$id]$dep %>% unique()
# #Récupérer les points pour voir sous QGIS
# liste_points <- lmen_test$men19[! lmen_test$men19$id %in% lmen$men19$id] %>% select(x,y)
# shp <- st_as_sf(x = liste_points,
#          coords = c("x", "y"),
#          crs = proj_lambert93)
# st_write(shp, "U:/AU33/Points_filtre/points_hors_territoire.shp")

# #Compter ceux retenus par le filtre territorial qui ont un département ne faisant pas partie du Grand Est
# lmen$men04 %>% filter(! dep %in% dep_valid_met ) %>% count()
# #Départements Retenus mais ne faisant pas partie de ceux du Grand Est
# lmen$men04 %>% filter(! dep %in% dep_valid_met ) %>% select(dep) %>% unique()
# #Récupérer ces points pour les voir sur QGIS:
# liste_points <- lmen$men04 %>% filter(! dep %in% dep_valid_met ) %>% select(x,y)
# shp <- st_as_sf(x = liste_points,
#          coords = c("x", "y"),
#          crs = proj_lambert93)
# st_write(shp, "U:/AU33/Points_filtre/points_mauvais_departement.shp")

#On filtre in fine par les départements du Grand Est
lmen <- lapply(lmen, function(df) setattr(df, "class", c("data.table", "data.frame")))
# lmen <- lapply(lmen, function(df) df[df$dep %in% dep_valid_met, ])
#### *********************************************
#### end ####


#### *********************************************
####            Contours Urbain/rural      -------
#Contours approximatifs
# com_contours <- st_read("U:/AU33/rural/commune_francemetro_2021.shp",quiet = T)
#Contours précis
com_contours <- st_read("U:/AU33/rural/commune_bdtopo_franceentiere_2022.shp",quiet = T)
#On filtre le Grand Est
com_contours <- com_contours %>% filter(reg %in% c(code_region)) %>% st_transform(proj_lambert93)
#Source : https://www.insee.fr/fr/statistiques/5039991?sommaire=5040030
com_rural <- read_excel("U:/AU33/rural/urbain_rural.xlsx", sheet = "Figure 5",skip = 2)
#1 si rural !!!
com_rural$`Typologie urbain/rural` <- com_rural$`Typologie urbain/rural` %>%  str_extract('\\w*') %>%  str_equal("rural")
#On garde que les communes de la région
com_rural <- com_rural %>% filter(com_rural$`Code géographique communal` %>% str_sub(1,2) %in% dep_valid_met)
#com_region
com_region <- com_rural %>% 
  mutate(dep = com_rural$`Code géographique communal` %>% str_sub(1,2),
         reg = as.integer(code_region)) %>% 
  select(-`Typologie urbain/rural`) %>% 
  setNames(c("CODGEO", "DEP", "REG"))
#On garde la liste des CODGEO des communes urbaines
com_urbain <- com_rural %>% filter(com_rural$`Typologie urbain/rural` %in% c("FALSE")) %>% 
  select(`Code géographique communal`)
com_rural <- com_rural %>% filter(com_rural$`Typologie urbain/rural` %in% c("TRUE")) %>% 
  select(`Code géographique communal`) %>% 
  mutate(reg = as.integer(code_region))
#On filtre les contours avec les communes rurales
com_rural_contours <- com_contours %>%filter(codgeo %in% com_rural$`Code géographique communal`)
#Autant de contours que de communes rurales, ouf !
#### *********************************************
#### end ####


scissions <- read_excel(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
                        sheet = "Liste des scissions",skip = 5)

###Région 
# com_region <- com_epci %>% filter(REG %in% c(reg)) %>% subset(select = -c(EPCI, LIBEPCI))
region <- read.csv("U:/AU33/v_region_2023.csv") %>% filter(REG %in% c(code_region))

#### ****************************************************
####      Definition de la région                    ----
#### ****************************************************


sf_region <- com_contours  %>% group_by(reg) %>% summarize(geometry=st_union(geometry))



#Urbain/rural
pole_sans_rural <- NULL
com_region_sans_rural <- com_urbain$`Code géographique communal`
#A commenter si inutile à l'échelle régionale
#Rural Ã  cheval sur pole ?
# sfqp_pole_inters <- sf_pole %>% st_intersection(qp_contours %>% select(code,geometry))
sf_rural_region_inters <- sf_region %>% st_intersection(com_rural_contours %>% select(codgeo,geometry))

# # #Récupérer les contours des communes rurales  pour les voir sur QGIS:
# # st_write(sf_rural_region_inters, "U:/AU33/rural/territoire_rural.shp")
territoire_rural <- st_union(sf_rural_region_inters$geometry)
# st_write(test, "U:/AU33/rural/territoire_rural_union.shp")

#Récupérer le revenu médian des ménages ruraux
deb <- Sys.time()
lmen_rural <- lmen %>% lapply(select_rows,territoire_rural)
fin <- Sys.time()-deb
fin #25min
lmen_rural <- lapply(lmen_rural, function(df) setattr(df, "class", c("data.table", "data.frame")))
liste_mediane_rurale <- lapply(lmen_rural,function(df) median(df$revdecucm))

#Territoire urbain
territoire_urbain <- st_difference(sf_region$geometry,territoire_rural)
# st_write(test_urbain, "U:/AU33/rural/territoire_urbain_union.shp")

#Récupérer le revenu médian des ménages urbains
deb <- Sys.time()
lmen_urbain <- lmen %>% lapply(select_rows,territoire_urbain)
fin <- Sys.time()-deb
fin #25min
lmen_urbain <- lapply(lmen_urbain, function(df) setattr(df, "class", c("data.table", "data.frame")))
liste_mediane_urbain <- lapply(lmen_urbain,function(df) median(df$revdecucm))

liste_mediane_region <- lapply(lmen,function(df) median(df$revdecucm))

# Check absence communes scissionnÃ©es dans les grands pÃ´les 
#Check Region
com_region[com_region$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]
#Problème avec : Châtillon-sur-Marne / Autreville-sur-la-Renne / Val-de-Meuse / Nully / Varennes-sur-Amance / Récicourt / Kirrwiller / Loisey

#### *********************************************
#### end ####

#### *****************************************************************
####            Filtrage ménages de la région par les cog        =====
#### *****************************************************************

# #Filtre département
filtre_par_pole <- function(dtmen,lcog){
  lcog %>% map(~dtmen[depcom %in% unique(.x$CODGEO)])
}



#Ajout Region
lregion_cog <- split(com_region,com_region$REG)
lmen_region <- lmen %>% map(filtre_par_pole,lregion_cog) # 1mn
lmen_region <- lmen_region %>% unlist(recursive = F)
#### *****************************************************************
#### end ####






#### *****************************************************************
####                          Carroyage                          =====
#### *****************************************************************

# Association menage-idInspire + filtrage

l_tcar_named <- purrr::set_names(liste_taille_carreaux,liste_taille_carreaux)
deb <- Sys.time()
cl <- makeCluster(20)
clusterExport(cl, "com_region")
clusterExport(cl, "pole_sans_rural")
clusterExport(cl, "sf_rural_region_inters")
clusterExport(cl, "l_tcar_named")
clusterExport(cl, "liste_taille_carreaux")
clusterEvalQ(cl, source("R/00a_global.R"))

#Ajout Region
lmen_region <- parLapply(cl,
                         lmen_region,
                         f_inspire_filtre,
                         liste_taille_carreaux,com_region,nbmen_min,"CODGEO", varCodeZe="REG")

#########Statistiques des carreaux avant filtrage (>20 ménages par carreaux)
#Nombre total de carreaux
Nb_total_carreaux <- as.data.frame(matrix(ncol=length(liste_taille_carreaux), nrow=length(millesi)))
Nb_total_menages <- as.data.frame(matrix(ncol=length(liste_taille_carreaux), nrow=length(millesi)))
Nb_total_population <- as.data.frame(matrix(ncol=length(liste_taille_carreaux), nrow=length(millesi)))
i <- 1
j <- 1
for(tcar in liste_taille_carreaux){
  i <- 1
  list_value <- c()
  for(annee in millesi){
    string_car <- paste0("idInspire" ,tcar)
    string_annee <- paste0("men", annee,".", code_region)
    #Carreaux
    value_carreaux <- nrow(unique(lmen_region[[i]] %>%
                           select(contains(string_car))))
    Nb_total_carreaux[i,j] <- value_carreaux
    #Menages
    value_menages <- nrow(lmen_region[[i]])
    Nb_total_menages[i,j] <- value_menages
    #Population
    value_pop <- lmen_region[[i]]$nbpersm %>% sum()
    Nb_total_population[i,j] <- value_pop
    i <- i+1
  }
  j <- j+1
}
#Carreaux
Nb_total_carreaux <- setNames(Nb_total_carreaux, paste0("Carreau de taille ",liste_taille_carreaux))
row.names(Nb_total_carreaux) <- paste0("Annee 20",millesi)
#Menages
Nb_total_menages <- setNames(Nb_total_menages, "Nb_total_men")
row.names(Nb_total_menages) <- paste0("Annee 20",millesi)
Nb_total_menages <- Nb_total_menages %>% select ("Nb_total_men")
#Population
Nb_total_population <- setNames(Nb_total_population, "Nb_total_pop")
row.names(Nb_total_population) <- paste0("Annee 20",millesi)
Nb_total_population <- Nb_total_population %>%  select("Nb_total_pop")

#Save statistiques
write.csv(Nb_total_carreaux,paste0("U:/AU33/Statistiques/Nb_total_carreaux.csv"))
write.csv(Nb_total_menages,paste0("U:/AU33/Statistiques/Nb_total_menages.csv"))
write.csv(Nb_total_population,paste0("U:/AU33/Statistiques/Nb_total_population.csv"))

# # 
# # #Nombre total de carreaux 
# # unique(lmen_region_test[[1]]$idInspire2000) %>% length()
# # 
# # #Nombre de carreaux gardés
# # unique(lmen_region_test[[1]]$idInspire2000) %>% length() -
# #   unique(lmen_region_test[[1]][lmen_region_test[[1]]$filtre2000 == FALSE]$idInspire2000) %>% length()
# # 
# # #Nombre total de ménages
# # nrow(lmen_region_test[[1]])
# # 
# # #Nombre de ménages gardés
# # nrow(lmen_region_test[[1]])-lmen_region_test[[1]]$filtre2000 %in% FALSE %>% sum()
# # 
# # #Nombre total de la population
# # lmen_region_test[[15]]$nbpersm %>% sum()
# # 
# # #Nombre total de la population gardée
# # count <- lmen_region_test[[1]][lmen_region_test[[1]]$idInspire2000 %in% unique(lmen_region_test[[1]][lmen_region_test[[1]]$filtre2000 == TRUE]$idInspire2000)]
# # count$nbpersm %>% sum()


# Pour les DOM avant 2014
#Ajout Region
lmen_region[! lmen_region %>% map_lgl(~.x %>% is.data.table)] <- NULL

#Ajout Region
lmen_region <- parLapply(cl,lmen_region,function(tab){
  l_tcar_named %>% 
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_region,ZE_ssQP=pole_sans_rural,sf_rural_region_inters,"CODGEO", varCodeZe="REG")
})

stopCluster(cl)
fin <- Sys.time()-deb
fin


#Ajout Region
lcouches_region <- lmen_region %>% restructure()

# # Sauvegarde de la liste des couches
# saveRDS(lcouches,file = paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
#### *****************************************************************
#### end ####




#### ***********************************************************************
####    Préparation des couches carroyées pour cartographie (lissage)  =====
#### ***********************************************************************

#' CrÃ©ation des couches carroyÃ©es en input de l'application de cartogrpahie
#' Pour chaque version (annee-ville-taille des carreaux) : 
#' - On crÃ©Ã© une variable de contribution du carreau Ã  la sÃ©grÃ©gation totale
#' - On lisse la sÃ©grÃ©gation et la contribution des carreaux (en lissant prÃ©alablement les populations des diffÃ©rents groupes sociaux et en recalculant proprement les indicateurs sur les populations lissÃ©es) 
#' - On discrÃ©tise les carreaux par niveaux de revenus et on associe une couleur pour la carto 
#' - Sauvegarde 

#Ajout Region
lcouches_region_car <- lcouches_region %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])


deb <- Sys.time()
cl <- makeCluster(15)
clusterExport(cl, "nb_tranches")
clusterEvalQ(cl, source("R/00a_global.R"))
#Ajout Region
lcentro_region_smooth <-  parLapply(cl,lcouches_region_car,fLissage,nb_tranches=nb_tranches)

stopCluster(cl)
duree_parLapply <- Sys.time()-deb #3mn
duree_parLapply

# saveRDS(lcentro_smooth,paste0(chemin_output,"centro_smooth/lcentro_smooth.rds"))
#### *****************************************************************
#### end ####



#### ***********************************************************************
####    Sauvegarde 1 fichier par ville*taille_carreau (pour Shiny)     =====
#### ***********************************************************************


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




# Sauvegarde des centroÃ¯des de pÃ´les pour les cartes
#Ajout region
lgeom_region <- split(sf_region$geometry,seq_along(sf_region$geometry))
names(lgeom_region) <- sf_region$reg
centroVilles_region <- lgeom_region %>% 
  map(~ list(long=as.numeric((.x %>% st_centroid %>% st_coordinates())[1]),
             lat=as.numeric((.x %>% st_centroid %>% st_coordinates())[2])))


# saveRDS(centroVilles,paste0(chemin_output,"carto3d/centroVilles.rds"))

#Ajout Region
diamVilles_region <- 
  lgeom_region %>% lapply(function(x){ # x=sf_pole$geometry[53]
    x %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% 
      st_distance() %>% max() %>% as.numeric()/10})

# saveRDS(diamVilles,paste0(chemin_output,"carto3d/diamVilles.rds"))
#### *****************************************************************
#### end ####






# *************************************************************
#    Liste nommÃ©e des pÃ´les pour les input des appli Shiny  ----
# *************************************************************

linputREGION <- as.list(as.character(region$REG)) %>% set_names(region$LIBELLE)


# saveRDS(linputVille,file = paste0(chemin_output,"carto3d/liste_AAV_app.RDS"))
#### *****************************************************************
#### end ####

######Testpour comparaison
test_lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))
test_lcentro <- readRDS(paste0(chemin_output,"centro_smooth/lcentro_smooth.rds"))
test_carto3d <- readRDS(paste0(chemin_output,"carto3d/001_200.rds"))
test_centroville <- readRDS(paste0(chemin_output,"carto3d/centroVilles.rds"))
test_diamètres <- readRDS(paste0(chemin_output,"carto3d/diamVilles.rds"))
test_listeaav <- readRDS(paste0(chemin_output,"carto3d/liste_AAV_app.RDS"))