#### *********************************************
####            Importation inputs           -----
#On charge nos variables globales et nos fonctions
source("R/00a_global.R")
#### *********************************************
#### end ####

#lmen doit ÃÂªtre fourni et filtrÃÂ© par bdtopo
region <- read.csv("U:/AU33/v_region_2023.csv") %>% filter(REG %in% c("44"))
sf_region <- com_contours  %>% group_by(reg) %>% summarize(geometry=st_union(geometry))

#### *********************************************
####            Contours > seuil / <seuil      -------
Part_frontalier <- read_excel("U:/AU33/frontalier/frontalier.xlsx", sheet = "tacComPoidsFront",skip = 0) %>% 
  mutate(part_frontalier_suisse = frontSU / actifsOccupes)
com_frontalieres <- Part_frontalier %>% 
  filter(part_frontalier_suisse >= seuil_nb_hab)
com_non_frontalieres <- Part_frontalier %>%
  filter(part_frontalier_suisse < seuil_nb_hab) 


#On garde la liste des CODGEO des communes urbaines
Liste_CODGEO_non_frontalieres <- com_non_frontalieres$CODGEO
Liste_com_non_frontalieres <- com_non_frontalieres$LIBGEO
Nb_com_non_frontalieres <- com_non_frontalieres %>% count()

Liste_CODGEO_frontalieres <- com_frontalieres$CODGEO
Liste_com_frontalieres <- com_frontalieres$LIBGEO
Nb_com_frontalieres <- com_frontalieres %>% count()
#On filtre les contours
com_non_frontalieres_contours <- com_contours %>%filter(codgeo %in% Liste_CODGEO_non_frontalieres)
com_frontalieres_contours <- com_contours %>%filter(codgeo %in% Liste_CODGEO_frontalieres)
#### *********************************************
#### end ####

sf_frontalieres_region_inters <- sf_region %>% st_intersection(com_frontalieres_contours %>% select(codgeo,geometry))

# # #RÃÂ©cupÃÂ©rer les contours des communes rurales  pour les voir sur QGIS:
# st_write(sf_frontalieres_region_inters, 
#   paste0("U:/AU33/frontalier/territoire_frontalier_", as.character(seuil_nb_hab),".shp"))
territoire_frontalier <- st_union(sf_frontalieres_region_inters$geometry)

st_write(territoire_frontalier,
         paste0("U:/AU33/frontalier/suisse/territoire_frontalier_suisse_", as.character(seuil_nb_hab),".shp"))

sf_non_frontalieres_region_inters <- sf_region %>% st_intersection(com_non_frontalieres_contours %>% select(codgeo,geometry))

# # #RÃÂ©cupÃÂ©rer les contours des communes rurales  pour les voir sur QGIS:
# st_write(sf_non_frontalieres_region_inters, 
#          paste0("U:/AU33/frontalier/territoire_non_frontalier_", as.character(seuil_nb_hab),".shp"))
territoire_non_frontalier <- st_union(sf_non_frontalieres_region_inters$geometry)

st_write(territoire_non_frontalier,
         paste0("U:/AU33/frontalier/suisse/territoire_non_frontalier_suisse_", as.character(seuil_nb_hab),".shp"))


select_rows_mediane <- function(df,shp){
  rural <- st_as_sf(x = df, 
                    coords = c("x", "y"),
                    crs = proj_lambert93) %>% 
    st_filter(shp) %>% 
    as.data.frame() %>% 
    mutate(
      x= as.data.frame(st_coordinates(geometry))$X,
      y= as.data.frame(st_coordinates(geometry))$Y) %>% 
    select(-geometry)
  return(list(rural,
              df[!id %in% rural$id])
         
  )
}

# #RÃ©cupÃ©rer le revenu mÃ©dian des mÃ©nages frontaliers / non_frontaliers
deb <- Sys.time()
list_df <- lmen %>% lapply(select_rows_mediane,territoire_frontalier)
liste_mediane_frontalier <- lapply(list_df,function(df) median(df[[1]]$revdecucm))
liste_mediane_non_frontalier <- lapply(list_df,function(df) median(df[[2]]$revdecucm))
fin <- Sys.time()-deb
fin #25min

liste_mediane_region <- lapply(lmen,function(df) median(df$revdecucm))



# #Filtre dÃ©partement
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
pole_sans_rural <- NULL
l_tcar_named <- purrr::set_names(liste_taille_carreaux,liste_taille_carreaux)
deb <- Sys.time()
cl <- makeCluster(20)
clusterExport(cl, "com_region")
clusterExport(cl, "pole_sans_rural")
clusterExport(cl, "sf_frontalieres_region_inters")
clusterExport(cl, "l_tcar_named")
clusterExport(cl, "liste_taille_carreaux")
clusterEvalQ(cl, source("R/00a_global.R"))

#Ajout Region
lmen_region <- parLapply(cl,
                         lmen_region,
                         f_inspire_filtre,
                         liste_taille_carreaux,com_region,nbmen_min,"CODGEO", varCodeZe="REG")

# Pour les DOM avant 2014
#Ajout Region
lmen_region[! lmen_region %>% map_lgl(~.x %>% is.data.table)] <- NULL

#Ajout Region
lmen_region <- parLapply(cl,lmen_region,function(tab){
  l_tcar_named %>%
    lapply(f_carroyage,tab,nb_tranches,passageComZE = com_region,ZE_ssQP=pole_sans_rural,sf_frontalieres_region_inters,"CODGEO", varCodeZe="REG")
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
####    PrÃÂ©paration des couches carroyÃÂ©es pour cartographie (lissage)  =====
#### ***********************************************************************

#' CrÃÂÃÂ©ation des couches carroyÃÂÃÂ©es en input de l'application de cartogrpahie
#' Pour chaque version (annee-ville-taille des carreaux) :
#' - On crÃÂÃÂ©ÃÂÃÂ© une variable de contribution du carreau ÃÂÃÂ  la sÃÂÃÂ©grÃÂÃÂ©gation totale
#' - On lisse la sÃÂÃÂ©grÃÂÃÂ©gation et la contribution des carreaux (en lissant prÃÂÃÂ©alablement les populations des diffÃÂÃÂ©rents groupes sociaux et en recalculant proprement les indicateurs sur les populations lissÃÂÃÂ©es)
#' - On discrÃÂÃÂ©tise les carreaux par niveaux de revenus et on associe une couleur pour la carto
#' - Sauvegarde

#Ajout Region
lcouches_region_car <- lcouches_region %>% unlist(recursive = F) %>% unlist(recursive = F) %>% map(~.x[["carreaux"]])


deb <- Sys.time()
cl <- makeCluster(2)
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
#'
#'
#'
#' #### ***********************************************************************
#' ####    Sauvegarde 1 fichier par ville*taille_carreau (pour Shiny)     =====
#' #### ***********************************************************************
#'
#'
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


# Sauvegarde des centroÃÂ¯des de pÃÂ´les pour les cartes
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
#    Liste nommÃÂ©e des pÃÂ´les pour les input des appli Shiny  ----
# *************************************************************

linputREGION <- as.list(as.character(region$REG)) %>% set_names(region$LIBELLE)