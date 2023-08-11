# :::::::::::::::::::::::::::::::::::::::
#         Fonctions globales du projet 
# :::::::::::::::::::::::::::::::::::::::

#' @auteur : Julien PRAMIL

cat("\n", "Chargement des fonctions", "\n")








# ******************************
#        import_bases       ####
# ******************************

#' Fonction d'importation des bases
#' selon un paramètre de millésime
#' et selon que l'on importe les bases initiales ou corrigées

# import_bases <- function(annee,corr=F){
#   cat("\n","Chargement de la base de l'annee ",annee, "\n")
#   if (corr) {
#     repo <- repo_basesRDS_corr
#     type <- "men"
#     extens <- ""
#   }else{
#     repo <- repo_basesRDS
#     type <- ifelse(as.numeric(annee)>11,"filo","rfl")
#     extens <- ifelse(as.numeric(annee)>11,"","_light")
#   }
# 
#   menages <- readRDS(paste0(repo,type,annee,extens,".RDS"))
#   return(menages)
# }











# ********************************************************************
#      Fonction (boucle) de confection de toutes les versions     ####
# ********************************************************************

#' Objectif : créer la liste de données emboîtées
#' La fonction gère la boucle et l'appel à toutes les sous-fonctions nécessaires
#' Idée général : importer une seule fois chaque millésime pour limiter temps de calcul
#' Paramètres :
#' - la liste des millésimes
#' - la liste des codes AAV (les villes dans le champ)
#' - le nombre de groupes de revenus (ici 5)
#' - le revenu minimal en dessous duquel on exclut les ménages du champ
#' - les tailles de carreaux possibles pour mailler le territoire
#' - Seuil de filtrage des carreaux trop peu peuplés (NULL si on ne veut pas filtrer)

# f_couche <- function(millesi,zaav,nb_tranches,revmen_min,liste_taille_carreaux,nbmen_min=NULL) {
# 
#   # millesi=c("04");zaav=c("013");nb_tranches=5;revmen_min=0;liste_taille_carreaux=c(200,500)
# 
#   # Packages
#     library(tidyverse)
#     library(classInt)
#     library(spatstat)
#     library(readxl)
#     library(sf)
#     library(btb)
# 
#     # Importation des contours des QP métropole
#     qp_contours <<- st_read(paste0(repo_qpv,"qp_francemetro_2020.shp"),quiet = T)
# 
#     # Création des listes de COG pour chaque pôle
#     cog_pole <- lapply(zaav, f_lzaav)
#     names(cog_pole) <- zaav
# 
# 
#     ##### Pour chaque années
# 
#     listeFinale <- list()
#     for (annee in millesi) {
#       #annee <- "16"
# 
#       # Importation de la base ménages correspondant au millésime
#       menagesInit <- import_bases(annee, corr = T)
# 
# 
#      ##### Pour chaque Zaav
# 
#       liste2 <- list()
#       for (codeZaav in zaav) { #codeZaav="013"
# 
# 
#         # Message dans la boucle
#         cat("\n","Traitements pour l'annee : ",annee,"\n",
#             "\n","Traitements pour AAV : ",codeZaav,"\n")
# 
#         # Restrictions du champ au pôle
#         menages <- menagesInit %>%
#           filter(CODGEO_2020 %in% cog_pole[[codeZaav]])
# 
# 
#         # ##### Pour chaque niveau d'observation (AAV entière ou pôle)
#         #
#         # liste2 <- list()
#         # for (nivAgglo in c("zaavTot", "pole")) { #nivAgglo="zaavTot"
#         #   # restriction au cog du pôle quand demandé
#         #   if (nivAgglo == "pole") {
#         #     menages <- menages %>% filter(CODGEO_2020 %in% lzaav[[codeZaav]]$cog_pole)
#         #   }
# 
# 
#           ##### Pour chaque taille de carreau
#           liste1 <- list()
#           for (taille_carreau in liste_taille_carreaux) {#taille_carreau=200
# 
#             # Passage de la fonction f_carroyage sur la base ménages restreinte
#             liste1[[paste0("c",taille_carreau)]] <-list(
#               "sansFiltrage"=f_carroyage(menages, nb_tranches, taille_carreau),
#               "avecFiltrage"=f_carroyage(menages, nb_tranches, taille_carreau,filtrage = T)
#             )
# 
#           }
#         #   liste2[[nivAgglo]] <- liste1
#         # }
#         liste2[[paste0("ville",codeZaav)]] <- liste1
#       }
#       listeFinale[[paste0("annee",annee)]] <- liste2
#     }
#     return(listeFinale)
#   }
# 


#### Detection des villes n'ayant pas de QP (pour éviter problème Annecy)
##########################################################################

# zoneAvQP <- function(liste_cog,fichier_qp){
#   res <- sum(liste_cog %in% unique(fichier_qp$depcom)) > 0
#   return(res)
# }





##### Fonction de calcul des indices de ségrégation et des couches carroyées



# Petit fonction de calcul de l'entropie à partir de f_entro_partielle

f_entro <- function(vec_prop){
  if(sum(vec_prop)==1){
    res <- f_entro_partielle(vec_prop) %>% sum()
  }else{
    warning("Probleme sur le vecteur de distribution")
    res <- NULL
  }
  return(res)
}



# ************************************************
#                     fLissage                ----
# ************************************************

#' @description Fonction de lissage de la ségrégation des carreaux pour la cartographie
#' carreaux_filtres est une couche carroyée filtrée préparée dans le script 05_crea_pixel (lcouches_trans3 dans la boucle)

# 
# # fLissage <- function(carreaux_filtres,nbmen_min,taille_carreau,rayon_lissage,nb_tranches){
# 
#   # Sélection des carreaux avec suffisamment de ménages
#   carreaux_filtres <- carreaux_filtres %>% rename(x=x_centro,y=y_centro)
# 
#   # Calcul de l'indice de Theil (car l'equirépartition est rompu par le filtrage)
#   # entropie_tot <- centroides_peuples %>% summarise(across(starts_with("nbPersTranche"),sum)) %>% as_vector()
#   # entropie_tot <- f_entro_partielle(entropie_tot/sum(entropie_tot)) %>% sum()
# 
#   # Recalcul de la segregation standardisée avec la vrai entropie de la ville
#   # centroides_peuples <- centroides_peuples %>% mutate(segregStandar=(entropie_tot-entro)/entropie_tot)
# 
#   # Calcul de la contribution à la ségrégation (brute, non lissee)
#   carreaux_filtres <- carreaux_filtres %>%
#     mutate(segregStandar_ponder=segregStandar*nbpers_carreau/sum(nbpers_carreau),
#            contribSegreg=100*segregStandar_ponder/sum(segregStandar_ponder))
# 
#   # Lissage avec le package BTB : nombre de personnes par groupes, médiane de revenus
#   carreaux_smooth <- btb::kernelSmoothing(
#     dfObservations = carreaux_filtres %>%
#       select(starts_with("nbpers",ignore.case = T),x,y), #median_carreau
#     iCellSize = taille_carreau_num,
#     sEPSG = "2154",
#     iBandwidth = rayon_lissage,
#     iNeighbor = 0 # Pas d'effet de bord aux frontières de la couche de carreaux
#   )
# 
#   # Changement noms
#   colnames(carreaux_smooth)[3:8] <- colnames(carreaux_smooth)[3:8] %>% paste0("_liss")
# 
# 
#   # Rapatriement de variables brutes sur la couche lissée
#   carreaux_smooth <- carreaux_smooth %>%
#     left_join(carreaux_filtres %>%
#                 select(idcarreau,x,y,segregStandar,contribSegreg,median_carreau,nbpers_carreau,nbmen_carreau), #nbPersTranche3
#                 # rename(median_carreau_nonLissee=median_carreau,
#                 #        segregStandar_nonLissee=segregStandar,
#                 #        nbpers_carreau_nonLissee=nbpers_carreau,
#                 #        nbmen_carreau_nonLissee=nbmen_carreau),
#               by=c("x","y"))
# 
# 
#   #### Calcul des entropies lissées *****************
# 
#   entropLisee <- carreaux_smooth %>% select(idcarreau,starts_with("nbpersTranche",ignore.case = T)) %>% st_drop_geometry()
#   entropLisee <- entropLisee %>% pivot_longer(!idcarreau,names_to = "tranche", values_to = "nbpers_liss")
#   entropLisee <- entropLisee %>%
#     group_by(idcarreau) %>%
#     mutate(
#       nbpersCarreau_liss = sum(nbpers_liss),
#       p_liss = nbpers_liss / nbpersCarreau_liss,
#       entroPartiel_liss = f_entro_partielle(p_liss)
#     ) %>%
#     summarise(entro_liss = sum(entroPartiel_liss),
#               nbpersCarreau_liss = sum(nbpers_liss)) %>%
#     mutate(
#       segregStandar_liss = (log(nb_tranches) - entro_liss) / log(nb_tranches), # Entropie totale inchangée par le lissage (car conservatif)
#       segregPonder_liss=segregStandar_liss*nbpersCarreau_liss / sum(nbpersCarreau_liss),
#       contribSegreg_liss=100*segregPonder_liss/sum(segregPonder_liss)
#     )
# 
#   #### Rapatriement de la segregation et de la contribution proprement lissées
#   carreaux_smooth <- carreaux_smooth %>%
#     left_join(entropLisee %>% select(!segregPonder_liss),by="idcarreau")
# 
# 
#   #### Passage à un format centroides
# 
#   centroides_smooth_4326 <- carreaux_smooth %>%
#     st_centroid() %>%
#     st_transform(crs=4326)
#   coords <- st_coordinates(centroides_smooth_4326) %>% as.data.frame() %>% rename(lat=Y,long=X)
#   centroides_smooth_4326 <- centroides_smooth_4326 %>% bind_cols(coords)
# 
# 
#   #### Modifications des formats des variables
#   centroides_smooth_4326 <-
#     centroides_smooth_4326 %>%
#     mutate(median_carreau_lab = median_carreau %>% round(digits=0) %>%  format(digits=0,scientific = F),
#            segregStandar_lab = segregStandar %>%round(digits=2) %>%  format(digits=2),
#            segregStandar_liss_lab = segregStandar_liss %>%round(digits=2)%>% format(digits=2),
#            contribSegreg_lab = (contribSegreg*100) %>% round(digits=2)%>% format(digits=2)%>% paste0(" pour 10 000"),
#            contribSegreg_liss_lab = (contribSegreg_liss*100) %>% round(digits=2)%>% format(digits=2) %>% paste0(" pour 10 000")
#     )
# 
#   return(centroides_smooth_4326)
# }



# *********************************************************************
#                              f_lzaav                             ####
# *********************************************************************

#' Confection liste des COG du pôle à partir d'un code ZAAV
#' Utilisée par f_couche()
#' Retourne la liste des COG du pôle
#' A partir d'un code ZAAV
#' En ajoutant les arrondissement communaux au cas où
#' /!\ Uniquement le plus grand pôle dans les AAV multipoles

f_lzaav <- function(codeZaav){

  # #Création de la liste (une pour le pole, une pour l'AAV entière)
  # lzaav <- list(
  #   cog_pole = passageZaav %>% filter(AAV20 == codeZaav, CATEAAV20 %in% c("11", "12")) %>% pull(CODGEO),
  #   cog_zaav = passageZaav %>% filter(AAV20 == codeZaav) %>% pull(CODGEO)
  # )

  cog_pole <- passageZaav %>%
    filter(AAV20 == codeZaav, CATEAAV20 %in% c("11", "12")) %>%
    pull(CODGEO)

  # Ajout si nécessaire des COG des arrondissements communaux de PLM

  if(codeZaav %in% c("001","002","003")){
    if(codeZaav=="001") cog_arrond <- 75101:75120 %>% as.character()
    if(codeZaav=="002") cog_arrond <- 69381:69389 %>% as.character()
    if(codeZaav=="003") cog_arrond <- 13201:13216 %>% as.character()
    # lzaav <- lzaav %>% lapply(function(vec) vec <- c(vec,cog_arrond))
    cog_pole <- c(cog_pole,cog_arrond)
  }
  #return(lzaav)
  return(cog_pole)
}



# # ***************************************
# #               ajoutCol
# # ***************************************
# 
# # Fonction de coloriage des carreaux selon la médiane de revenus (utilisée infra)
# # On ajoute seulement une variable contenant les couleurs en code HTML suite à une discrétisation en quantiles de la variable de revenu médian par carreau
# ajoutCol <- function(tab){
#   classif <- classIntervals(tab$median_carreau,n = nb_tranches,style = "quantile") #jenks"quantile"
#   pal <- colorBin("RdYlBu", bins = classif$brks)
#   tab$couleur <- pal(tab$median_carreau)
#   return(tab)
# }













