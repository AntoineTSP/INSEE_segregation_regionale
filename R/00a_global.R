# :::::::::::::::::::::::::::::::::::
#         Paramètres globaux 
# :::::::::::::::::::::::::::::::::::



cat("\n","Paramétrages globaux","\n")


# **********************************************
#            Répertoires du projet          ####
# **********************************************

coffre <- "X:/HAB-PSAR-AU-AU33-DEV/au33_v2/"
chemin_input <- paste0(coffre,"data/input/")
chemin_basesmen <- paste0(chemin_input,"bases_menages/")
chemin_output <- paste0(coffre,"data/output/")
chemin_dirnoseq <- paste0(chemin_input,"passage_dirno_xy/") #table de passage Dirnoseq/XY de Gabrielle Gallic
lien_passage_divlog <- "X:/HAB-PSAR-AU-AU33-DEV/au33_v2/data/input/passage_dirno_xy/passage_divlog/geolocalisation dirnoseq 2016-2019/l_dirnoseq_xy_20220419.csv"
#Commentaire Antoine
#chemin_filoProd <- "X:/HAB-FILOSOFI-PROD/"


# **********************************************
#                 packages                  ####
# **********************************************
cat("\n","Chargement des packages","\n")

# .libPaths(paste0(coffre,"packages"))

listpack <- c("rmdformats",
              "Hmisc",
              "haven",
              "parallel",
              "tidyverse",
              "data.table",
              "plotly",
              "knitr",
              "beepr",
              "visNetwork",
              "readxl",
              "classInt",
              "sf",
              "btb",
              # "mapview",
              "leaflet",
              "tictoc")
# 
# lapply(listpack, function(pkg) {
#   if (system.file(package = pkg,lib.loc = .libPaths()[1]) == '') {
#     install.packages(pkg,lib = .libPaths()[1])
#     }
#   require(pkg,character.only = T)
# }
# )

lapply(listpack, function(pkg) {
  if (system.file(package = pkg,lib.loc = .libPaths()[1]) == '') {
    install.packages(pkg,lib = .libPaths()[1])
  }
})

lapply(listpack, function(pkg) {
  require(pkg,character.only = T)
})
# 
# require(rmdformats)
# require(Hmisc)
# require(haven)
# require(parallel)
# require(tidyverse)
# require(data.table)
# require(plotly)
# require(knitr)
# require(beepr)
# require(visNetwork)
# require(readxl)
# require(classInt)
# require(sf)
# require(btb)
# require(mapview)
# require(leaflet)


# 
# # Packages sous Nexus :
# listpack_nexus <- c("COGugaison")
# lapply(listpack_nexus, function(pkg) {
#   if (system.file(package = pkg) == '') install.packages(pkg)
#   require(pkg,character.only = T,quietly = T)
# })

# Options pour voir les keys et classes des variables dans data.table
options(
  "datatable.print.keys" = TRUE,
  "datatable.print.class" = TRUE
)



# ***********************************************************
#      Chemin vers les bases stockées dans les coffres   ####
# ***********************************************************

# Coffres
# coffreRFL1 <- "X:/HAB-RFL-Mise-a-disposition/"
# coffreRFL2 <- "X:/HAB-RFL-PROD/"
# coffreFilosofi <- "X:/HAB-FILOSOFI-DONNEES-NBL/"
# 
# # RFL
# cheminRfl04 <- "HAB_A1223040_DMMICSAS/MENIR.sas7bdat"
# cheminRfl05 <- "HAB_A1223050_DMMICSAS/MENIR.sas7bdat"
# cheminRfl06 <- "HAB_A1223060_DMMICSAS/MENIR.sas7bdat"
# cheminRfl07 <- "HAB_A1223070_DMMICSAS/MENIR.sas7bdat"
# cheminRfl08 <- "HAB_A1223080_DMMICSAS/MENIR.sas7bdat"
# cheminRfl09 <- "HAB_A1223090_DMMICSAS/MENIRMET09.sas7bdat"
# 
# cheminRfl10 <- "2010/INFRA/MENAGE/MENIR/menirmet10.sas7bdat"
# cheminRfl11 <- "2011/INFRA/MENAGE/MENIR/menirmet11.sas7bdat"
# 
# # Filosofi
# cheminFilo12 <- "Filosofi 2012/Table Ménages/données/menages12.sas7bdat"
# cheminFilo13 <- "Filosofi 2013/Table Ménages/données/menages13.sas7bdat"
# cheminFilo14 <- "Filosofi 2014/Table Ménages/données/R/menages14.fst" # fst
# cheminFilo15 <- "Filosofi 2015/Table Ménages/données/menages15.sas7bdat"
# cheminFilo16 <- "Filosofi 2016/Table Ménages/données/menages16.sas7bdat"
# cheminFilo17 <- "Filosofi 2017/Table Ménages/données/menages17.fst" # fst
# 
# 


# *****************************************
#           Variables globales         ----
# *****************************************

millesi <- c("04","05","06","07","08","09","10","11","12","13","14","15","16","17", "18" , "19")


#### Paramètrage des hypothèses
nb_tranches <- 5 # Nombre de groupes sociaux
revmen_min <- 0 # Suppression des ménages ayant des revenus strictement négatifs
nbmen_min <- 20 # Seuil de filtrage des carreaux pas suffisamment peuplés
liste_taille_carreaux <- list(4000)# Liste des tailles de carreaux pour le maillage


proj_lambert93 <- 2154 
proj_mart <- 5490
proj_reun <-2975
proj_wgs84 <- 4326



code_region <- "44"
dep_valid_met <- c(paste0("0",8),10,51,52,54,55,57,67,68,88)
dep_valid_grand_est_limitrophe <- c(paste0("0",8),10,51,52,54,55,57,67,68,88,
                                    paste0("0",2),21,59,70,77,89,90)




# Option pour éviter l'erreur 63 de la fonction mapshot (enregistrer des cartes en html)
# mapviewOptions(fgb = FALSE)


# *****************************************
#           Change PATH       ----
# *****************************************
# # On r�cup�re le chemin (PATH) du r�pertoire de travail
# PATH <- getwd()
# # On change le chemin (PATH) du r�pertoire de travail
# setwd(str_sub(PATH, end = -2))
# getwd()

# *****************************************
#           Fonctions         ----
# *****************************************
source("R/00b_fonctions.R")
files_functions <- paste0("R/functions/",list.files("R/functions"))
map(files_functions, source,encoding = "UTF-8")


