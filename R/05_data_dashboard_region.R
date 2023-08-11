# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#     Création des input secretisés pour l'appli dashboard
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' 
#' @auteur : Antoine Klein


source("R/00a_global.R")
files_functions <- paste0("R/function_dashprod/",list.files("R/function_dashprod/"))
map(files_functions, source,encoding = "UTF-8")


# lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))

lcouches_region <- lcouches_region %>% set_names(2004:2019)

# linputVille <- readRDS(paste0(chemin_output,"carto3d/liste_reg_app.RDS"))
Regionpassage <- data.frame(reg=unlist(linputREGION),nomreg=names(linputREGION))


# Initialisation de la liste des données
codesREG <- names(lcouches_region$`2019`)
datadash <- rep(list(NULL),length(codesREG))
names(datadash) <- codesREG

# taille <- 2000
maCouche <- lcouches_region %>% modify_depth(2,as.character(taille))
maCouche <- maCouche %>% purrr::transpose(codesREG)
maCouche <- maCouche %>% map(compact)


#### Indices totaux =========================


indices_segreg <- maCouche %>% 
  modify_depth(2,"segreg") %>% 
  modify_depth(3,"segTot")

prod_indices_segreg <- function(liste,typindice){
  liste %>% 
    modify_depth(2,typindice) %>% 
    map(~.x %>% as_tibble() %>% pivot_longer(everything(),names_to = "annee",values_to = "segTot"))
}

indices_segreg_theil <- prod_indices_segreg(indices_segreg,"theil")
indices_segreg_iim <- prod_indices_segreg(indices_segreg,"iim")
indices_segreg_rebase_theil <- indices_segreg_theil %>% map(~.x %>% mutate(segTot=100*segTot/segTot[1]))
indices_segreg_rebase_iim <- indices_segreg_iim %>% map(~.x %>% mutate(segTot=100*segTot/segTot[1]))




#### Graphiques de comparaison =========================

dataCompar <- extrac_compar(lcouches_region,passage_nom = Regionpassage, taille)
dataCompar_theil <- dataCompar %>% filter(typ_indice=="theil")
dataCompar_iim <- dataCompar %>% filter(typ_indice=="iim")




#### Décomposition QP-Hors QP =========================

data_distriQP <- distriQP(maCouche)
data_distri_hors_QP <- distri_hors_QP(maCouche)
data_evolPartQP <- evolPartQP(maCouche,exclusion=c("043"))

tabContrib <- returnTabContrib(maCouche)
tabContrib_theil <- tabContrib %>% map("theil")
tabContrib_iim <- tabContrib %>% map("iim")


#### Décomposition par groupes de revenus =========================


data_contribGroupes <- tabContrib_groupes(maCouche)
data_contribGroupes_theil <- data_contribGroupes %>% map("theil")
data_contribGroupes_iim <- data_contribGroupes %>% map("iim")


#### Compléments =========================

data_effectifs <- effectifs(maCouche)
data_nb_carreaux <- nbCarreaux(maCouche)


data_decilesMen <- tab_deciles(maCouche,type="men")
data_decilesIndiv <- tab_deciles(maCouche,type="indiv")

data_quantiles <- def_quantiles(maCouche)
data_medianes_groupes <- medianes_groupes(maCouche)

#### Sauvegardes ======================

for (reg in codesREG) {#reg="001"
  datadash[[reg]][["indices_segreg_theil"]] <- indices_segreg_theil[[reg]]
  datadash[[reg]][["indices_segreg_iim"]] <- indices_segreg_iim[[reg]]
  datadash[[reg]][["indices_segreg_rebase_theil"]] <- indices_segreg_rebase_theil[[reg]]
  datadash[[reg]][["indices_segreg_rebase_iim"]] <- indices_segreg_rebase_iim[[reg]]
  
  datadash[[reg]][["data_distriQP"]] <- data_distriQP[[reg]]
  datadash[[reg]][["data_distri_hors_QP"]] <- data_distri_hors_QP[[reg]]
  datadash[[reg]][["data_evolPartQP"]] <- data_evolPartQP[[reg]]
  datadash[[reg]][["tabContrib_theil"]] <- tabContrib_theil[[reg]]
  datadash[[reg]][["tabContrib_iim"]] <- tabContrib_iim[[reg]]
  
  datadash[[reg]][["data_contribGroupes_theil"]] <- data_contribGroupes_theil[[reg]]
  datadash[[reg]][["data_contribGroupes_iim"]] <- data_contribGroupes_iim[[reg]]
  
  datadash[[reg]][["data_effectifs"]] <- data_effectifs[[reg]]
  datadash[[reg]][["data_nb_carreaux"]] <- data_nb_carreaux[[reg]]
  datadash[[reg]][["data_decilesMen"]] <- data_decilesMen[[reg]]
  datadash[[reg]][["data_decilesIndiv"]] <- data_decilesIndiv[[reg]]
  datadash[[reg]][["data_quantiles"]] <- data_quantiles[[reg]]
  datadash[[reg]][["data_medianes_groupes"]] <- data_medianes_groupes[[reg]]
}

datadash$dataCompar_theil <- dataCompar_theil
datadash$dataCompar_iim <- dataCompar_iim


# saveRDS(datadash,paste0(chemin_output,"data_dashboard/datadash2.rds"))

# #A comparer avec :
# test_datadash <- readRDS(paste0(chemin_output,"data_dashboard/datadash2.rds"))