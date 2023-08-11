# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#   Correction des bases FST et gestions DIRNOSEQ pour RFL
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @auteur : Julien PRAMIL
#' @description : 
#' - Passer les tables RFL en Lambert 93 (elles sont en Lambert 2)
#' - Corriger quelques xy de 2018 (conformément à la note utilisateurs)
#' - Ajout des XY récents par appariement au Dirnoseq (table passage Gabrielle)
#' - filtrage des ménages ayant des revenus négatifs
#' - filtrage des ménages sans XY
#' - Passage des cog en géographie 2022
#' - Suppression des cog des arrondissements communaux de Paris-Lyon-Marseille

#' Ajouter les corrections de xy de la doc filo 2018


source("R/00a_global.R")



#### Gestion table de passage dirnoseq-XY de la DSAU --------------------------------------------

#' Base fournie par la section géolocalisations
#' XY les plus récents pour chaque dirnoseq  
#' Besoin de supprimer des doublons exacts et inexacts

passageDirno <- readRDS(paste0(chemin_dirnoseq,"dirnoseq06a18.RDS"))
passageDirnoDom <- haven::read_sas("X:/HAB-Infracommunal/Temp/dirnoseq05a18dom.sas7bdat")

colnames(passageDirno) <- tolower(colnames(passageDirno)) 
setDT(passageDirno,key="dirnoseq") 
colnames(passageDirnoDom) <- tolower(colnames(passageDirnoDom))
setDT(passageDirnoDom,key = "dirnoseq") 

# Fusion des deux bases

passageDirno <- passageDirno[str_sub(dirnoseq,1,2) != '97'] #On ne conserve que la métropole
passageDirnoDom <- passageDirnoDom[str_sub(dirnoseq,1,3) %in% c('972','974')] #On ne conserve que la matrinique et la Réunion
passageDirno <- rbindlist(list(passageDirno,passageDirnoDom))
setkey(passageDirno,dirnoseq)

# Check sur le champs des départements de la base
# passageDirno[,.N,by=depcom %>% str_sub(1,2)]
# passageDirno[str_sub(depcom,1,2)=="97",.N,by=depcom %>% str_sub(1,3)]

# Suppression des lignes avec des coordonnées vides
passageDirno[is.na(x) | is.na(y),.N] #34478
passageDirno[,.N,tax]
passageDirno <- passageDirno[!is.na(x) & !is.na(y),]

# Suppression des doublons exactes (dirnoseq-coords géo) : on ne garde que la première ligne
passageDirno[duplicated(passageDirno,by=c("dirnoseq","x","y")),.N] # 68486 lignes supprimées
passageDirno <- passageDirno[!duplicated(passageDirno,by=c("dirnoseq","x","y"))]

# Doublons de Dirnosec associés à plusieurs jeux de coordonnées XY : 1678 lignes pour 836 Dirnoseq concernés
doublons_NonExacts <- passageDirno[duplicated(dirnoseq),unique(dirnoseq)]
passageDirno[dirnoseq %in% doublons_NonExacts] %>% View()
# Répartition par commune (essentiellement dans 4 communes du Maine-et-Loire 49):
passageDirno[dirnoseq %in% doublons_NonExacts,.(nb=.N),depcom][
  order(nb,decreasing = T)] %>% View
# Suppression de ces doublons (en ne gardant que le premier des deux dirnoseq dans la base)
passageDirno <- passageDirno[!duplicated(dirnoseq)]




#### Fusion avec la table de passage dirnoseq-xy de la division logements ------------------------

pass_divlog <- fread(lien_passage_divlog)

passageDirno <- passageDirno[,.(dirnoseq,x,y,millesi=millesimexy)] %>% setkey(dirnoseq)
pass_divlog <- pass_divlog[,.(dirnoseq,x,y,millesi=last_update)] %>% setkey(dirnoseq)

passageDirno <- rbind(pass_divlog,passageDirno[!pass_divlog]) %>% setkey(dirnoseq)

# Correction erreurs dans les xy (vu avec division logement - Camille Freppel)
passageDirno[x<10,x:=x*10^10]
passageDirno[y<10,y:=y*10^10]





#### Table de passage pour COG en géographie 2022 -----------------------------

# Table de passage : url = https://www.insee.fr/fr/information/2028028
unzip(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.zip"),
      exdir = paste0(chemin_input,"passage_cog"))

tabCogHisto <-
  read_excel(
    paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
    sheet = "Table de passage",
    skip = 5)
setDT(tabCogHisto,key="CODGEO_INI")
tabCogHisto <- tabCogHisto[CODGEO_INI!=CODGEO_2022,.(CODGEO_INI,CODGEO_2022)]



#### Modifications des base ménages --------------------------------------------------------

lmen <- readRDS(file = paste0(chemin_output,"/lmen.RDS"))




#### Préalable : passer les xy de RFL en Lambert 93 ==============

# La quasi totalité des xy de RFL étaient en Lambert 2 étendu (27572)
# Durée : 1h
tab_rfl <- paste0("men",c("04","05","06","07","08","09","10","11"))

lmen_reproj27572 <- lmen[tab_rfl]
lmen_reproj27572 <- lmen_reproj27572 %>% lapply(au33_reproj)
# 4600 xy n'étaient exprimés en Lambert 2 centre 
# Essentiellement à Lamballe en 2004...
# => Pas de solution évidente : suppression
rfl_proj_pb <- lmen_reproj27572 %>% lapply(au33_bboxValid,verbose = F)
rfl_proj_pb <- map2(.x = lmen[tab_rfl],.y = rfl_proj_pb,~.x[id %in% .y$met$id, ]) 
saveRDS(rfl_proj_pb,paste0(chemin_output,"qualite_inputs/rfl_proj_pb.rds"))

lmen[tab_rfl] <- map2(.x=lmen_reproj27572,
                      .y=rfl_proj_pb,
                      ~.x[!id %in% .y$id]
                      )
lmen_reproj27572 %>% lapply(au33_bboxValid,verbose = F)


# lmen_corr_rfl <- map2(.x=lmen_reproj27572,
#                       .y=rfl_proj_pb,
#                       ~rbind(
#                         .x[!id %in% .y$id],
#                         .y)
# )

# # Un seul local non projeté en 27572 ni en 27562 : on le vire
# lmen_corr_rfl %>% lapply(au33_bboxValid, verbose=F)
# lmen_corr_rfl$men11 <- lmen_corr_rfl$men11[id!="4400927295",]

# map2(lmen[tab_rfl],lmen_corr_rfl,~nrow(.x)-nrow(.y))
# lmen[tab_rfl] <- lmen_reproj27572


rm(list = c("lmen_reproj27572","rfl_proj_pb")) ; gc()

#### Check projections bases Filo (avec DOM)
# Pb en 2018

annees_filo_dom <- paste0("men",c("14","15","16","17","18","19"))

lmen %>% lapply(au33_bboxValid)
# 
# lmen$men12 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men13 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men14 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men15 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men16 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men17 %>% au33_bboxValid(verbose = F) # 0 problème
# lmen$men18 %>% au33_bboxValid(verbose = F) # 340+37+39  problèmes
# lmen$men19 %>% au33_bboxValid(verbose = F) # 0 problème

#### Corriger les 416 xy / 10^10 de Filo 2018 (df note utilisateurs)

lmen$men18[x < 1 | y < 1,.N]
lmen$men18[x<1,x:=x*10^10]
lmen$men18[y<1,y:=y*10^10]

lmen %>% lapply(setkey,"id")

lmen %>% lapply(au33_bboxValid,verbose=F)


#### Check des projections sur la table de passage
# 13118  points bizarres dans la table de passage DSAU DOM côté La Réunion (2006, 2007, 2009)
# Pas de solution => suppression de passageDirno
passage_proj_pb <- passageDirno %>% au33_bboxValid(vardirno="dirnoseq",verbose = F)
passage_proj_pb$reun[,.N,millesi]
saveRDS(passage_proj_pb,paste0(chemin_output,"qualite_inputs/passage_proj_pb.rds"))
passageDirno <- passageDirno[! dirnoseq %in% passage_proj_pb$reun$dirnoseq]


rm("passage_proj_pb")
gc()

#### Ajout des XY par ceux de la table de passage ================


deb <- Sys.time()
lmen <- lmen %>% lapply(function(dt){
  res <- merge(dt,passageDirno,by.x="id",by.y="dirnoseq",all.x=T,suffixes=c("","_pass"))
  res[!is.na(x_pass) & !is.na(y_pass),':='(x=x_pass,y=y_pass)]
  res[,':='(x_pass=NULL,y_pass=NULL,millesi=NULL)]
  stopifnot(res[,.N]==dt[,.N])
  return(res)
})
Sys.time()-deb #16 mn


#### filtrage si absence de XY ===========================

# Sauv men sans coordonnées (documentation)
lmen_coordNA <- lmen %>% map(~.x[is.na(x) | is.na(y)])
lmen_coordNA %>% map(~.x[,.N])
saveRDS(lmen_coordNA,paste0(chemin_output,"data_docu/lmen_coordNA.RDS")) 

# Filtrage
lmen <- lmen %>% lapply(function(dt){
  return(dt[!is.na(x) & !is.na(y) & !is.na(id)])
})

#### Vérifier absence de xy abberrants (hors bbox de leur territoires)
lmen %>% lapply(au33_bboxValid)


#### Remplacement COG fusionnés ================================

lmen <- lmen %>%  map(~setkey(.x,"depcom")) 
tabCogHisto <- tabCogHisto %>% setkey("CODGEO_INI")

lmen <- lmen %>% lapply(function(dt){
  res <-
    merge(dt,
          tabCogHisto,
          by.x = "depcom",
          by.y = "CODGEO_INI",
          all.x = T)
  stopifnot(res[,.N]==dt[,.N])
  res[!is.na(CODGEO_2022), depcom := CODGEO_2022][, CODGEO_2022 := NULL]
  return(res)
})

#### Suppression des COG d'arrondissement ===================

lmen <- lmen %>% lapply(function(dt){
  dt[depcom %in% as.character(75101:75120),depcom:="75056"]
  dt[depcom %in% as.character(69381:69389),depcom:="69123"]
  dt[depcom %in% as.character(13201:13216),depcom:="13055"]
})
                        
#### Suppression ménages à revenus négatifs ===================

lmen <- lmen %>% lapply(function(dt){
  return(dt[revdecucm>=0,])
})




#### Sauvegarde ----------------------------------------------------------

lmen %>% map_dbl(~.x[,sum(is.na(id))]) %>% sum
lmen %>% map_dbl(~.x[,sum(is.na(x))]) %>% sum
lmen %>% map_dbl(~.x[,sum(is.na(y))]) %>% sum

saveRDS(lmen,paste0(chemin_output,"lmen_cor_25112022.RDS"))

#A tester avec : 
test_lmen_corr <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))



