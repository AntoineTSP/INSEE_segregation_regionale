# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                         Préparation des bases ménages  
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @description : 
#' - Importation des bases ménages
#' - Standardisation des variables
#' - Restrictions à la métropole + Martinique + Réunion
#' - Passage id-Dirnoseq sur les bases Filosofi (2014-2019)
#' @details : Préalable : conversion et standardisation des bases menages via SAS : "export_csv.sas"
#' @auteur : Julien PRAMIL 


source("R/00a_global.R")

# L

#### *********************************************************************************
####                Importation des données                                  ---------
#### *********************************************************************************

fichiers <- dir(chemin_basesmen)
millesimes <- str_extract(fichiers,"\\d+")
fichiers <- fichiers[order(millesimes)]

# 6 min

cl <- makeCluster(8)
clusterExport(cl, "fichiers")
clusterEvalQ(cl, source("R/00a_global.R"))
deb <- Sys.time()
lmen <- parLapply(cl,fichiers,function(tab){
  fread(paste0(chemin_basesmen,tab))
})
fin <- Sys.time()
stopCluster(cl)

names(lmen) <- str_remove_all(fichiers,".csv")

#C'est long(5min)

#### *********************************************************************************
####                Standardisation et restrictions champ                    ---------
#### *********************************************************************************


#### Contrôle de la qualité des bases --------------------------------------------

lmen <- lmen %>% lapply(setmyclass) # On transforme la classe des variables si besoin

# Restriction aux seuls départements valides (d'après depcom)
#France enti�re
#dep_valid_met <- c(paste0("0",1:9),10:19,c("2A","2B"),21:95)
#dep_valid_dom <- c(972,974)
#lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met | str_sub(depcom,1,3) %in% dep_valid_dom])

#Restriction au Grand Est
dep_valid_met <- c(paste0("0",8),10,51,52,54,55,57,67,68,88)
dep_valid_dom <- c()
lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met])

#Apr�s, j'ai besoin de l'acc�s Filosofi



#### *********************************************************************************
####                Ajout des dirnoseq sur Filo 2014-2019                    ---------
#### *********************************************************************************


#' Méthode validée par le pôle RFS (mail Hélène Génuit du 20/04/2022)
#' Utilisation des bases "ident_menagesXX.sas7bdat" dans le coffre de production HAB-FILOSOFI-PROD
#' Remarque : besoin de dézipper des fichiers SAS volumineux...
#' Remarque 2 : Pas de table de passage en 2012 et 2013 : utilisation base ménage entière avec Dirnoseq

#### Dezipage   ============= 

# dirextract <- chemin_input %>% paste0("passage_id_filo")
# dir.create(dirextract)
# 
# liste_dezip_id <-
#   list(
#      `14` = list(
#       passid = "X:/HAB-FILOSOFI-PROD/bases14/r_menages.zip",
#       file = "r_menages/ident_menages14.sas7bdat",
#       dirextract = paste0(dirextract, "/14")
#     ),
#     `15` = list(
#       passid = "X:/HAB-FILOSOFI-PROD/bases15/r_menages.zip",
#       file = "r_menages/ident_menages15.sas7bdat",
#       dirextract = paste0(dirextract, "/15")
#     )
#   )
# 
# 
# cl <- makeCluster(4)
# clusterExport(cl, "liste_dezip_id")
# clusterEvalQ(cl, source("R/00a_global.R"))
# 
# parLapply(cl,liste_dezip_id,function(l){
#   unzip(zipfile = l$passid,
#         files = l$file,
#         exdir = l$dirextract)
# })
# stopCluster(cl)

#### Importation et jointure   =============

liste_chemin_passId <- list(
  `14`=paste0(chemin_input,"passage_id_filo/14/r_menages/ident_menages14.sas7bdat"),
  `15`=paste0(chemin_input, "passage_id_filo/15/r_menages/ident_menages15.sas7bdat"),
  `16`=paste0(chemin_filoProd,"bases16/r_menages/ident_menages16.sas7bdat"),
  `17`=paste0(chemin_filoProd,"bases17/r_menages/ident_menages17.sas7bdat"),
  `18`=paste0(chemin_filoProd,"bases18/r_menages/ident_menages18.sas7bdat"),
  `19`=paste0(chemin_filoProd,"bases19/r_menages/ident_menages19.sas7bdat")
)

cl <- makeCluster(6)
clusterExport(cl, "liste_chemin_passId")
clusterEvalQ(cl, source("R/00a_global.R"))
deb <- Sys.time()
lpass_dirno <- parLapply(cl,liste_chemin_passId,function(l){
  haven::read_sas(l)
})
fin <- Sys.time()
print(fin-deb)
stopCluster(cl)


lpass_dirno <- lpass_dirno %>% lapply(function(tab){
  dt <- copy(tab)
  dt <- as.data.table(tab)
  colnames(dt) <- str_to_lower(colnames(dt))
  setnames(dt,old="identifiant","id")
  setkey(dt,id)
  return(dt)
})

names_filo1219 <- c("men14","men15","men16","men17","men18","men19")
# names_filo1219 <- c("men14")

lmen[names_filo1219] <- map2(.x = lmen[names_filo1219],
                             .y = lpass_dirno,
                             function(tabmen, tabpass) {
                               setkey(tabmen, "id")
                               res <- merge(tabmen, tabpass, by = "id", all.x = T)
                               message("Présence de ", res[is.na(dirnoseq), .N], " ménage(s) sans dirnoseq")
                               return(res)
                             })

lmen[names_filo1219] %>% map(~.x[is.na(dirnoseq)]) # 1 ménage en 2014, 2017 et 2019 (id="0")

lmen[names_filo1219] <- 
  lmen[names_filo1219] %>% map(
    ~.x[,id:=dirnoseq][,dirnoseq:=NULL]
    )


#### *********************************************************************************
####                        Sauvegarde                                       ---------
#### *********************************************************************************

lmen <- lmen %>% lapply(function(dt) {
  dt[, dep := str_sub(depcom, 1, 2)]
  dt[dep == "97", dep := str_sub(depcom, 1, 3)]
  setkey(dt, id)
  return(dt)
})

saveRDS(lmen,paste0(chemin_output,"lmen.RDS"))

#A tester avec :
test_lmen <- readRDS(paste0(chemin_output,"lmen.RDS"))


