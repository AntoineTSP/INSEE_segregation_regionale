# ::::::::::::::::::::::::::::::::::::::::::::::::::::
#      Contrôle de qualité des bases ménages
# ::::::::::::::::::::::::::::::::::::::::::::::::::::


#' @description : 
#' - Contrôle de la qualité des données (préparation des données et génération d'un rapport)
#' @auteur : Julien PRAMIL 


source("R/00a_global.R")

# Table passage dep-reg
correspDepReg_lib <- fread(paste0(chemin_input,"depreg/correspDepReg.csv"))
correspDepReg <- correspDepReg_lib[,.(dep,libReg)] %>% distinct()

lmen <- readRDS(paste0(chemin_output,"/lmen.RDS"))
#5min

#Restriction au Grand Est
dep_valid_met <- c(paste0("0",8),10,51,52,54,55,57,67,68,88)
dep_valid_dom <- c()
lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met])

#### ***********************************************************
####         Fonction de contrôle de la qualité             ----
#### ***********************************************************

#' zero_reg
#' @description : calculer la part de ménages avec des revenus 
#' strictement nuls dans chaque département
#' @param dt objet data.table avec variable "dep"
#' @param passage : table de passage depreg
#' @return table départementale

zero_reg <- function(dt,passage=correspDepReg){
  setkey(dt,dep)
  setkey(passage,dep)
  dt <- merge(dt,passage,by="dep",all.x = T)
  dt <- dt[,.(nbzero=sum(revdecucm==0,na.rm = T),pop=.N),by="libReg"]
  dt[,partzero:=100*nbzero/pop]
  return(dt)
}


#' ctrl_quali
#' @description : production des différents indicateurs de qualité
#' @param tab objet data.tabl
#' @return liste de résultats

ctrl_quali <- function(tab){
  dt <- copy(tab)
  res <- list(
    nbdoublons = dt[duplicated(id),.N],
    nbmen = dt[,.N],
    nbNA = dt[,lapply(.SD,function(var) sum(is.na(var)))],
    nbvide = dt[,lapply(.SD,function(var) sum(var[!is.na(var)] %in% c("",".")))],
    min = dt[,lapply(.SD,function(var) min(var,na.rm = T)),.SDcols=c("revdecucm","nbpersm")],
    max = dt[,lapply(.SD,function(var) max(var,na.rm = T)),.SDcols=c("revdecucm","nbpersm")],
    revNul = list(tot=dt[revdecucm==0,.N],
                  tabreg=zero_reg(dt)),
    revNegat = dt[revdecucm<0,.N] ,
    nbMenDom = dt[dep %in% c("972","974"),.N]
  )
  return(res) 
}

#### ***********************************************************
####   Enregistrement des données de qualité dans lquali    ----
#### ***********************************************************

deb <- Sys.time()
cl <- makeCluster(16)
clusterExport(cl, "ctrl_quali")
clusterExport(cl, "zero_reg")
clusterExport(cl, "correspDepReg")
clusterEvalQ(cl, source("R/00a_global.R"))
lquali <- parLapply(cl,lmen,ctrl_quali)
stopCluster(cl)
duree_para <- Sys.time()-deb #21mn (30% plus rapide...)



#### Sauvegarde ====================================================
saveRDS(lquali,file=paste0(chemin_output,"qualite_inputs/lquali.RDS"))
# saveRDS(lmen,file = paste0(chemin_output,"/lmen.RDS"))



#### Production du rapport de qualité des bases ********************************************

rmarkdown::render('R/01c_controle_qualite.Rmd', 
                  output_file = paste0(chemin_output,"/qualite_inputs/controle_qualite.html"))

# Verifications a posteriori 
lmen$men07[is.na(revdecucm),.N,by=nbpersm]
lmen$men07[nbpersm==0,.N]
lmen$men07[nbpersm==0 & !is.na(revdecucm),.N]
lmen$men14[is.na(x)]

#A tester avec :
test_lquali <- readRDS(paste0(chemin_output,"qualite_inputs/lquali.RDS"))

