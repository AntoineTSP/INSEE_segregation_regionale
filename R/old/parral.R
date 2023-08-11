# Chargement environnement et packages
setwd("U:/developpements/au33_v2/au33_dev")
source("R/00a_global.R")

# Import data
toto <- readRDS(paste0(chemin_output,"toto.rds"))
toto2 <- toto %>% unlist(recursive = F) %>% unlist(recursive = F)%>% unlist(recursive = F)

data_para <- readRDS(paste0(chemin_output,"data_para.rds"))
nb_tranches <- data_para$nb_tranches
com_poles <- data_para$com_poles
pole_ssQP <- data_para$pole_ssQP

# -----------------------------------------------------------------------------------------

# apply ============================
deb <- Sys.time()
res1 <- lapply(toto2,f_carroyage,nb_tranches,passageComZE = com_poles,ZE_ssQP=pole_ssQP)
fin <- Sys.time()
duree_lapply <- fin-deb

# ParLapply ============================
cl <- makeCluster(10)
clusterExport(cl, "com_poles")
clusterExport(cl, "pole_ssQP")
clusterExport(cl, "nb_tranches")
clusterExport(cl, "sfqp_pole_inters")

clusterEvalQ(cl, setwd("U:/developpements/au33_v2/au33_dev"))
clusterEvalQ(cl, source("R/00a_global.R"))

deb <- Sys.time()
res3 <- parLapply(cl,toto2,f_carroyage,nb_tranches,passageComZE = com_poles,ZE_ssQP=pole_ssQP)
fin <- Sys.time()
duree_parLapply <- fin-deb
stopCluster(cl)



# ParLapply sans f_carroyage

f <- function(tab){
  Sys.sleep(2)
  return(0)
}
f(toto2$men06.006.200.sansFiltrage)


deb <- Sys.time()
res1 <- lapply(toto2,f)
fin <- Sys.time()
duree_apply <- fin-deb


cl <- makeCluster(5)
clusterExport(cl, "f")

deb <- Sys.time()
res3 <- parLapply(cl,toto2,f)
stopCluster(cl)
fin <- Sys.time()
duree_parLapply <- fin-deb

length(toto2)






