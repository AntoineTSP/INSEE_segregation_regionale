# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                 Préparation du Rmd de cylindrage
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @auteur : Julien PRAMIL
#' @details Préparation des tableaux et graphiques pour le Rmd de cylindrage
#' (contrôle de la stabilité de la distribution des revenus entre les millésimes)

source("R/00a_global.R")

# Table de passage entre les départements et les régions
correspDepReg_lib <- fread(paste0(chemin_input,"depreg/correspDepReg.csv"))
correspDepReg <- correspDepReg_lib[,.(dep,reg)]

lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS")) # Base men après corrections

#Restriction au Grand Est
dep_valid_met <- c(paste0("0",8),10,51,52,54,55,57,67,68,88)
dep_valid_dom <- c()
lmen <- lmen %>% map(~.x[str_sub(depcom,1,2) %in% dep_valid_met])

#### Préparation des données -------------------------------------------------------

splitreg <- function(dt,passage=correspDepReg){
  dt_c <- copy(dt)
  dt_c[,dep:=str_sub(depcom,1,2)]
  dt_c[dep=="97",dep:=str_sub(depcom,1,3)]
  dt_c <- merge(dt_c,passage,by="dep",all.x=T)
  stopifnot(dt_c[,.N]==dt[,.N])
  res <- split(dt_c,by="reg")
  res <- res %>% map(~.x[,.(revdecucm,nbpersm)])
  return(res)
}

# Calcul du 99e quantile de revenu par UC (pour gérer les valeurs extrêmes sur le graph de densité)
fdensite <- function(dt){
  quant99 <- dt[,quantile(revdecucm, probs = 0.99)]
  men <- dt[revdecucm<quant99]
  men[,poids:=nbpersm/sum(nbpersm)]
  dens <- density(x=men$revdecucm,weights = men$poids)
  # # Ajout des lignes à densiteTab
  densiteTab <- data.frame(x=dens$x,y=dens$y)
  return(densiteTab)
}

fdeciles <- function(dt){
  men <- copy(dt)
  men[,poids:=nbpersm/sum(nbpersm)]
  
  quant <-
    reldist::wtd.quantile(
      x = men$revdecucm,
      weight =  men$nbpersm,
      q = seq(0.1, 0.9, 0.1))
      
  dt_quant <-
    data.table(decile=paste0("decile", 1:9),
               val = quant)
  return(dt_quant)
}

lmen_split <- lmen %>% lapply(splitreg)


#### Densité ==================

# Régions **************

lmen_densite <- lmen_split %>% lapply(function(l){
  l %>% lapply(fdensite) 
}) 

lmen_densite <- lmen_densite %>% map(~rbindlist(.x,idcol = "reg"))
data_dens <- lmen_densite %>% rbindlist(idcol = "annee")

# France metro ****************

data_dens_fr <- lmen %>% 
  map(~.x[str_sub(depcom,1,2)!="97"]) %>% #uniquement en métropole
  lapply(fdensite) %>% 
  rbindlist(idcol = "annee")

data_dens_fr[,reg:="france_met"]

# Bind et save ***************
data_dens <- rbind(data_dens,data_dens_fr) %>% arrange(annee,reg)
fwrite(data_dens,paste0(chemin_output,"cylindrage/data_dens.csv"))


#### Deciles ===============

# Région ***********
lmen_deciles <- lmen_split %>% lapply(function(l){
  l %>% lapply(fdeciles) 
}) 

lmen_deciles <- lmen_deciles %>% map(~rbindlist(.x,idcol = "reg"))
data_deciles <- lmen_deciles %>% rbindlist(idcol = "annee")

# France metro *************
data_deciles_fr <- lmen %>% 
  map(~.x[str_sub(depcom,1,2)!="97"]) %>% #uniquement en métropole
  lapply(fdeciles) %>% 
  rbindlist(idcol = "annee")
data_deciles_fr[,reg:="france_met"]

# Bind et save *****************
data_deciles <- rbind(data_deciles,data_deciles_fr) %>% arrange(annee,reg,decile)
fwrite(data_deciles,paste0(chemin_output,"cylindrage/data_deciles.csv"))


####  Confection des graphiques dynamiques en Plotly ---------------------------------------------------

data_dens <- fread(paste0(chemin_output,"cylindrage/data_dens.csv"))
data_deciles <- fread(paste0(chemin_output,"cylindrage/data_deciles.csv"))

passageLibReg <- correspDepReg_lib[,.(reg,libReg)] %>% 
  distinct() %>% 
  rbind(data.table(reg="france_met",libReg="France metro."))

vec_reg <- unique(data_deciles$reg)
vec_reg <- c("france_met",vec_reg[vec_reg!="france_met"])


# Boutons Plotly ========================

buttons <- vec_reg %>% map( ~ list(method = "restyle",
                                   args = list("transforms[0].value", .x),
                                   label = passageLibReg[reg==.x,libReg])
)

updatemenus <-  list(
  list(
    type = 'dropdown',
    active = 0,
    buttons = buttons
  )
)

# quantiles ===================

quantileChart <- plot_ly(
  data = data_deciles,
  x = ~ decile,
  y = ~ val,
  color =  ~ annee,
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '=',
      value = passageLibReg[reg=="france_met",libReg]
    )
  )) %>% layout(
    updatemenus = updatemenus
  ) 

saveRDS(quantileChart,file = paste0(chemin_output,"cylindrage/quantileChart.RDS"))

#### densite ===========================

densiteChart <- plot_ly(
  data = data_dens,
  x = ~ x,
  y = ~ y,
  color =  ~ annee,
  type = "scatter",mode="lines",
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '=',
      value = passageLibReg[reg=="france_met",libReg]
    )
  )) %>% layout(
    updatemenus = updatemenus
  ) 

saveRDS(densiteChart,file = paste0(chemin_output,"cylindrage/densiteChart.RDS"))


#### Lancement Rmd --------------------------------------------------

rmarkdown::render('R/03b_cylindrage.Rmd', 
                  output_file = paste0(chemin_output,"cylindrage/cylindrage.html"))
