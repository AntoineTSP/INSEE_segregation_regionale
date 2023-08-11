#### ::::::::::::::::::::::::::::::::::::::::::
####      EPCI versus pôle AAV sur Nantes
#### ::::::::::::::::::::::::::::::::::::::::::

# Pour Nantes :
aav_cible <- "008"
epci_cible <- "244400404"

#### Chargement de l'environnement et des bases ménages
source("R/00a_global.R",encoding = "UTF-8")
lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))
liste_taille_carreaux <- list(200) # unqiement pour la grille de 200m


#### Chargement données faire fonctionner les fonctions ========================
qp_contours <- st_read(paste0(chemin_input,"qp/qp_franceentiere_2021.shp"),quiet = T)
qp_contours <- qp_contours %>% filter(str_sub(dep,1,2) != "97" |str_sub(dep,1,3) %in% c("972","974")) 
comQP <- fread(paste0(chemin_input,"qp/data_table_appartenance_geo_QPV_2021.csv"))

com_zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                       sheet = "Composition_communale",skip = 5)
scissions <- read_excel(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
                        sheet = "Liste des scissions",skip = 5)
sfcom <- st_read(paste0(chemin_input,"com/commune_franceentiere_2022.gpkg"))
zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                   sheet = "AAV2020",skip = 5)
epci <- read_excel(paste0(chemin_input,"epci/Intercommunalite_Metropole_au_01-01-2019.xls"),
                   sheet = "Composition_communale",skip = 5)


#### ZAAV ou EPCI ==============================

zaav <- zaav %>% filter(AAV2020==aav_cible)
com_poles <- com_zaav %>% filter(AAV2020 %in% zaav$AAV2020 & CATEAAV2020 %in% c("11","12"))
com_epci <- epci %>% filter(EPCI==epci_cible)

sf_com_poles <- sfcom %>% filter(code %in% com_poles$CODGEO)
sf_pole <- sf_com_poles  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))
sfqp_pole_inters <- sf_pole %>% st_intersection(qp_contours %>% select(code,geometry))

sf_com_epci <- sfcom %>% filter(code %in% com_epci$CODGEO)
sf_epci <- sf_com_epci  %>% group_by(aav20) %>% summarize(geometry=st_union(geometry))
sfqp_epci_inters <- sf_epci %>% st_intersection(qp_contours %>% select(code,geometry))

com_epci %>% setnames(old="EPCI",new="AAV2020") #leurre pour les fonctions infra


#### Filtrage ménages des pôles par les cog   ================================


lmen_poles <- lmen %>% map(~.x[depcom %in% unique(com_poles$CODGEO)])
lmen_epci <- lmen %>% map(~.x[depcom %in% unique(com_epci$CODGEO)])


lmen_poles <- lapply(lmen_poles,
                     f_inspire_filtre,
                     liste_taille_carreaux,
                     com_poles,
                     nbmen_min)


#### Carroyage et résultats finaux   =====================================

lmen_epci <- lapply(lmen_epci,
                    f_inspire_filtre,
                    liste_taille_carreaux,
                    com_epci,
                    nbmen_min)


res_poles <- lmen_poles %>% map(~f_carroyage(200,.x,nb_tranches,passageComZE = com_poles,ZE_ssQP=NULL,sfqp_pole_inters))
res_epci <- lmen_epci %>% map(~f_carroyage(200,.x,nb_tranches,passageComZE = com_epci,ZE_ssQP=NULL,sfqp_epci_inters))




#### Graphique comparaison ségragation totale Theil ============================

dta <- tibble(annee=rep(2004:2019,2),
       agglo=rep(c("pole","epci"),each=16),
       valeurs=c(res_poles %>% map_dbl(~.x$segreg$theil$segTot),
                 res_epci %>% map_dbl(~.x$segreg$theil$segTot))
       )


dta <- dta %>% group_by(agglo) %>% mutate(valeurs_rebase=100*valeurs/valeurs[1])

ggp <- ggplot(data=dta,aes(x=annee,y=valeurs,color=agglo))+geom_line()
ggp <- ggplotly(ggp)
htmlwidgets::saveWidget(ggp,file="X:/HAB-PSAR-AU-AU33-DEV/au33_v2/divers/segreg_epci_nantes.html")

ggp_rebase <- ggplot(data=dta,aes(x=annee,y=valeurs_rebase,color=agglo))+geom_line()
ggp_rebase <- ggplotly(ggp_rebase)
htmlwidgets::saveWidget(ggp_rebase,file="X:/HAB-PSAR-AU-AU33-DEV/au33_v2/divers/segreg_epci_nantes_rebase.html")




# Chute de code : 
men19 <- lmen$men19


#### ZAAV ========================
com_zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                       sheet = "Composition_communale",skip = 5)
zaav <- read_excel(paste0(chemin_input,"zaav/AAV2020_au_01-01-2022.xlsx"),
                   sheet = "AAV2020",skip = 5)
scissions <- read_excel(paste0(chemin_input,"passage_cog/table_passage_geo2003_geo2022.xlsx"),
                        sheet = "Liste des scissions",skip = 5)
sfcom <- st_read(paste0(chemin_input,"com/commune_franceentiere_2022.gpkg"))


#### ****************************************************
####      Definition des pôles des grandes AAV       ----
#### ****************************************************


#### Selection +50 000 hab en metropole ou martinique ou Reunion ===========
zaav <- zaav %>% filter(AAV2020=="008")
com_poles <- com_zaav %>% filter(AAV2020 %in% zaav$AAV2020 & CATEAAV2020 %in% c("11","12"))


#### Check absence communes scissionnées dans les grands pôles ================
com_poles[com_poles$CODGEO %in% unique(scissions$COM_INI,scissions$COM_FIN),]


#### EPCI

epci <- read_excel(paste0(chemin_input,"epci/Intercommunalite_Metropole_au_01-01-2019.xls"),
                   sheet = "Composition_communale",skip = 5)
com_epci <- epci %>% filter(EPCI=="244400404")



####  Filtrage ménages des pôles par les cog *****************

tcar <- 200
crs <- 2154

men_epci <- men19 %>% filter(depcom %in% com_epci$CODGEO)
men_pole <- men19 %>% filter(depcom %in% com_poles$CODGEO)

men_pole <- add_centroides(men_pole,tcar)
men_pole <- add_inspire(men_pole,tcar,crs)
men_pole[,nb:=.N,by=idInspire]
men_pole[,filtre:=F]
men_pole[nb>=nbmen_min,filtre:=T]
car_pole <- men_pole %>% select(idInspire,x=x_centroide,y=y_centroide,nb,filtre) %>% distinct()
car_pole_filtre <- car_pole %>% filter(filtre)
sfcar_pole_filtre <- btb::dfToGrid(car_pole_filtre,sEPSG=crs,iCellSize = tcar)
mapview(sfcar_pole_filtre)

men_epci <- add_centroides(men_epci,tcar)
men_epci <- add_inspire(men_epci,tcar,crs)
men_epci[,nb:=.N,by=idInspire]
men_epci[,filtre:=F]
men_epci[nb>=nbmen_min,filtre:=T]
car_epci <- men_epci %>% select(idInspire,x=x_centroide,y=y_centroide,nb,filtre) %>% distinct()
car_epci_filtre <- car_epci %>% filter(filtre)
sfcar_epci_filtre <- btb::dfToGrid(car_epci_filtre,sEPSG=crs,iCellSize = tcar)
mapview(sfcar_epci_filtre)


# Cartographie
absents_du_pole <- setdiff(sfcar_epci_filtre$idInspire,sfcar_pole_filtre$idInspire)
absents_de_epci <- setdiff(sfcar_pole_filtre$idInspire,sfcar_epci_filtre$idInspire)
length(absents_du_pole)
length(absents_de_epci)
nrow(sfcar_epci_filtre)
nrow(sfcar_pole_filtre)
sfcar_epci_filtre$carreau_pole <- !sfcar_epci_filtre$idInspire %in% absents_du_pole 

mapview(sfcar_epci_filtre,zcol = "carreau_pole") %>% mapshot("Z:/AU33/epci_nantes.html")

sfcar_epci_filtre %>% group_by(carreau_pole) %>% summarise(nbmen=sum(nb))
sfcar_epci_filtre %>% filter(!carreau_pole) %>% summarise(nbmen_moy_hpole=mean(nb))
sfcar_epci_filtre %>% filter(carreau_pole) %>% summarise(nbmen_moy_hpole=mean(nb))










