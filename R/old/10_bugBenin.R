# ****************************************************************************
#                   Carreaux positionnés loin de leur agglo
# ****************************************************************************


# Résumé : 
# Mail du 11/10/2022 de David Veil (DR IDF)
# Détection de carreaux appartenant à l'agglomération parisienne mais dont l'id Inspire est
# situé loin de la France métropolitaine
# Investigations : 
#  - lcouches est affectée par le problème (comme les bases de l'application carto)
#  - Affecte les bases lissées et l'app de carto 3D
#  - Problèmes présents dans lmen_cor.RDS (version corrigée de la base ménage)
#  - Les xy foireux ne semblent pas provenir des tables de passage xy-dirnoseq
#  - Les xy foireux sont présents dans les bases ménages (RFL sûr, Filosofi à voir)


# Exemple problème dans les bases ménages : 

#  DEPCOM = 01004
#  DIRNOSEQ = 0100002327  
#  x = 833895.9 
#  y = 2111191   



# BBOX de la France métropolitaine

reg <- sf::st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2021/franceentiere/reg_franceentiere_2021.shp")
metro <- reg %>% filter(!code %in% c("01","02","03","04","06")) %>% sf::st_union()
mart <- reg %>% filter(code =="02") %>% sf::st_union()
reun <- reg %>% filter(code =="04") %>% sf::st_union()

metro_bbox <- metro %>% sf::st_transform(proj_lambert93) %>%  sf::st_buffer(50) %>% st_bbox()
mart_bbox <- mart %>% sf::st_transform(proj_mart) %>%  sf::st_buffer(50) %>% st_bbox()
reun_bbox <- reun %>% sf::st_transform(proj_reun) %>%  sf::st_buffer(50) %>% st_bbox()


#### Détection des cas dans les bases ménages RFL-Filo : 
lmen <- readRDS(file = paste0(chemin_output,"/lmen.RDS"))

xy_pb <- lmen %>% map(
  ~ .x[ (y>metro_bbox[["ymax"]] | y<metro_bbox[["ymin"]] | 
         x>metro_bbox[["xmax"]] | x<metro_bbox[["xmin"]]) &
          ( str_sub(id,1,2) != c("97"))]
)

xy_pb$men18[1:10,]
xy_pb %>% map(~.x[,.N])


# Check système de pro Lambert 2 : 
rfl04 <- lmen$men04
rfl04_2154 <- rfl04[!is.na(x) & !is.na(y)] %>% 
  st_as_sf(coords=c("x","y"),crs=27572) %>% 
  st_transform(2154) %>% 
  mutate(x=st_coordinates(geometry)[,1],
         y=st_coordinates(geometry)[,2]) %>% 
  st_drop_geometry()
  


~ .x[ (y>metro_bbox[["ymax"]] | y<metro_bbox[["ymin"]] | 
         x>metro_bbox[["xmax"]] | x<metro_bbox[["xmin"]]) &
        ( str_sub(id,1,2) != c("97"))]


# Check base lissees pour carto
datalissee <- readRDS("X:/HAB-PSAR-AU-AU33-DEV/au33_v2/data/output/carto3d/001_200.rds")
datalissee %>% filter(annee == "04") %>% head()


# Même problèmes sur les données lissées (sous AUS)



# Conclusions
#  - lcouches avec des carreaux hors de France
#  - Affecte les bases lissées et l'app de carto 3D
#  - Problèmes présents dans lmen_cor.RDS (version corrigée de la base ménage)



passageDirno %>% filter(Y<3000000 & str_sub(DIRNOSEQ,1,2)!="97")
pass_divlog[y<3000000 & str_sub(dirnoseq,1,2)!="97"] %>% View()

passageDirno <- fread(paste0(chemin_dirnoseq,"passageDirnoCorr.csv"))
passageDirno[y<3000000 & str_sub(dirnoseq,1,2)!="97"] %>% View()

lmen$men04[y<3000000 & str_sub(id,1,2)!="97"] %>% View()

rfl04 <- fread("X:/HAB-PSAR-AU-AU33-DEV/au33_v2/data/input/bases_menages/men04.csv")

rfl04[y<3000000 & str_sub(id,1,2)!="97"] %>% View()


#### Quantifier nombre de carreaux problèmatiques -------------

lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))



tata <- lcouches %>% 
  purrr::modify_depth(2,"200") %>% 
  purrr::modify_depth(2,"carreaux")

tata04 <- tata$men08


taille_carreau <- 200L

toto <- tata04 %>% map_dfr(
  ~ .x %>% mutate(
    x = idInspire %>% str_extract("(?<=E)\\d+$") %>% as.numeric() + taille_carreau/2,
    y = idInspire %>% str_extract("(?<=N)\\d+(?m)") %>% as.numeric() + taille_carreau/2) %>% 
    au33_bboxValid(vardirno = "idInspire",verbose = F)
)

toto




# ***************************

# Problème de reprojection
# men04 : 2200065903 

men04_test <- readRDS(file = paste0(chemin_output,"/lmen.RDS"))["men04"]
toto <- men04_test$men04[id=="2200065903"]

toto %>% st_as_sf(coords=c("x","y"),crs=27572) %>% st_transform(2154)

tata <- men04_test$men04 %>% au33_reproj(from_proj = 27562)
tata %>% au33_bboxValid()



#### Projection de la table de passage de Gabrielle ***********************


passageDirno <- readRDS(paste0(chemin_dirnoseq,"dirnoseq06a18.RDS"))
colnames(passageDirno) <- tolower(colnames(passageDirno)) 
setDT(passageDirno,key="dirnoseq") 
toto <- passageDirno %>% au33_bboxValid(vardirno = "dirnoseq",verbose = F)
pb_mart <- toto$mart

pb_mart[1:1000] %>% au33_reproj(from_proj = 4559, to_proj = proj_mart,keyvar="dirnoseq")
fwrite(pb_mart,"U:/temporaire/au33_dev/pb_mart.csv")


#### Tables de construction de la table de passage de Gabrielle *******

passageDirnoDom <- haven::read_sas("X:/HAB-Infracommunal/Temp/dirnoseq05a18dom.sas7bdat")
colnames(passageDirnoDom) <- tolower(colnames(passageDirnoDom))
setDT(passageDirnoDom,key = "dirnoseq") 

# Nombre de ménages martinique
mart_passDom <- passageDirnoDom[dirnoseq %>% str_sub(1,3) == "972",]
mart_pass <- passageDirno[dirnoseq %>% str_sub(1,3) == "972",]
mart_pass[!mart_passDom] # 561 dirnoseq pas retrouvé dans la table de passage DOM

# Nombre de ménages Réunion
reun_passDom <- passageDirnoDom[dirnoseq %>% str_sub(1,3) == "974",.N]
reun_pass <- passageDirno[dirnoseq %>% str_sub(1,3) == "974",.N]
reun_pass[!reun_passDom] # 0 dirnoseq pas retrouvé dans la table de passage DOM





# Carte des reprojection 27562


dta <- relicat_reproj27562$men04 %>% st_as_sf(coords=c("x","y"),crs=proj_lambert93)
mapview(dta) %>% mapshot("Z:/AU33/reproj27562/carte.html") 



toto <- rfl_proj_pb %>% rbindlist(idcol = "annee")
toto[,dep:=str_sub(depcom,1,2)]
toto[,.N,.(annee,dep)]


rfl_proj_pb$men04[,.N,depcom]
# Ils viennent tous de Lamballe


# Table dpassageDirno
toto <- problemes_passage %>% rbindlist(idcol = "territoire")
toto[,.N,.(millesi,territoire)]
toto[,depcom:=str_sub(dirnoseq,1,5)]
toto[,.N,.(millesi,depcom)]




# 
lmen_coordNA_old <- readRDS(paste0(chemin_output,"data_docu/lmen_coordNA.RDS"))


# Test xy / département

lmen <- readRDS(paste0(chemin_output,"lmen_cor_25112022.RDS"))
dep <- sf::st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2021/franceentiere/dep_franceentiere_2021.gpkg")

toto <- lmen %>% map(~.x[sample(1:.N,5000000)]) %>% rbindlist(idcol="annee")
toto[,dep:=str_sub(depcom,1,2)]
toto_met <- toto[!dep %in% c(97)]
toto_met[,.N,dep]
toto_met <- st_as_sf(toto_met,coords=c("x","y"),crs=2154)

problemes <- data.frame()
for (idep in unique(toto_met$dep)) {
  
  points <- toto_met %>% filter(dep==idep) %>% st_transform(proj_wgs84)
  surface <- dep %>% filter(code == idep) %>% st_buffer(4000)
  
  # nb_ok <- points %>% st_intersects(surface) %>% unlist() %>% sum()
  pos.pb <- points %>% st_intersects(surface) %>% map(~length(.x)==0) %>% unlist()
  
 
  
  if(length(pos.pb)>0){
    warning(paste0("Departement", idep," : ",length(pos.pb)," problemes"))
    problemes <- problemes %>% rbind(points[pos.pb,])
  }

}

problemes
mapview(problemes)+mapview( dep %>% filter(code == 50) %>% st_buffer(4000))

