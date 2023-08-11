# ****************************************************
#                     definQP
# ****************************************************

#' Appelée par la fonction f_carroyage
#' Objectif reconstituer les QP carroyés à partir des ménages
#' Besoin d'une base de ménages avec coord des ménages et des centroides des carreaux auxquels ils appartiennent
#' Un carreau est constitutif des QP si la majorité de sa population est géolocalisée à l'intérieur des contours des QP
#' Résultats détaillés avec distinction des carreaux à cheval, à l'intérieur, etc.


#' menages = menages,proj = proj,codeZE = codeZE,
#' taille_carreau = taille_carreau,sfqp_pole_inters

definQP <- function(menages,taille_carreau,proj,codeZE,sfqp_pole_inters){
  
  # Liste des QP dans la zone d'étude et union des géométries
  qp_contours_ze <- sfqp_pole_inters %>% filter(reg==codeZE) 
  qp_contours_ze_union <-  qp_contours_ze %>% st_union %>% st_as_sf() %>% st_transform(crs=proj)
  
  # Création d'un buffer externe
  qp_buffer <- qp_contours_ze_union %>% 
    st_buffer(((2*(taille_carreau^2))^(1/2))/2) %>%
    st_as_sf() %>% st_transform(crs=proj)
  
  # Liste des centroïdes de carreaux, passage en points
  dfcoords <- tibble(idInspire=unique(menages$idInspire)) %>% 
    mutate(x_centro=idInspire %>% str_extract("(?<=E)\\d+$") %>% as.numeric() + 100,
           y_centro=idInspire %>% str_extract("(?<=N)\\d+(?m)") %>% as.numeric() + 100)
  if(sum(is.na(dfcoords$x_centro)|is.na(dfcoords$y_centro))>0) print(dfcoords %>% filter(is.na(x_centro)|is.na(y_centro)))
  
  sf_centroides <- st_as_sf(dfcoords, coords = c("x_centro","y_centro"),crs=proj)
  
  # Carreaux intersectant les QP
  sf_centroides_inbuff <- sf_centroides %>% st_join(qp_buffer,left = F) 
  
  # Carreaux entièrement à l'intérieur des QP
  sf_carr_inbuff <- sf_centroides_inbuff  %>% 
    mutate(x=st_coordinates(geometry)[,1],
           y=st_coordinates(geometry)[,2]) %>% 
    st_drop_geometry() %>% 
    btb::dfToGrid(sEPSG = proj,iCellSize = taille_carreau)
  
  sf_carr_insideQP <- sf_carr_inbuff %>% 
    st_join(qp_contours_ze_union,join = st_within,left=F)
  
  # Choix des carreaux à cheval à retenir
  idInpire_carr_acheval <- sf_centroides_inbuff$idInspire %>% setdiff(sf_carr_insideQP$idInspire)
  
  men_carr_acheval <- menages[idInspire %in% idInpire_carr_acheval]
  sf_men_carr_acheval <- men_carr_acheval %>% st_as_sf(coords=c("x","y"),crs=proj)
  sf_men_carr_acheval <- sf_men_carr_acheval %>% 
    st_join(qp_contours_ze_union %>% mutate(dansQP=T),left=T) 
  
  idInpire_carr_acheval_okQP <- sf_men_carr_acheval %>% 
    st_drop_geometry() %>% group_by(idInspire) %>% 
    summarise(partmen_inQP=sum(dansQP,na.rm = T)/n()) %>% 
    filter(partmen_inQP>=0.5) %>% 
    pull(idInspire)
  
  # Resultat => liste des idInspire des carreaux
  res <- c(idInpire_carr_acheval_okQP,sf_carr_insideQP$idInspire)
  stopifnot(sum(duplicated(res))==0) 
  
  return(res)
}

