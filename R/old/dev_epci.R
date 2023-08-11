

.x <- lmen_epci$men10
menages <- .x
y = liste_taille_carreaux$`200`
taille_carreau <- y


# taille_carreau <- 200
# menages <- lmen_epci$men10
nb_tranches <- 5
passageComZE = com_53epci;
ZE_ssQP=c("200066793"); 
sfqp_pole_inters=sfqp_epci_inters[sfqp_epci_inters$aav20 == epci_cible,]
varCom="CODGEO"
varCodeZe="EPCI"




# Debug

sfqp_epci_inters <- sf_epci %>% filter(EPCI=="200046977") %>% 
  st_intersection(qp_contours %>% select(code,geometry))%>% 
  rename(aav20=EPCI)


qp_contours_ze <- sfqp_epci_inters
qp_contours_ze %>% st_union %>% st_as_sf() %>% st_transform(crs=2154)
