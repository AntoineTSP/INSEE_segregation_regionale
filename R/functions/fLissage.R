#'@title fLissage
#'
#' @param car : table de carreaux (df) avec les variables suivantes : 
#'          - idInspire : identifiant inspire du carreau
#'          - segregStandar : ségrégation standardisée (= perte relative d'entropie du carreau par rapport à la distribution idéale)
#'          - nbpers_carreau : nombre d'individus dans le carreau
#'          - nbPersTrancheX : nombre d'individus appartenant au groupe X (1, 2, ..., 5)
#'          - median_carreau=10000
#'          - nbmen_carreau=30
#' @param nb_tranches : nombre de groupes
#'
#' @return : base de centroides de carreaux "centroides_smooth_4326" avec : 
#'            - idInspire : 
#'            - Nombre lissé de personnes par groupe
#'            - Nombre lissé de personnes (total)
#'            - Nombre de ménages dans le carreau
#'            - long,lat : coordonnées du centroïde dans la projection WGS84
#'            - x,y : coordonnées du centroïde dans la projection du territoire
#'            - Segragation standardisée brute
#'            - Contribution à l'indice de Theil
#'            - Segragation standardisée lissée
#'            - Contribution à l'indice de Theil lissée
#'            - Médiane de revenus brute
#'            - Nombre de ménages dans le carreau
#'            - Entropie lissée du carreau
#'            - Variable caractères de labelle des contribution/ségrégation brutes/lissées
#'            - Geometry : colonne vectorielle du point (en WGS84)
#'@details : utilise : 
#'          - BTB
#'          - sf
#'          - f_entro_partielle
#'          Le lissage est réalisé avec un rayon de 3 fois la taille des carreaux
#' @examples
#' car <- data.frame(
#' idInspire = "CRS2154RES500mN6680500E357000",
#' segregStandar = 0.03428639,
#' nbpers_carreau = 76.0,
#' nbPersTranche1 = 10,
#' nbPersTranche2 = 15,
#' nbPersTranche3 = 15,
#' nbPersTranche4 = 20,
#' nbPersTranche5 = 16,
#' median_carreau=10000,
#' nbmen_carreau=30
#' )
#' fLissage(car,nbmen_min,nb_tranches)


# car <- lcouches_car$men17.9B1.2000
# nb_tranches <- 5
 
fLissage <- function(car,nb_tranches){
  
  stopifnot("fLissage : pas de colonne idInspire dans la base menages"=!is.null(car$idInspire))
  stopifnot("fLissage : pas de colonne segregStandar dans la base menages"=!is.null(car$segregStandar))
  stopifnot("fLissage : pas de colonne nbpers_carreau dans la base menages"=!is.null(car$nbpers_carreau))
  stopifnot("fLissage : absence de la variable nbPersTranche1"=!is.null(car$nbPersTranche1))
  stopifnot("fLissage : absence de la variable median_carreau"=!is.null(car$median_carreau))
  stopifnot("fLissage : absence de la variable nbmen_carreau"=!is.null(car$nbmen_carreau))
  
  # Récupération projection
  proj <- car$idInspire[1] %>% str_sub(4,7)
  stopifnot("fLissage : probleme dans IdInspire du premier carreau"=str_length(proj)==4)
  
  # Récupération taille carreaux
  taille_carreau <- car$idInspire %>% str_extract("(?<=RES)\\d+") %>% unique() %>% as.numeric()
  stopifnot("fLissage : probleme avec taille des carreaux dans IdInspire"=length(taille_carreau)==1)
  rayon_lissage <- 3*taille_carreau
  
  
  #### Calcul de la contribution à la ségrégation (brute, non lissee) ===========
  car <- car %>%
    mutate(segregStandar_ponder=segregStandar*nbpers_carreau/sum(nbpers_carreau),
           contribSegreg=100*segregStandar_ponder/sum(segregStandar_ponder))
  
  ##### Lissage avec le package BTB : nombre de personnes par groupes =============
  
  car <- car %>% mutate(x=idInspire %>% str_extract("(?<=E)\\d+$") %>% as.numeric() + taille_carreau/2,
                        y=idInspire %>% str_extract("(?<=N)\\d+(?m)") %>% as.numeric() + taille_carreau/2)
  carreaux_smooth <- btb::kernelSmoothing(
    dfObservations = car %>%
      select(starts_with("nbpers",ignore.case = T),x,y),
    iCellSize = taille_carreau,
    sEPSG = proj,
    iBandwidth = rayon_lissage,
    iNeighbor = 0 # Pas d'effet de bord aux frontières de la couche de carreaux
  )
  
  # Changement noms
  noms <- colnames(carreaux_smooth)
  pos.modif <- !noms %in% c("x","y","geometry") 
  colnames(carreaux_smooth)[pos.modif] <- noms[pos.modif] %>% paste0("_liss")
  
  # Rapatriement de variables brutes sur la couche lissée
  carreaux_smooth <- carreaux_smooth %>%
    left_join(car %>%
                select(idInspire,x,y,segregStandar,contribSegreg,median_carreau,nbpers_carreau,nbmen_carreau), 
              by=c("x","y"))
  stopifnot("fLissage : Problème lors du lissage"=nrow(car)==nrow(carreaux_smooth))
  
  #### Calcul des entropies lissées =======================
  
  entropLisee <- carreaux_smooth  %>% st_drop_geometry() %>% select(idInspire,starts_with("nbpersTranche",ignore.case = T))
  entropLisee <- entropLisee %>% pivot_longer(!idInspire,names_to = "tranche", values_to = "nbpers_liss")
  entropLisee <- entropLisee %>%
    group_by(idInspire) %>%
    mutate(
      nbpersCarreau_liss = sum(nbpers_liss),
      p_liss = nbpers_liss / nbpersCarreau_liss,
      entroPartiel_liss = f_entro_partielle(p_liss)
    ) %>%
    summarise(entro_liss = sum(entroPartiel_liss),
              nbpersCarreau_liss = sum(nbpers_liss)) %>%
    mutate(
      segregStandar_liss = (log(nb_tranches) - entro_liss) / log(nb_tranches), # Entropie totale inchangée par le lissage (car conservatif)
      segregPonder_liss=segregStandar_liss*nbpersCarreau_liss / sum(nbpersCarreau_liss),
      contribSegreg_liss=100*segregPonder_liss/sum(segregPonder_liss)
    ) %>% 
    select(!segregPonder_liss)
  
  #### Rapatriement de la segregation et de la contribution proprement lissées
  carreaux_smooth <- carreaux_smooth %>%
    left_join(entropLisee,by="idInspire")
  
  
  #### Passage à un format centroides ======================
  
  centroides_smooth <- carreaux_smooth %>%
    st_centroid() %>% suppressWarnings()
  
  # centroides_smooth_4326 <- centroides_smooth %>% st_transform(crs=proj_wgs84)
  centroides_smooth_4326 <- centroides_smooth %>% st_transform(crs=proj_lambert93)
  
  # Coordonnées coin en bas à gauche de chaque carreau en WGS84 pour deckgl (carto)
  #Antoine : En Lambert 83 
  coords <- (centroides_smooth$geometry  + c(-taille_carreau/2,-taille_carreau/2)) %>% 
    st_sf() %>% st_set_crs(as.numeric(proj)) %>% 
    # st_transform(proj_wgs84) %>% 
    st_transform(proj_lambert93) %>%
    st_coordinates() %>%
    as.data.frame() %>% 
    rename(lat_hg=Y,long_hg=X)
  
  centroides_smooth_4326 <- centroides_smooth_4326 %>% bind_cols(coords)
  
  
  #### Modifications des formats des variables ====================
  centroides_smooth_4326 <-
    centroides_smooth_4326 %>%
    mutate(median_carreau_lab = median_carreau %>% round(digits=1) %>%  format(digits=1,scientific = F),
           segregStandar_lab = segregStandar %>%round(digits=2) %>%  format(digits=2),
           segregStandar_liss_lab = segregStandar_liss %>%round(digits=2)%>% format(digits=2),
           contribSegreg_lab = (contribSegreg*100) %>% round(digits=2)%>% format(digits=2)%>% paste0(" pour 10 000"),
           contribSegreg_liss_lab = (contribSegreg_liss*100) %>% round(digits=2)%>% format(digits=2) %>% paste0(" pour 10 000")
    )
  
  
  #### Ajout d'une variable de couleur (revenu médian du carreau discrétisé) ====================
  
  centroides_smooth_4326 <- ajoutCol(centroides_smooth_4326,nb_tranches)
  
  
  return(centroides_smooth_4326)
}
