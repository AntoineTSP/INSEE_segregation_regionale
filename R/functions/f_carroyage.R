# ********************************************************
#                     f_carroyage                     ####
# ********************************************************


#' Objectifs :
#' - Création des couches carroyées à partir des bases ménages
#' - Calculs des indices de ségrégation
#' - Calculs des décompositions spatiales QP/ hors-QP
#' - Calculs des décompositions par groupes sociaux
#' - Discrétisation des carreaux selon leur niveau de revenus "moyens" (couleur sur les cartes)
#' Paramètres :
#' - une base ménages
#' - le nombre de groupes de revenus à former
#' - la taille des carreaux
#' - Option avec ou sans filtrage des carreaux peu peuplés
#'

#' @title f_carroyage
#' @description : 
#'
#' @param menages : base ménages (dt) avec les variables : 
#' - depcom
#' - revdecucm
#' - idInspire
#' @param nb_tranches : nombre de classes de revenus
#' @param taille_carreau : taille des carreaux
#' @param ZE_ssQP : liste des zones d'études sans QP (ex : pôle d'Annecy)
#' @param sfqp_pole_inters : table sf des intersections entre les ZE et les QP
#' @param passageComZE : Table de passage communes - zonage d'intérêt (pôles des AAV, EPCI, etc.)
#'
#' @return : 
#' - Si base ménages associée à des carreaux de 200, dans une ZE contenant des QP :
#'    - Base carroyée
#'    - Indice de ségrégation avec décomposition par groupes sociaux et décomposition QP/hors-QP
#'    - Définition des quantiles de revenus
#'    - Liste des IdInspire des carreaux définissant l'ensmeble des QP
#' - Autrement
#'    - Base carroyée
#'    - Indice de ségrégation avec décomposition par groupes sociaux
#'    - Définition des quantiles de revenus
#'
#' @import 
#'     - guess_codeZE
#'     - guess_proj
#'     - calcul_indSegreg
#'     - f_entro_partielle
#' 
#' @examples
#' taille_carreau <- 200
#' menages <- lmen_poles$men06.004
#' nb_tranches <- 5
#' passageComZE = com_poles;ZE_ssQP=pole_ssQP; sfqp_pole_inters=sfqp_pole_inters
#' varCom="CODGEO",varCodeZe="AAV2020"


f_carroyage <- function(taille_carreau,menages,nb_tranches,passageComZE = com_poles,ZE_ssQP=pole_ssQP, sfqp_pole_inters=sfqp_pole_inters,varCom="CODGEO",varCodeZe="AAV2020"){
  
  
  
  if(any(nrow(menages)==0, is.na(menages))){
    message("base menage vide")
    return(NA)
  }else{
    
    codeZE <- guess_codeZE(lcom = unique(menages$depcom),
                           passageComZE = passageComZE,
                           varCom=varCom, 
                           varCodeZe=varCodeZe)
    
    proj <- guess_proj(unique(menages$depcom))
    # taille_carreau <- menages$idInspire[1] %>% str_extract("RES\\d+") %>% str_remove("RES") %>% as.integer()
    
    message(paste0(codeZE,"-",taille_carreau))
    
    
    # Filtrage de la base
    nomvarfiltre <- paste0("filtre",taille_carreau)
    nomvaridInspire <- paste0("idInspire",taille_carreau)
    nomcolonnes_agarder <- colnames(menages)[!colnames(menages) %>% str_detect("idInspire")]
    nomcolonnes_agarder <- nomcolonnes_agarder[!nomcolonnes_agarder %>% str_detect("filtre")]
    nomcolonnes_agarder <- c(nomcolonnes_agarder,nomvaridInspire,nomvarfiltre)
    # Filtre des ménages et des variables pour une taille de carreaux donnée
    menages <- menages[get(nomvarfiltre)==T,.SD,.SD=nomcolonnes_agarder]
    setnames(menages,old=nomvaridInspire,new = "idInspire")
    menages[,(nomvarfiltre):=NULL]
    
    
    
    
    # Calcul quantiles de revenus des individus par UC sur l'agglomération (/!\ des individus)
    quantRev_indiv <- menages$revdecucm %>%
      Hmisc::wtd.quantile(probs=seq(0,1,1/nb_tranches),
                          w=menages$nbpersm) %>% as.numeric()
    menages$tr_rev <- cut(menages$revdecucm, quantRev_indiv,include.lowest = T,labels = F)
    
    # Mediane des revenus par groupes
    
    medianes_tr <-menages[,.(med_tr=Hmisc::wtd.quantile(revdecucm,probs=0.5,w=nbpersm)),
                          by=tr_rev][order(med_tr)] 
    lmedianes_tr <- unlist(medianes_tr$med_tr) %>% set_names(medianes_tr$tr_rev)
    
    #### Calculs de l'entropie et des indices de ségregation ####
    res_ville <- calcul_indSegreg(menages,liste_carrQP = NULL)
    carreaux <- res_ville$carreaux
    carreaux_tranches <- res_ville$carreaux_tranches
    
    #### Décomposition groupes sociaux (voir documentation méthodo)
    
    # Pour les constantes
    teta_binaire_iim <- 1
    teta_multi_iim <- 1-1/nb_tranches
    teta_binaire_theil <- (f_entro_partielle(1/nb_tranches)+f_entro_partielle(1-1/nb_tranches))/log(nb_tranches)
    teta_multi_theil <- (1-1/nb_tranches)*log(nb_tranches-1)/log(nb_tranches)
    
    decompo_groupes_theil <- list()
    decompo_groupes_iim <- list()
    
    for (i in 1:nb_tranches) { #i=2L
      res_multi <- menages %>% filter(tr_rev!=i) %>% calcul_indSegreg() # calcul de l'indice de ségrégation sur les groupes sociaux hors groupe i
      res_binaire <- menages %>% mutate(tr_rev=if_else(tr_rev==i,i,999L)) %>% calcul_indSegreg() # calcul de l'indice de ségrégation binaire (groupe i versus le reste)
      
      # IIM
      decompo_groupes_iim[[paste0("g",i)]] <- list(
        teta_binaire=teta_binaire_iim,
        indice_binaire=res_binaire$iim,
        teta_multi=teta_multi_iim,
        indice_multi=res_multi$iim)
      
      # Theil
      decompo_groupes_theil[[paste0("g",i)]] <- list(
        teta_binaire=teta_binaire_theil,
        indice_binaire=res_binaire$theil,
        teta_multi=teta_multi_theil,
        indice_multi=res_multi$theil
      )
    }
    
    
   #### Décomposition QP : Modification Antoine
  
   if(!codeZE %in% ZE_ssQP) {#ssi petits carreaux et pôle contenant des QP (pas le cas à Annecy)
      
      # Liste des carreaux définissant les QP (et ceux définissant les zones hors-QP)
      id_carreaux_QP <- definQP(menages = menages,proj = proj,codeZE = codeZE,
                                taille_carreau = taille_carreau,sfqp_pole_inters)
      id_carreaux_horsQP <- menages[!idInspire %in% id_carreaux_QP,idInspire] %>% unique()
      
      # Indices sur sous-territoires et tetas + Indices inter
      res_QP <- calcul_indSegreg(menages,liste_carrQP = id_carreaux_QP)
      res_horsQP <- calcul_indSegreg(menages,liste_carrQP = id_carreaux_horsQP)
      
      # Calcul des composantes Inter
      theil_inter <- res_QP$teta_iim*(1-res_QP$entro/log(nb_tranches))+res_horsQP$teta_iim*(1-res_horsQP$entro/log(nb_tranches))
      iim_inter <- log(nb_tranches)-(res_QP$teta_iim * res_QP$entro) - (res_horsQP$teta_iim * res_horsQP$entro)
      
      # Listes des résultats
      decompo_qp_theil=list(
        teta_QP=res_QP$teta_theil,
        indice_QP=res_QP$theil,
        teta_horsQP=res_horsQP$teta_theil,
        indice_horsQP=res_horsQP$theil,
        indice_inter=theil_inter)
      
      decompo_qp_iim=list(
        teta_QP=res_QP$teta_iim,
        indice_QP=res_QP$iim,
        teta_horsQP=res_horsQP$teta_iim,
        indice_horsQP=res_horsQP$iim,
        indice_inter=iim_inter)
   }
    
    
    ##### Méthodes de détermination de la classe de revenu de chaque carreau #####
    
    # Discrétisation à partir de la distribution des médianes des carreaux
    carreau_medianes <- copy(menages)
    carreau_medianes <- carreau_medianes[,.(median_carreau =Hmisc::wtd.quantile(revdecucm,nbpersm,0.5),
                                            nbmen_carreau = .N)
                                         ,by="idInspire"]
    
    # Ajout de la médiane pondérée à la base des carreaux
    carreaux <- merge(carreaux,carreau_medianes,by="idInspire")
    
    # # Rapatriement des coordonnées des centroides des carreaux
    # carreaux <- carreaux %>%
    #   left_join(passage_id_centroides)
    
    
    # Rapatriement du nombre de personnes par tranche pour chaque carreau pour calculer la ségrégation lissée
    
    carreaux_tranches <- carreaux_tranches[,.(idInspire, tr_rev, nbpers_carreau_tr)][
      ,tr_rev := paste0("nbPersTranche", tr_rev)
    ]
    if(carreaux_tranches[,.N]>0){
    carreaux_tranches <- dcast(carreaux_tranches,idInspire~tr_rev,value.var = "nbpers_carreau_tr",fill=0L)
    }
    carreaux <- carreaux %>% merge(carreaux_tranches,by="idInspire",all.x = T)
    
    
    # Résultats différenciés selon taille des carreaux : Modification Antoine
    if(!codeZE %in% ZE_ssQP){
      res <- list(carreaux=carreaux,
                  segreg=list(
                    theil=list(
                      segTot=res_ville$theil,
                      decompo_groupes=decompo_groupes_theil,
                      decompo_qp=decompo_qp_theil
                    ),
                    iim=list(
                      segTot=res_ville$iim,
                      decompo_groupes=decompo_groupes_iim,
                      decompo_qp=decompo_qp_iim
                    )
                  ),
                  quant=list(bornes=quantRev_indiv,
                             medianes_rev=lmedianes_tr),
                  id_carreaux_QP=id_carreaux_QP)

    }else{
      res <- list(carreaux=carreaux,
                  segreg=list(
                    theil=list(
                      segTot=res_ville$theil,
                      decompo_groupes=decompo_groupes_theil
                    ),
                    iim=list(
                      segTot=res_ville$iim,
                      decompo_groupes=decompo_groupes_iim
                    )
                  ),
                  quant=list(bornes=quantRev_indiv,
                              medianes_rev=lmedianes_tr)
                  )
        
      }
    
    #gc() #libération mémmoire
    return(res)
  }
}


