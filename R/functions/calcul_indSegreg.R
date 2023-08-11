# *****************************************
#            calcul_indSegreg
# *****************************************

#' @title calcul_indSegreg
#' @description : 
#' #' Fonction retournant des résultats différents selon :
#' - qu'on souhaite des résultats sur le pôle dans son ensemble
#' - qu'on souhaite calculer les indices de ségrégation intra dans le cadre de la décomposition QP/hors-QP
#' L'idée est de créer une table intermédiaire de croisement (groupes sociaux * carreau) pour réaliser les calculs

#' @param menages : base ménages (dt) avec des variables idInspire, nbpersm et tr_rev
#' @param liste_carrQP : liste des idcarreaux constituant les QP (pour décomposition)
#'
#' @return : 
#' - Si liste_carrQP=NULL : 
#'      - Base de carreaux
#'      - Base de carreaux*tranches
#'      - Indice de Theil
#'      - Indice IIM
#' - liste_carrQP non nul
#'      - Indices de Theil et IIM
#'      - Constantes des décompositions spatiales de Theil et IIM
#'      - Entropie totale de la population des carreaux QP
#'
#' @examples
#' menages <- data.table(nbpersm=1:3,tr_rev=c(1,1,2),idInspire=c("a","b","b"))
#' calcul_indSegreg(menages)
#' @import f_entro_partielle





calcul_indSegreg <- function(menages,liste_carrQP=NULL){
  
  stopifnot("calcul_indSegreg : la table des menages n'est pas un dt"= is.data.table(menages) )
  stopifnot("calcul_indSegreg : pas de colonne tr_rev dans la base menages"=!is.null(menages$tr_rev))
  stopifnot("calcul_indSegreg : pas de colonne nbpersm dans la base menages"=!is.null(menages$nbpersm))
  stopifnot("calcul_indSegreg : pas de colonne idInspire dans la base menages"=!is.null(menages$idInspire))
  
  # Nombre de groupes sociaux (besoin pour calcul de décomposition)
  nb_tranches_loc <- unique(menages$tr_rev) %>% length()
  
  # Restriction ou non de la base ménages aux QP (si décomposition)
  if(liste_carrQP %>% is.null()){
    menages_restr <- menages
  }else{
    menages_restr <-menages[idInspire %in% liste_carrQP]
  }
  
  # Calcul du nb personnes (sur champs restreint si on est dans ce cas)
  nb_pers_tot <- menages_restr[,sum(nbpersm)]
  
  #### Calculs pour obtenir les composantes des indices de ségrégation
  
  carreaux_tranches <- menages_restr[,.(nbpers_carreau_tr=sum(nbpersm)),by=.(idInspire,tr_rev)]
  carreaux_tranches[,nbpers_carreau:=sum(nbpers_carreau_tr),by=idInspire]
  carreaux_tranches[,":="(#pik=nb_tranches_loc*nbpers_carreau/sum(nbpers_carreau),  # Poids démographique du carreau
                          p=nbpers_carreau_tr/nbpers_carreau # Part de chaque groupe dans chaque carreau
                          )]
  carreaux_tranches[,entroPartiel:=f_entro_partielle(p)]# calcul des p*log(1/p) pour chaque ligne
  
  # Entropie totale sur le champ considéré (ville entière ou partie)
  
  tranches <- carreaux_tranches[,.(nb_pers_tr=sum(nbpers_carreau_tr)),by=tr_rev]  #5 lignes
  tranches[,p:=nb_pers_tr/nb_pers_tot] # part de chaque groupe dans la pop total
  entro_tot <- f_entro_partielle(tranches$p) %>% sum()
  
  # Base de carreaux avec entropie, ségrégation normalisée et poids démographique du carreau
  
  carreaux <- carreaux_tranches[,.(entro=sum(entroPartiel), # entropie des carreau
                                   nbpers_carreau=sum(nbpers_carreau_tr)),
                                by=idInspire]
  carreaux[,":="(pi=nbpers_carreau/sum(nbpers_carreau), # Poids démo du carreau
                 segregStandar=(entro_tot-entro)/entro_tot)] # Ségrégation de chaque carreau
  
  # Calcul de l'indice de Theil
  theil <- carreaux[,sum(pi*segregStandar)]
  
  # Calcul de l'indice d'information mutuelle
  iim <- entro_tot-carreaux[,sum(pi*entro)] 
  
  
  #### Composantes à calculer si décomposition QP (voir Annexes méthodo de la documentation)
  if(!liste_carrQP %>% is.null()){
    # Teta Theil :
    teta_theil <- (nb_pers_tot/sum(menages$nbpersm))*(entro_tot/log(nb_tranches_loc))
    
    # Teta iim :
    teta_iim <- (nb_pers_tot/sum(menages$nbpersm))
    
  }
  
  #### Détermination des résultats selon les cas :
  
  if(liste_carrQP %>% is.null()){ # Si pas de décomposition
    res <- list(carreaux=carreaux,
                carreaux_tranches=carreaux_tranches,
                theil=theil,
                iim=iim)
  }else{ # Si décomposition
    res <- list(theil=theil,
                iim=iim,
                teta_theil=teta_theil,
                teta_iim=teta_iim,
                entro=entro_tot) # Pour calcul composante inter
  }
  
  return(res)
}
