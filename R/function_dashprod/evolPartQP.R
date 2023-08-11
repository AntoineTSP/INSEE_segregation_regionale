
#' evolPartQP
#' 
#' @description Part des habitants vivant dans les carreaux désignés "en QP"
#' @param maCouche : liste de villes
#' @param exclusion : code des villes à exclure (sans QP) 
#'
#' @return liste de tibbles (une lisgne par année)
#' @export
#'
#' @examples

evolPartQP <- function(maCouche,exclusion=c("043")){
  
  maCouche_expurg <- maCouche[!names(maCouche) %in% exclusion]
  
  maCouche_expurg %>% 
    purrr::modify_depth(2,~.x$segreg$iim$decompo_qp$teta_QP) %>% 
    purrr::map(~.x %>% as_tibble() %>% pivot_longer(everything(),names_to = "annee",values_to = "teta_QP"))
  
}


  
 
  