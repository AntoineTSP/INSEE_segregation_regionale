# ********************************
#         f_entro_partielle
# ********************************


#' @title  f_entro_partielle
#' @description :
#' Fonction vectorielle de calcul de l'entropie partielle
#' Avec gestion du cas pour p=0
#' @param prop : vecteur de proportions (numeric) 
#' @return vecteur des valeurs vec_prop*log(1/vec_prop)
#' @examples
#' vec_prop <- c(0.3,0.7)
#' f_entro_partielle(vec_prop)

f_entro_partielle <- function(vec_prop){
  stopifnot("f_entro_partielle : proportion non-numerique"=is.numeric(vec_prop))
  stopifnot("f_entro_partielle : proportion negative"=sum(vec_prop<0)==0)
  if_else(vec_prop>0,vec_prop*log(1/vec_prop),0)
}

