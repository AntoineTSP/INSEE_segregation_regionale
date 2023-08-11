#' add_inspire
#' @description : ajoute une variable idInspire à la table à partir des coordonnées des centroïdes
#' @param dt : un data.table avec les variables x_centroide et y_centroide
#' @param tcar : taille des carreaux (valeur numerique)
#' @param crs : projection géographique (valeur unique)
#'
#' @return : tab avec une variable IdInspire supplémentaire
#'
#' @examples
#' dt <- data.table::data.table(x_centroide=500,y_centroide=300,val=1000)
#' add_inspire(dt,200,"2154")


add_inspire <- function(dt,tcar,crs) {
  
  dta <- copy(dt)
  if(!dta %>% is.data.table()) stop("add_inspire : dt n'est pas un data.table")
  else if (nrow(dt)==0) stop("add_inspire : dt est vide")
  else if (is.null(dta$x_centroide) | is.null(dta$y_centroide)) stop("add_inspire : pas de variables x_centroide ou y_centroide")
  
  dta[, idInspire := paste0("CRS",crs,
                            "RES",tcar,"m",
                            "N", format(y_centroide - tcar / 2,scientific = F,trim=T),
                            "E", format(x_centroide - tcar / 2,scientific = F,trim=T))]
  return(dta)
}

