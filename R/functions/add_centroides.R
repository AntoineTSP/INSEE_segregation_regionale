#' add_centroides
#' @description : ajoute les coordonnées des centroïdes à chaque point
#' @param dt : un data.table avec les variables des coordonnées géometriques
#' @param tcar : taille des carreaux (valeur numerique)
#'
#' @return : tab avec 2 variables "x_centroide" et "y_centroide" supplémentaire
#'
#' @examples
#' dt <- data.table::data.table(x=599,y=226,val=1000)
#' add_centroides(dt,200)
add_centroides <- function(dt,tcar,lvar=c("x","y")) {
  
  dta <- copy(dt)
  if(!dta %>% is.data.table()) stop("add_inspire : dt n'est pas un data.table")
  if(nrow(dta)==0) stop("add_inspire : dt est vide")
  if (length(lvar)!=2) stop("add_inspire : lvar ne contient pas exactement 2 variables")
  if (is.null(dta[,lvar[1],with=F]) | is.null(dta[,lvar[2],with=F])) stop("add_inspire : pas de variables x_centroide ou y_centroide")
  
  dta[,':='(x_centroide=x - (x %% tcar) + (tcar / 2),
            y_centroide=y - (y %% tcar) + (tcar / 2))]
  return(dta)
}