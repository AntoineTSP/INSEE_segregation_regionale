# ***************************************
#               ajoutCol
# ***************************************

# Fonction de coloriage des carreaux selon la médiane de revenus (utilisée infra)
# On ajoute seulement une variable contenant les couleurs en code HTML suite à une discrétisation en quantiles de la variable de revenu médian par carreau

#' ajoutCol 
#' @description :
#'      - discrétise la variable revenu médian par carreau en quantiles
#'      - ajoute une variable contenant les couleurs en code HTML 
#' @param : 
#'          - tab : table contenant la variable median_carreau
#'          - nb_tranches : nombre de groupes de revenus
#' @details : utilise le package classInt
#' @return tab avec une variable "couleur"
#'
#' @examples

ajoutCol <- function(tab,nb_tranches){
  stopifnot("ajoutCol : absence de la variable median_carreau"= !is.null(tab$median_carreau))
  
  classif <- classIntervals(tab$median_carreau,n = nb_tranches,style = "quantile") #jenks"quantile"
  pal <- colorBin("RdYlBu", bins = classif$brks)
  tab$couleur <- pal(tab$median_carreau)
  return(tab)
}
