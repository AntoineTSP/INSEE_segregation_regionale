#' nbCarreaux
#'
#' @param maCouche 
#'
#' @return liste avec une table pour chaque ville. La table contient le nombre de carreau par année
#' @export
#'
#' @examples

nbCarreaux <- function(maCouche){
  maCouche %>% map(~.x %>%
                     map_dfr(~.x$carreaux %>% nrow) %>% 
                     pivot_longer(everything(),names_to = "Annee",values_to = "Nombre de carreaux"))

}
