#' effectifs
#'
#' @param maCouche 
#'
#' @return une liste par ville contenant une table d'effectifs par année.
#' @export
#'
#' @examples
effectifs <- function(maCouche){
 
  maCouche %>% 
    modify_depth(2,"carreaux") %>% 
    modify_depth(2,~tibble(nb_indiv=sum(.x$nbpers_carreau),nb_men=sum(.x$nbmen_carreau))) %>% 
    map(~bind_rows(.x,.id="annee"))
}

