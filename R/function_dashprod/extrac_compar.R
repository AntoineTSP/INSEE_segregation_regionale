#' extrac_compar
#'
#' @param lcouches : liste hiérarchique des données (totale)
#' @param passage_nom : table de passage entre les noms des aav et leur code
#'
#' @return table des indices de segregation pour le graph de comparaisons des aav
#' @export
#'
#' @examples

extrac_compar <- function(lcouches,passage_nom,taille){
  
  stopifnot("extrac_compar : pb dans lcouches"="2012" %in% names(lcouches))
  stopifnot("extrac_compar : pb dans passage_nom"= (is.data.frame(passage_nom)) & ("reg" %in% names(passage_nom)))
  
  donnees_graph <- lcouches %>% 
    modify_depth(2,as.character(taille)) %>% 
    modify_depth(2,"segreg") %>% 
    modify_depth(3,"segTot") %>% 
    unlist() %>% 
    as_tibble(rownames = "info")
  info_matrix <- str_split(donnees_graph$info,"\\.",simplify = T)
  
  donnees_graph <- donnees_graph %>% 
    mutate(annee=info_matrix[,1],
           reg=info_matrix[,2],
           typ_indice=info_matrix[,3]) %>% 
    rename(segTot=value) %>% 
    select(!info)
  
  donnees_graph <- donnees_graph %>% left_join(passage_nom,by="reg")
  
  return(donnees_graph)
}