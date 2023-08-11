
liste <- list("a"=1,"b"=2,"c"=3)
#' first_and_last
#' @description : extrait le premier et le dernier element d'une liste
#' @param liste 
#'
#' @return : liste contenant le prmeier et le dernier élement de la liste en input
#' @export
#'
#' @examples
#' liste <- list("a"=1,"b"=2,"c"=3)
#' first_and_last(liste)
#' 
first_and_last <- function(liste){
  long <- length(liste)
  liste[c(1,long)]
}




#' distriQP
#' @description : Proportion des groupes dans les QP (en première et dernière année possible)
#' @param maCouche : version filtrée (carreaux de 200m) et transposée de lcouches 
#'
#' @return liste pour chaque AAV contenant un tibble
#' @export
#'
#' @examples


distriQP <- function(maCouche){
  
  l0 <- maCouche %>% map(first_and_last)
  l0 <- l0 %>% modify_depth(2,~.x$carreaux %>% filter(idInspire %in% .x$id_carreaux_QP))
  l0 <- l0 %>% modify_depth(2,
                            ~.x %>% 
                         summarise(across(starts_with("nbPersTranche"),
                                          ~ 100 * sum(.x) / sum(nbpers_carreau))),.id="annee") 
  l0 <- l0 %>% map(rbindlist,idcol="annee")
  
  l0 <- l0 %>% map(~ .x %>% pivot_longer(!annee, names_to = "groupe", values_to ="part")) 
  
  l0 <- l0 %>% map(~ .x %>% mutate(groupe=str_replace_all(groupe,pattern = "nbPersTranche",replacement = "g")))
  
  return(l0)

}

#Essai Antoine
#' distri_hors_QP
#' @description : Proportion des groupes dans les hors_QP (en première et dernière année possible)
#' @param maCouche : version filtrée (carreaux de 200m) et transposée de lcouches 
#'
#' @return liste pour chaque AAV contenant un tibble
#' @export
#'
#' @examples


distri_hors_QP <- function(maCouche){
  
  l0 <- maCouche %>% map(first_and_last)
  l0 <- l0 %>% modify_depth(2,~.x$carreaux %>% filter( ! idInspire %in% .x$id_carreaux_QP))
  l0 <- l0 %>% modify_depth(2,
                            ~.x %>% 
                              summarise(across(starts_with("nbPersTranche"),
                                               ~ 100 * sum(.x) / sum(nbpers_carreau))),.id="annee") 
  l0 <- l0 %>% map(rbindlist,idcol="annee")
  
  l0 <- l0 %>% map(~ .x %>% pivot_longer(!annee, names_to = "groupe", values_to ="part")) 
  
  l0 <- l0 %>% map(~ .x %>% mutate(groupe=str_replace_all(groupe,pattern = "nbPersTranche",replacement = "g")))
  
  return(l0)
  
}
