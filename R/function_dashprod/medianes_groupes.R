#' medianes_groupes
#'
#' @param maCouche 
#'
#' @return tableaux des médianes de revenus par UC des différents groupes, pour chaque année (une table par ville)
#' @export
#'
#' @examples
#' 
medianes_groupes <- function(maCouche){
  
  nb_groupes <- length(maCouche[[1]][[1]]$quant$bornes)-1
  
  liste_res <- list()
  
  for(aav in names(maCouche)){
    maCouche_restric <- maCouche[[aav]]
    
    dta <- maCouche_restric %>% 
      map('quant') %>% 
      map_dfr('medianes_rev',.id="annee") 
    colnames(dta)[-1] <- paste0("groupe_",1:nb_groupes)
    
    dta <- dta %>% mutate_if(is.numeric,round)
    
    liste_res[[aav]] <- dta
  }
  
  return(liste_res)
}
