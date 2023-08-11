#' def_quantiles
#'
#' @param maCouche 
#'
#' @return liste. tableau des quantiles de revenus pour chaque ville.
#' @export
#'
#' @examples

def_quantiles <- function(maCouche){
  
  nb_groupes <- length(maCouche[[1]][[1]]$quant$bornes)-1
  bornes <- c("min",paste0("q",1:(nb_groupes-1)),"max")#
  
  liste_res <- list()
  for (aav in names(maCouche)) { #"002"
    maCouche_restric <- maCouche[[aav]]
    
    dta <-  maCouche_restric %>% map("quant") %>% 
      map_dfr("bornes") %>% t()
    dta <- dta %>% as_tibble(rownames = "annee") 
    colnames(dta)[-1] <- bornes
    dta <- dta %>% mutate_if(is.numeric,round)
    
    # Supression de la colonne "max"
    dta <- dta %>% select(-max)
    liste_res[[aav]] <- dta
    
  }
  return(liste_res)
}
