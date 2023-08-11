#' tab_deciles
#'
#' @param maCouche 
#' @param type : caractère. "men" pour calculer sur la population de ménages. Autrement, calcul sur la population d'individus 
#'
#' @return Liste de df, 2 df par ville.
#' @export
#'
#' @examples


tab_deciles <- function(maCouche,type="men"){
  
  
  variab <- ifelse(type=="men","nbmen_carreau","nbpers_carreau")
   
  
  liste_res_def <- list()
  for (aav in names(maCouche)) { # aav="010"
    
    maCoucheVille <- maCouche[[aav]]
    anneemax <- names(maCoucheVille) %>% as.numeric() %>% max %>% as.character()
    anneemin <- names(maCoucheVille) %>% as.numeric() %>% min %>% as.character()
    
    #### Calculs des déciles et création d'une table  =========================
    
    classifmax <- maCoucheVille[[anneemax]]$carreaux[[variab]] %>% classIntervals(n = 10)
    classifmin <- maCoucheVille[[anneemin]]$carreaux[[variab]] %>% classIntervals(n = 10)
    
    bornes_max <- classifmax$brks %>% round(1)
    bornes_min <- classifmin$brks %>% round(1)
    
    maTable <- rbind(c(anneemin,bornes_min),c(anneemax,bornes_max)) %>% as.data.frame()
    colnames(maTable) <- c("annee","min",paste0("decile",1:9),"max")
    
    #### Calcul des effectifs par déciles ======================================
    maTable_pop <- maCoucheVille[[anneemax]]$carreaux %>% 
      cbind(deciles=findCols(classifmax)) %>% 
      group_by(deciles) %>% 
      summarise(nombre=sum(.data[[variab]])) %>% 
      mutate(proportion=(100*nombre/sum(nombre)) %>% round(1))
    
    liste_res_def[[aav]] <- list(res_def=maTable,res_effect=maTable_pop)
  }
  return(liste_res_def)
  
}