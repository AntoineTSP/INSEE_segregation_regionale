#' tabContrib_groupes
#'
#' @param maCouche 
#'
#' @return liste ville<type_segreg<groupe contenant les tableaux de décomposition
#' @export
#'
#' @examples
#' 
tabContrib_groupes <- function(maCouche){ 
  
  noms_groupes <- names(maCouche[[1]][[1]]$segreg[[1]]$decompo_groupes)
  noms_aav <- names(maCouche)
  
  liste_res <- list()
  
  for (aav in noms_aav) { #aav="001"
    
    l1 <- maCouche[[aav]] %>% map("segreg")
    
    for (type in c("theil","iim")) { # type='iim'
      l2 <- l1 %>% map(type)
      
      for (g in noms_groupes) { #g <- "g3"
        res_segtot <- l2 %>% map_dfr("segTot",.id="annee") %>% pivot_longer(everything(),names_to = "annee",values_to = "segTot")
        res_g <- l2 %>% map_dfr(~.x[["decompo_groupes"]][[g]],.id="annee") 
        res <- merge(res_segtot,res_g,by="annee")
        
        liste_res[[aav]][[type]][[g]] <- res
      }
    }
  }
  
  liste_res <- liste_res %>% 
    modify_depth(3,~.x %>% mutate(contrib_binaire=teta_binaire*indice_binaire,
                       contrib_multi=teta_multi*indice_multi))
  
  return(liste_res)
  
}


