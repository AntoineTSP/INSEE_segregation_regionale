
#' returnTabContrib
#' 
#' 
#' @description : Retourne le tableau détailé de la décomposition QP-hordQP
#' @param maCouche : liste de villes (version expurgée et transposée et lcouches)
#' @param exclusion  : codes des villes à retirer faute de QP (Annecy)
#'
#' @return data.frame 
#' @export
#'
#' @examples

returnTabContrib <- function(maCouche,exclusion=c("043")){ 
  
  maCouche_expurg <- maCouche[!names(maCouche) %in% exclusion]
  
  maCouche_expurg %>%
    modify_depth(2, "segreg") %>%
    modify_depth(3, `[`, c("segTot", "decompo_qp")) %>%
    map(purrr::transpose) %>%
    modify_depth(2,  ~ .x %>% unlist(use.names = T) %>% as.data.frame()) %>%
    modify_depth(
      2,
      ~ .x %>%
        rownames_to_column("id") %>%
        mutate(
          id = id %>% str_remove("decompo_qp."),
          annee = str_split(id, pattern = "\\.") %>% map_chr(1),
          indic = str_split(id, pattern = "\\.") %>% map_chr(2)
        ) %>%
        select(!id) %>%
        rename_with(.cols = 1, ~ "values") %>%
        pivot_wider(names_from = "indic",
                    values_from = "values")  %>%
        mutate(
          contrib_QP = teta_QP * indice_QP,
          contrib_horsQP = teta_horsQP * indice_horsQP
        ) %>%
        mutate_if(is.numeric, round, 4) %>%
        as.data.frame
    )
}





