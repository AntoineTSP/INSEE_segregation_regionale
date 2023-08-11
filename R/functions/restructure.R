


selection <- function(liste,lnoms){
  res <- list()
  for(nom in lnoms){
    # nom <- "200"
    ltemp <- liste[str_detect(names(liste),nom)]
    names(ltemp) <- str_remove(names(ltemp),paste0(nom,"."))
    res[[nom]] <- ltemp
  }
  names(res) <- lnoms
  return(res)
}

restructure <- function(liste_plate){
  lnoms_plit <- names(liste_plate) %>% str_split("\\.") 
  lannee <- lnoms_plit %>% map_chr(1) %>% unique() %>% as.list()
  res <- selection(liste_plate,lannee)

  return(res)
}


