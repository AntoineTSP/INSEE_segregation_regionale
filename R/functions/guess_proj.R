#' @title guess_proj
#' @description : Détermine la projection à utiliser à partir d'une liste de depcom
#' @param lcom : liste de codes communes
#'
#' @return : le code epsg
#'
#' @examples
#' lcom <- c("35590","35200")
#' guess_proj(lcom)

guess_proj <- function(lcom){
  
  stopifnot("guess_proj : lcom vide"=length(lcom)>0)
  
  if(sum(str_detect(lcom,"^97"))==0){ 
    return(proj_lambert93)
  }else if(sum(str_detect(lcom,"^972"))==length(lcom)){
    return(proj_mart)
  }else if(sum(str_detect(lcom,"^974"))==length(lcom)){
    return(proj_reun)
  }else{
        stop("guess_proj : probleme dans la liste des communes")
      }
}




