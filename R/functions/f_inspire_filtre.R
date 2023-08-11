#' f_inspire_filtre
#'
#' @param dtmen : dt base ménages avec : 
#'                 - depcom
#' @param liste_taille_carreaux : liste des tailles de carreaux possibles
#' @param com_poles : table des communes des différents pôles retenus
#' @param nbmen_min : nombre de ménage minimal par carreau
#'
#' @return dt de la base ménage en entrée avec des variables supplémentaires :
#'          - idInspire 
#'          - et filtre (1 version par taille de carreaux) pour savoir si le ménage appartient à un carreau contenant au moins (nbmen_min) ménages
#' @export
#' @details : utilise les fonctions : 
#'                 - guess_proj
#'                 - guess_codeZE
#'                 - add_centroides
#'                 - add_inspire
#' 
#' @examples
#' 
f_inspire_filtre <- function(dtmen,liste_taille_carreaux,com_poles,nbmen_min, varCom="CODGEO", varCodeZe="AAV2020"){
  
  if(dtmen[,.N]==0){
    dt <- NA
  }else{
    dt <- copy(dtmen)
    stopifnot("f_inspire_filtre : dtmen n'est pas un data.table"=is.data.table(dt))
    stopifnot("f_inspire_filtre : variable depcom absente"=!is.null(dt$depcom))
    crs <- guess_proj(dt$depcom)
    message(guess_codeZE(unique(dt$depcom),passageComZE = com_poles, varCom, varCodeZe))
    
    for(tcar in liste_taille_carreaux){ #tcar <- 300
      print(tcar)
      dt <- add_centroides(dt,tcar)
      dt <- add_inspire(dt,tcar,crs)
      idInspireCar <- paste0("idInspire",tcar)
      setnames(dt,old = "idInspire", new = idInspireCar)
      dt[,":="(x_centroide=NULL,y_centroide=NULL)]
      dt[,nb:=.N,by=get(idInspireCar)]
      filtreCar <- paste0("filtre",tcar)
      dt[,c(filtreCar):=F]
      dt[nb>=nbmen_min,c(filtreCar):=T][,nb:=NULL]
    }
  }
  return(dt)
}