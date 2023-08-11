#' @title guess_codeZE
#' @description : Détermine la zone d'étude (pôle, EPCI) d'une liste de depcom
#' @param lcom : liste de codes communes
#' @param passageComZE : table de passage entre les communes et le zonage
#'
#' @return : le code de la zone d'étude
#'
#' @examples
#' lcom <- c("35590","35200")
#' passageComZE <- data.frame(CODGEO=c("35590","35200","35750","75000"),AAV2020=c("010","010","010","001"))
#' guess_codeZE(lcom,passageComZE)
#' 
guess_codeZE <- function(lcom ,passageComZE, varCom="CODGEO", varCodeZe="AAV2020"){
  
  stopifnot("guess_codeZE : pb de variables dans passageComZE"=varCom %in% colnames(passageComZE))
  stopifnot("guess_codeZE : pb de variables dans passageComZE"=varCodeZe %in% colnames(passageComZE))
  
  lcom <- as.vector(lcom)
  codeZE <- passageComZE[passageComZE[[varCom]] %in% lcom,]
  codeZE <- codeZE[[varCodeZe]] %>% unique()
  stopifnot("Pb dans guess_codeZE"= length(codeZE)>=1)
  return(codeZE)
}

# lcom = unique(menages$depcom)
# passageComZE = com_poles
# varCom="CODGEO"
# varCodeZe="AAV2020"
