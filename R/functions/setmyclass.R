#' @title setmyclass
#' @description Change la classe des colonnes (en numérique ou caractères)
#' @param tab : data.table avec les variables suivantes : "id","revdecucm","nbpersm","depcom","x","y"
#' @return la même data.table avec les bon formats de variables 

#' @examples 
#' tab <- data.table(id=12,revdecucm=1000,nbpersm=3,depcom="35360",x=1354.3545,y="1545.4684")
#' setmyclass(tab)

setmyclass <- function(tab){
  
  stopifnot(colnames(tab) %>% identical(c("id","revdecucm","nbpersm","depcom","x","y")) )
  
  if(class(tab[,id])!="character") {tab[,id:=as.character(id)];print("Changement pour id")}
  if(class(tab[,revdecucm])!="numeric") {tab[,revdecucm:=as.numeric(revdecucm)];print("Changement pour revdecucm")}
  if(class(tab[,nbpersm])!="numeric") {tab[,nbpersm:=as.numeric(nbpersm)];print("Changement pour nbpersm")}
  if(class(tab[,depcom])!="character"){ tab[,depcom:=as.character(depcom)];print("Changement pour depcom")}
  if(class(tab[,x])!="numeric") {tab[,x:=as.numeric(x)];print("Changement pour x")}
  if(class(tab[,y])!="numeric") {tab[,y:=as.numeric(y)];print("Changement pour y")}
  return(tab)
}

