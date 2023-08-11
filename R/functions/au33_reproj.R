
#' au33_reproj
#' 
#' Permet de reprojeter un data.table contenant des coordonnées xy dans un nouveau système de projection
#'
#' @param dt : data.frame with xy coordinates
#' @param from_proj : initial projection systeme
#' @param to_proj : final projection needed
#' @param keyvar : result key
#'
#' @return data.table with new projection system
#'
#' @examples
#' au33_reproj(dt = lmen_test$men08[1:10000])




au33_reproj <- function(dt,from_proj=27572,to_proj=2154,keyvar="id"){
  
  stopifnot("dt must be a data.table"=is.data.table(dt))
  stopifnot("No xy variables"= (!is.null(dt$x)) & (!is.null(dt$y)))
  stopifnot("Keyvar not found in dt"= !is.null(dt[[keyvar]]) )
  
  if(dt[,.N]>0){
    dttransform <- dt[!is.na(x) & !is.na(y)] %>% 
      st_as_sf(coords=c("x","y"),crs=from_proj) %>% 
      st_transform(to_proj) %>% 
      mutate(x=st_coordinates(geometry)[,1],
             y=st_coordinates(geometry)[,2]) %>% 
      st_drop_geometry() %>% 
      as.data.table()
    
    res <- rbind(
      dt[is.na(x) | is.na(y)],
      dttransform
    )
    #setkeyv(res,keyvar)
    
    stopifnot("Different number of rows between input and output"=res[,.N]==dt[,.N])
    
    return(res)
  }else{
    return(dt)
  }
  
}


