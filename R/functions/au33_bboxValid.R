#' au33_bboxValid
#'
#' Fonction permettant de détecter les coordonnées xy en Lambert 93 invalides car trop éloignées
#' des bbox de la métropole, de la Martinique ou de la Réunion
#' 
#' @param dt data.table avec une variable de dirnoseq, des coordonnées xy  
#' @param vardirno nom de la variable contenant le dirnoseq
#'
#' @return message (erreur ou non)
#' @export
#'
#' @examples
#' # Ex 1 ===================
#' dt <- au33_reproj(dt = lmen$men08[sample(1:nrow(lmen$men08),size = 200000)])
#' au33_bboxValid(dt)
#' # Ex 2 ===================
#' dt <- lmen$men08[sample(1:nrow(lmen$men08),size = 200000)]
#' au33_bboxValid(dt)
#' 
#' @details 
#' Construction des bornes des bbox
#' # reg <- sf::st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2021/franceentiere/reg_franceentiere_2021.shp")
#' metro <- reg %>% filter(!code %in% c("01","02","03","04","06")) %>% sf::st_union()
#' mart <- reg %>% filter(code =="02") %>% sf::st_union()
#' reun <- reg %>% filter(code =="04") %>% sf::st_union()
#' 
#' metro_bbox <- metro %>% sf::st_transform(proj_lambert93) %>%  sf::st_buffer(1000) %>% st_bbox()
#' mart_bbox <- mart %>% sf::st_transform(proj_mart) %>%  sf::st_buffer(1000) %>% st_bbox()
#' reun_bbox <- reun %>% sf::st_transform(proj_reun) %>%  sf::st_buffer(1000) %>% st_bbox()




au33_bboxValid <- function(dt,vardirno="id",verbose=T){
  
  bbox_met <- list(xmin = 98225.97, ymin = 6048646.95 , xmax= 1243375.00 , ymax = 7111479.90 )
  bbox_mart <- list(xmin = 689574.8 , ymin = 1590773.4, xmax= 737110.2 , ymax = 1646739.0 )
  bbox_reun <- list(xmin = 313671.7 , ymin = 7633125.3  , xmax= 380236.4 , ymax = 7692263.4 )
       
  dt <- dt[!is.na(x) & !is.na(y)]
  stopifnot("vardirno not found in dt"=vardirno %in% colnames(dt))
  #dt <- dt[x!="" & y!=""]
  
  # Metropole
  dt_met <- dt[str_sub(dt[[vardirno]],1,2) != "97"]
  dt_met <- dt_met[!(x %between% c(bbox_met$xmin,bbox_met$xmax)) | 
             !(y %between% c(bbox_met$ymin,bbox_met$ymax))]
  problems_met <- dt_met[,.N]
  
  # Martinique
  dt_mart <- dt[str_sub(dt[[vardirno]],1,3) == "972"]
  dt_mart <- dt_mart[!(x %between% c(bbox_mart$xmin,bbox_mart$xmax)) | 
                     !(y %between% c(bbox_mart$ymin,bbox_mart$ymax))]
  problems_mart <- dt_mart[,.N]
  
  #  Réunion
  dt_reun <- dt[str_sub(dt[[vardirno]],1,3) == "974"]
  dt_reun <- dt_reun[!(x %between% c(bbox_reun$xmin,bbox_reun$xmax)) | 
                       !(y %between% c(bbox_reun$ymin,bbox_reun$ymax))]
  problems_reun <- dt_reun[,.N]
  
  if(verbose) {
    if (identical(problems_met + problems_mart + problems_reun, 0L)) {
      message("Aucune coordonnees en dehors de sa bbox")
    } else{
      stop(
        paste0(
          "Presence de coordonnees hors zone (",
          problems_met,
          " en metropole, ",
          problems_mart,
          " en Martinique, ",
          problems_reun,
          " a la Reunion)"
        )
      )
      
    }
  } else{
    return(list(met = dt_met, mart = dt_mart, reun = dt_reun))
  }
  
}




