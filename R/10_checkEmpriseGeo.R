# Test sur l'emprise géographique des bases carroyées


source("R/00a_global.R",encoding = "UTF-8")

get_diametre <- function(car,nom_id="idInspire"){
  car <- car %>% mutate(x= .data[[nom_id]] %>% str_extract("(?<=E)\\d+$") %>% as.numeric(),
                        y= .data[[nom_id]] %>% str_extract("(?<=N)\\d+(?m)") %>% as.numeric())
  
  bbox <- car %>% summarise(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y))
  diametre <- bbox %>% mutate(diametre=((xmax-xmin)^2+(ymax-ymin)^2)^(1/2))
  diametre <- diametre %>% pull(diametre)
  diametre <- round(diametre/10^3)
  return(diametre)
}

#### Pour les AAV ===================================

lcouches <- readRDS(paste0(chemin_output,"pour_utilisateurs/lcouches.RDS"))

lcar <- lcouches %>% modify_depth(2,~.x$`200`$carreaux)
lcar <- lcar %>% unlist(recursive = F)
res_aav <- lcar %>% map_dfr(get_diametre) %>% 
  pivot_longer(everything(),names_to = "var",values_to = "diametre") %>% 
  mutate(annee=paste0("20",str_sub(var,4,5)),
         aav=str_sub(var,7,9)) %>% 
  select(!var)
  
#### Pour les EPCI ===================================

repo <- paste0(chemin_output,"pour_utilisateurs/epci")
files <- dir(repo,full.names = T)
lepci <- map(files,readRDS)
names <- files %>% str_extract( "(?<=_)(\\d*?)(?=.rds)" ) # Récupération du code EPCI
names(lepci) <- names

# Supression des années vides pour les DOM
millesi_dom <- c("men04","men05","men06","men07","men08","men09","men10","men11","men12","men13")
lepci$`249740119`[millesi_dom] <- NULL
lepci$`249740077`[millesi_dom] <- NULL
lepci$`249720061`[millesi_dom] <- NULL


lcar_epci$`200023414` <- lepci %>% modify_depth(2,~ .x$`200`$carreaux)
lcar_epci <- lcar_epci %>% unlist(recursive = F)
res_epci <- lcar_epci %>% map_dfr(get_diametre)  
res_epci <- res_epci %>%  pivot_longer(everything(),names_to = "var",values_to = "diametre") 

