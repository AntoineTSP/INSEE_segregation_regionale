# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#               Effet du filtrage des carreaux peu peuplés 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @author  Julien PRAMIL
#' @description Etude de l'effet du filtrage sur les indices de Theil

lcouches <- readRDS(paste0(repo_couches,"lcouches.RDS"))

lcouches <- lcouches[c("annee04","annee17")]
lcouches <- transpose(lcouches)


lcouches04 <- lcouches %>% map("annee04")
lcouches17 <- lcouches %>% map("annee17")


# df04 <- lcouches04 %>% map_dbl(function(x) x$pole$c200$sansFiltrage$segreg$theil$segTot) %>% as.data.frame() %>% setNames("theil04")%>% rownames_to_column("ville") 

df04_sansfiltrage <- lcouches04 %>% map_dbl(function(x) x$c200$sansFiltrage$segreg$theil$segTot) %>% as.data.frame() %>% setNames("theil04_sansfiltrage")%>% rownames_to_column("ville") %>% mutate(classniv_sansfiltrage04=51-rank(theil04_sansfiltrage)) 
df04_avecfiltrage <- lcouches04 %>% map_dbl(function(x) x$c200$avecFiltrage$segreg$theil$segTot) %>% as.data.frame() %>% setNames("theil04_avecfiltrage")%>% rownames_to_column("ville") %>% mutate(classniv_avecfiltrage04=51-rank(theil04_avecfiltrage)) 

df17_sansfiltrage <- lcouches17 %>% map_dbl(function(x) x$c200$sansFiltrage$segreg$theil$segTot) %>% as.data.frame() %>% setNames("theil17_sansfiltrage")%>% rownames_to_column("ville") %>% mutate(classniv_sansfiltrage17=51-rank(theil17_sansfiltrage)) 
df17_avecfiltrage <- lcouches17 %>% map_dbl(function(x) x$c200$avecFiltrage$segreg$theil$segTot) %>% as.data.frame() %>% setNames("theil17_avecfiltrage")%>% rownames_to_column("ville") %>% mutate(classniv_avecfiltrage17=51-rank(theil17_avecfiltrage)) 

nbper_sansfiltrage <- lcouches17 %>% map_dfr(~.x$c200$sansFiltrage$carreaux$nbpers_carreau %>% sum()) %>% pivot_longer(cols=everything(),names_to = 'ville',values_to='nbpers17_sansfiltrage')
nbper_avecfiltrage <- lcouches17 %>% map_dfr(~.x$c200$avecFiltrage$carreaux$nbpers_carreau %>% sum()) %>% pivot_longer(cols=everything(),names_to = 'ville',values_to='nbpers17_avecfiltrage')


res <- df17_sansfiltrage %>% 
  left_join(df17_avecfiltrage) %>% 
  left_join(df04_sansfiltrage)%>% 
  left_join(df04_avecfiltrage) %>% 
  left_join(nbper_sansfiltrage)%>% 
  left_join(nbper_avecfiltrage)

res <- res %>% mutate(evol_sansfiltrage=theil17_sansfiltrage/theil04_sansfiltrage-1,
                      classevol_sansfiltrage=51-rank(evol_sansfiltrage),
                      evol_avecfiltrage=theil17_avecfiltrage/theil04_avecfiltrage-1,
                      classevol_avecfiltrage=51-rank(evol_avecfiltrage),
                      evolnbpers=nbpers17_avecfiltrage/nbpers17_sansfiltrage-1
)

# Importation de la table de passage zaav-Cog
passageZaav <- readRDS(paste0(chemin_defZaav,"AAV_definitif_09092020.rds"))


passage_noms <- passageZaav %>% filter(AAV20 %in% str_sub(res$ville,6,8)) %>% mutate(ville=paste0("ville",AAV20)) %>% select(ville,libVille=LIBAAV20) %>% distinct()

res <- res %>% left_join(passage_noms)
saveRDS(res,file = "extrants/impact_filtrage.RDS")

####

res <- readRDS("extrants/impact_filtrage.RDS")
library(plotly)
library(htmlwidgets)

graph_niv17 <- plot_ly(data = res,
                       x=~classniv_sansfiltrage17,
                       y=~classniv_avecfiltrage17,type = "scatter",
                       text = ~libVille,
                       name='positions réelles'
) %>% 
  add_trace(x=1:50,y=1:50,type="scatter",mode="lines",
            name="position inchangée") %>% 
  layout(xaxis=list(title='Classement sans filtrage'),
         yaxis=list(title='Classement avec filtrage'),
         title='Changements de classement des villes selon leur indice de Theil en 2017')
htmlwidgets::saveWidget(graph_niv17,file = paste0(chemin_extrants,"/impactFiltrage/graph_niv17.html"))


graph_niv04 <- plot_ly(data = res,
                       x=~classniv_sansfiltrage04,
                       y=~classniv_avecfiltrage04,type = "scatter",
                       text = ~libVille,
                       name='positions réelles'
) %>% 
  add_trace(x=1:50,y=1:50,type="scatter",mode="lines",
            name="position inchangée") %>% 
  layout(xaxis=list(title='Classement sans filtrage'),
         yaxis=list(title='Classement avec filtrage'),
         title='Changements de classement des villes selon leur indice de Theil en 2004')
htmlwidgets::saveWidget(graph_niv04,file = paste0(chemin_extrants,"/impactFiltrage/graph_niv04.html"))


graph_evol <- plot_ly(data = res,
                      x=~classevol_sansfiltrage,
                      y=~classevol_avecfiltrage,type = "scatter",
                      text = ~libVille,
                      name='positions réelles'
) %>% 
  add_trace(x=1:50,y=1:50,type="scatter",mode="lines",
            name="position inchangée") %>% 
  layout(xaxis=list(title='Classement sans filtrage'),
         yaxis=list(title='Classement avec filtrage'),
         title="Changements de classement des villes selon l'évolution de leur indice de Theil entre 2004 et 2017")
htmlwidgets::saveWidget(graph_evol,file = paste0(chemin_extrants,"/impactFiltrage/graph_evol.html"))




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#        Extraction des indices de Theil pour ANRU et FS
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#' @auteur : Julien PRAMIL
#' @date : 04/02/2021
#' @description code obsolète en partie, juste pour info au cas où

#' Hypothèses :
#' Uniquement Theil
#' Uniquement pôles


# Importation lcouche
lcouches <- readRDS(paste0(repo_couches,"lcouches.RDS"))

# FOnction d'extraction de l'indice global seul (quelque soit taille du carreau)
extrac_segTot <- function(tab_theil){
  if (length(tab_theil)!=1) {
    res <- tab_theil[["segTot"]]
  }else{
    res <- tab_theil
  }
  return(res)
}
  



test <- lcouches %>% lapply(function(tab_annee)
  tab_annee %>% lapply(function(tab_ville)
    tab_ville[["pole"]] %>% lapply(function(tab_carreau)
      tab_carreau[["theil"]] %>% extrac_segTot()
      )))

test2 <- test %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>% 
  rownames_to_column("nom") %>% 
  rename("theil"=V1)
nouv_nom <- str_split(test2$nom,pattern = fixed("."),simplify = T) %>% as.data.frame() # Le point est spécial pour str_split
colnames(nouv_nom) <- c("Annee","codeVille","tailleCarreaux")

test3 <- test2 %>% cbind(nouv_nom)


test3$Annee <- paste0("20",test3$Annee %>% str_remove_all("annee"))
test3$codeVille <- test3$codeVille %>% str_remove_all("ville")
test3$tailleCarreaux <- test3$tailleCarreaux %>% str_remove_all("c")
test3$nom <- NULL


# Mise en forme ---------------------------------

# Vrais nom des pôles

# Importation de la table de passage zaav-Cog
passageZaav <- readRDS(paste0(chemin_defZaav,"AAV_definitif_09092020.rds"))
test4 <- test3 %>% left_join(
  passageZaav %>% select(codeVille=AAV20,libVille=LIBAAV20) %>% distinct()
)

test5 <- test4 %>% select(Annee,codeVille,libVille,tailleCarreaux,theil)

listeFinale <-test5 %>% split(f = test5$tailleCarreaux) 

# Fonction d'enregistrement CSV

enregistreCSV <- function(nom){
  dossier_nom <- paste0(chemin_extrants,"extractions_partenaires/","table_carreaux",nom,".csv")
  write.csv2(listeFinale[[nom]],file = dossier_nom,row.names = F)
}

lapply(names(listeFinale), enregistreCSV)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                         Extraction ANRU
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

lcouches <- readRDS(paste0(repo_couches,"lcouches.RDS"))

lcouches$annee07$ville030$c200$avecFiltrage$segreg$theil$segTot
# 0.114428

typeIndic="theil"
taille_carreau="200"


linputVille <- readRDS(paste0(repo_couches,"liste_AAV_app.RDS"))
AAVpassage <- data.frame(AAV20=linputVille,nomAAV20=names(linputVille))



restri_compar <- function(lcouche_ville,typeIndic="theil",taille_carreau="200",filtrage="avecFiltrage"){
  res <- lcouche_ville[[paste0("c",taille_carreau)]][[filtrage]][["segreg"]][[typeIndic]][["segTot"]] 
  return(res)
}
# $annee04$ville002$c200$avecFiltrage$segreg

donnees_graph <- lcouches %>% 
  lapply(function(couche_annee) couche_annee %>% lapply(restri_compar,typeIndic)) %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rownames_to_column("info") %>% 
  rename(indSeg=".") %>%  
  mutate(annee=info %>% str_sub(6,7),AAV20=info %>% str_sub(14,16)) %>% 
  mutate(annee=paste0("20",annee)) %>% 
  left_join(AAVpassage) %>% 
  select(annee,AAV20,nomAAV20,indSeg)


donnees_graph <- donnees_graph %>% arrange(AAV20)

write.csv2(donnees_graph,row.names = F,
           file = "extrants/extractions_partenaires/indices_theil_poles_mai2021.csv")

lcouches$annee07$ville030$c200$avecFiltrage$segreg$theil$segTot



