# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# #      Fonctions et figures nécessaires au tableau de bord
# # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# 
# # input <- list()
# # input$zaav <- "030"
# # #input$nivAgglo <- "zaavTot"
# # #input$taille_carreau <- "200"
# # #input$filtrage <- "avecFiltrage"
# # input$rebase1 <- T
# # input$rebase2 <- T
# 
# #### Couleurs
# 
# couleurGris <- 'rgba(228, 222, 249, 0.65)'
# couleurOrange <- 'rgba(253,194,12,0.65)'
# couleurBleu <- 'rgba(103,142,199,0.65)'
# couleurBleuClair <- 'rgba(203,221,245,0.65)'
# couleurRose <- 'rgba(150,70,121,0.65)'
# couleurVert <- 'rgba(26,164,87,0.65)'
# 
# couleurGris_html <- rgb(228, 222, 249, 0.65,maxColorValue = 255)
# couleurOrange_html <- rgb(253,194,12,0.65,maxColorValue = 255)
# couleurBleuClair_html <- rgb(203,221,245,0.65,maxColorValue = 255)
# couleurBleu_html <- rgb(103,142,199,0.65,maxColorValue = 255)
# couleurRose_html <- rgb(150,70,121,0.65,maxColorValue = 255)
# couleurVert_html <- rgb(26,164,87,0.65,maxColorValue = 255)
# 
# 
# #### Sélection ZAAV, nivAgglo et taille_carreau
# 
# restricCouches <- function(zaav,taille_carreau="200",filtrage="avecFiltrage"){
#   lcouches_restric <- lcouches %>% 
#     lapply(function(tab) tab[[paste0("ville",zaav)]]) %>% 
#     # lapply(function(tab) tab[[nivAgglo]]) %>% 
#     lapply(function(tab) tab[[paste0("c",taille_carreau)]]) %>% 
#     lapply(function(tab) tab[[filtrage]])
#   return(lcouches_restric)
# }
# 
# 
# ######### Extraction des listes Theil et IIM
# extracListes <- function(lcouches_restric){ #lcouches_restric=maCouche
#   maListeTemp <- lcouches_restric %>% lapply(function(tab) tab[["segreg"]])
#   maListeTheil <- maListeTemp %>% lapply(function(tab) tab[["theil"]])
#   maListeIIM <- maListeTemp %>% lapply(function(tab) tab[["iim"]])
#   res <- list(maListeTheil=maListeTheil,maListeIIM=maListeIIM)
#   return(res)
# }
# 
# #### Créations tables des indices totaux
# 
# transfo_tabIndiceTot <- function(maListe_indic){ #maListe_indic=maListe$maListeTheil
#   tabIndiceTot <- maListe_indic %>% lapply(function(x) x[["segTot"]])
# 
#   tabIndiceTot <- do.call(rbind, tabIndiceTot) %>%
#     as.data.frame() %>%
#     rownames_to_column("annee") %>%
#     mutate(annee=paste0("20",annee %>% str_sub(6,8)) %>% as.numeric()) %>%
#     rename(segTot="V1")
#   
#   
#   return(tabIndiceTot)
# }
# 
# 
# ####
# 
# rebaseTable <- function(maTable,rebase){ #maTable=maListe$maListeTheil %>% transfo_tabIndiceTot()
#   if(rebase){
#     maTable <- maTable %>%  mutate(segTot=100*segTot/as.numeric(maTable$segTot[maTable$annee==2004]))
#   }
#   return(maTable)
# }
# 
# 
# ####
# 
# graphSimple <- function(maTable){
#   fig <- plot_ly(maTable, 
#                  x = ~annee, 
#                  y = ~segTot, 
#                  type = 'scatter', mode = 'lines+markers') 
#   return(fig)
# }
# 
# 
# 
# ####### Fonctions pour les graph de comparaisons (besoin de toutes les villes)
# 
# 
# restri_compar <- function(lcouche_ville,typeIndic="theil",taille_carreau="200",filtrage="avecFiltrage"){
#   res <- lcouche_ville[[paste0("c",taille_carreau)]][[filtrage]][["segreg"]][[typeIndic]][["segTot"]] 
#   return(res)
# }
# 
# extrac_compar <- function(lcouches,typeIndic="theil"){
#   
#   # Extraction et mise en forme
#   donnees_graph <- lcouches %>% 
#     lapply(function(couche_annee) couche_annee %>% lapply(restri_compar,typeIndic)) %>% 
#     unlist() %>% 
#     as.data.frame() %>% 
#     rownames_to_column("info") %>% 
#     rename(indSeg=".") %>%  
#     mutate(annee=info %>% str_sub(6,7),AAV20=info %>% str_sub(14,16)) %>% 
#     mutate(annee=paste0("20",annee)) %>% 
#     left_join(AAVpassage) %>% 
#     select(annee,AAV20,nomAAV20,indSeg)
#   
#   return(donnees_graph)
# }
# 
# 
# graphCompar <- function(donnees_graph,zaav){
#   
#   # Paramétrage couleurs/épaisseur dans la base (en fonction de la ville choisie)
#   donnees_graph <- donnees_graph %>% 
#     mutate(couleur = if_else(AAV20==zaav,"blue","orange"),
#            epaiss = if_else(AAV20==zaav,4,0.5)
#     )
#   
#   # Initialisation du graph vide
#   chart <- plot_ly(type = "scatter",mode="lines")
#   
#   for (ville in c(donnees_graph$nomAAV20[donnees_graph$AAV20!=zaav] %>% as.vector() %>% unique(),
#                   donnees_graph$nomAAV20[donnees_graph$AAV20==zaav] %>% as.vector() %>% unique()
#   )
#   ) {
#     donnees_boucle <- donnees_graph %>% filter(nomAAV20==ville)
#     chart <- chart %>% add_trace(data=donnees_boucle,
#                                  x=~annee, 
#                                  y=~indSeg, 
#                                  name=ville,
#                                  line=list(width=~epaiss,
#                                            color=~couleur
#                                  )
#     )
#   }
#   
#   return(chart)
# }
# 
# 
# 
# # *************************************************
# #        Décomposition QP/ hors QP ---------------
# 
# 
# # Distribution des habitants des QP
# 
# graph_distriQP <- function(maCouche) {
#   l0 <- setNames(maCouche[c("annee04", "annee17")], c("2004", "2017"))
#   
#   fliste <- function(l) {
#     etape1 <- l$carreaux %>% filter(idcarreau %in% l$id_carreaux_QP)
#     
#     vec <- which(str_detect(colnames(etape1), "Tranche"))
#     etape2 <-
#       etape1 %>% summarise_at(vec,  ~ 100 * sum(.x) / sum(nbpers_carreau))
#     colnames(etape2) <-
#       colnames(etape2) %>% str_replace_all("nbPersTranche", "g")
#     res <-
#       etape2 %>% pivot_longer(everything(), names_to = "groupe", values_to =
#                                 "part")
#     return(res)
#   }
#   
#   data_graph <-
#     l0 %>% map2_dfr(names(l0),  ~ fliste(.x) %>% mutate(annee = .y))
#   
#   nb_groupes <- length(unique(data_graph$groupe))
#   pal <- brewer.pal(nb_groupes, "RdYlBu")
#   if (nb_groupes %% 2 == 1)
#     pal[floor(nb_groupes / 2) + 1] <- "#7f8c8d"
#   
#   fig <- plot_ly(
#     data = data_graph,
#     x =  ~ annee,
#     y =  ~ part,
#     type = "bar",
#     color =  ~ groupe,
#     colors = pal
#   ) %>%
#     layout(
#       title = "Distribution des habitants des QP parmi les groupes sociaux",
#       yaxis = list(title = "part en %"),
#       xaxis = list(title = NA)
#     )
#   return(fig)
# }
# 
# # Evolution de la part d'habitants en QP
# 
# graph_evolPartQP <- function(maListe){
#   
#   vec_annees <- paste0("20",str_remove_all(maListe$maListeIIM %>% names(),"annee"))
#   
#   dta <- maListe$maListeIIM %>% 
#     map("decompo_qp")%>% 
#     map2_dfr(vec_annees,~.x["teta_QP"] %>% as_tibble() %>% mutate(annee=.y))
#   
#   dta <- dta %>% mutate(teta_QP=100*teta_QP/teta_QP[1])
#   fig <- plot_ly(
#     data = dta,
#     x =  ~ annee,
#     y =  ~ teta_QP,
#     colors = couleurBleu_html,
#     type = "scatter",
#     mode = "line+marker"
#   ) %>% layout(
#     title = "Evolution de la part des habitants vivant en QP",
#     xaxis = list(title = NA),
#     yaxis = list(title = "Indice base 100 en 2004")
#   )
#   return(fig)
# }
# 
# 
# 
# 
# 
# # Tableau de donnees sur la decomposition QP-horsQP
# returnTabContrib <- function(maListe_indice){#maListe_indice=maListe$maListeTheil
#   
#   maTable <- maListe_indice %>% 
#     lapply(function(x) x[c("segTot","decompo_qp")] %>% unlist())
#   maTable <- do.call(rbind,maTable)
#   maTable <- maTable %>% 
#     as.data.frame() %>% 
#     rownames_to_column("annee") %>% 
#     mutate(annee=paste0("20",str_sub(annee,6,7))) %>% 
#     mutate_if(is.numeric,round,4)
#   colnames(maTable) <- colnames(maTable) %>% str_remove_all("decompo_qp.")
#   
#   #Calcul des contributions
#   maTable$contrib_QP <- (maTable$teta_QP*maTable$indice_QP) %>% round(4)
#   maTable$contrib_horsQP <- (maTable$teta_horsQP*maTable$indice_horsQP) %>% round(4)
#   
#   return(maTable)
#   
# }
# 
# 
# 
# 
# # Graphique d'evolution des indices QP et indices hors QP (niveau)
# 
# graph_IndiceIntra <- function(tabContrib) { #tabContrib=tabContribTheil
#     data <- tabContrib %>%
#       select(annee, indice_QP, indice_horsQP) %>%
#       pivot_longer(cols = !annee,
#                    names_to = "type_indice",
#                    values_to = "val")
#     
#     fig <- plot_ly(
#       data = data,
#       x =  ~ annee,
#       y =  ~ val,
#       color =  ~ type_indice,
#       colors = c(couleurBleu_html, couleurBleuClair_html),
#       type = "scatter",
#       mode = "line+marker"
#     ) %>% layout(
#       title = "Indices de segregation 'intra' en niveau",
#       xaxis = list(title = NA),
#       yaxis = list(title = NA)
#     )
#     return(fig)
#   }
# 
# 
# 
# 
# tableau_contribQP <- function(maTable){ #tabContribTheil
#   couleurs_cols <- c("black",couleurVert,rep("white",4),couleurOrange,couleurBleu,couleurBleuClair)
#   
#   res <- plot_ly(
#     type = 'table',
#     header = list(
#       values = names(maTable),
#       align = rep('center', ncol(maTable)),
#       line = list(width = 1, color = 'black'),
#       fill = list(color = couleurs_cols),
#       font = list(family = "Arial", size = 14, color = c("white","black"))
#     ),
#     cells = list(
#       values = rbind(
#         t(as.matrix(unname(maTable)))
#       ),
#       align = rep('center', ncol(maTable)),
#       line = list(color = "black", width = 1),
#       fill = list(color = couleurs_cols),
#       font = list(family = "Arial", size = 12, color = c("white","black"))
#     )) %>% layout(
#       title="Decomposition detaillee 'QP-hors QP' de la segregation"
#     )
#   return(res)
# }
# 
# 
# 
# 
# returngraphContribBar <- function(maTable){ # maTable=tabContribTheil
#   if("contrib_QP" %in% colnames(maTable)){
#   
#     fig <- plot_ly(maTable, x = ~annee, y = ~contrib_QP, type = 'bar', name = 'contrib_QP')
#     fig <- fig %>% add_trace(y = ~contrib_horsQP, name = 'contrib_horsQP')
#     fig <- fig %>% add_trace(y = ~indice_inter, name = 'contrib_inter')
#     fig <- fig %>% add_trace(y = ~segTot,type = 'scatter', mode = 'lines+markers', name = 'Total') #,line=list(color=couleurBleu)
#     fig <- fig %>% layout(yaxis = list(title = ''),
#                         xaxis=list(title=NA),
#                         barmode = 'stack',
#                         title= "Contributions 'inter' et 'intra' a la segregation totale",
#                         colorway=c(couleurBleu,couleurBleuClair,couleurOrange,couleurVert))
#   }else{
#     fig <- NULL}
#   return(fig)
# }
# 
# 
# returngraphContribBarPct <- function(maTable){ # maTable=tabContribTheil
#   if("contrib_QP" %in% colnames(maTable)){
#     
#     fig <- plot_ly(maTable, x = ~annee, y = ~100*contrib_QP/segTot, type = 'bar', name = 'contrib_QP')
#     fig <- fig %>% add_trace(y = ~100*contrib_horsQP/segTot, name = 'contrib_horsQP')
#     fig <- fig %>% add_trace(y = ~100*indice_inter/segTot, name = 'contrib_inter')
#     fig <- fig %>% add_trace(y = ~100*segTot/segTot,type = 'scatter', mode = 'lines+markers', name = 'Total') #,line=list(color=couleurBleu)
#     fig <- fig %>% layout(yaxis = list(title = 'en %'),
#                           xaxis=list(title=NA),
#                           barmode = 'stack',
#                           title= "Parts des contributions 'inter' et 'intra' a la segregation totale",
#                           colorway=c(couleurBleu,couleurBleuClair,couleurOrange,couleurVert))
#   }else{
#     fig <- NULL}
#   return(fig)
# }
# 
# 
# 
# graphContribEvol <- function(tabContrib){ #tabContrib=tabContribTheil
#   
#   dta <- tabContrib %>% mutate_at(
#     vars(c("segTot","indice_inter","contrib_QP","contrib_horsQP")),
#     ~ 100*.x/.x[[1]] ) %>% 
#     select(annee,segTot,indice_inter,contrib_QP,contrib_horsQP)
#   
#   dta <- dta %>% 
#     pivot_longer(!annee,
#                  names_to="type_indice",
#                  values_to="val")
#   
#   fig <- plot_ly(
#     data = dta,
#     x =  ~ annee,
#     y =  ~ val,
#     color =  ~ type_indice,
#     colors = c(couleurBleuClair_html,couleurBleu_html, couleurOrange_html,couleurVert_html),
#     type = "scatter",
#     mode = "line+marker"
#   ) %>% layout(
#     title = "Evolutions des contributions de la decomposition 'QP - hors QP'",
#     xaxis = list(title = NA),
#     yaxis = list(title = "Indice en base 100 en 2004")
#   )
#   return(fig)
#   
#                                     
# }
# 
# 
# # ******************************************************************
# #       Fonctions pour la décomposition par groupes sociaux
# 
# 
# returnTabContrib_groupes <- function(maListe_indice){ #maListe_indice=maListe$maListeTheil
# 
#   lseg <- maListe_indice %>% map("segTot") %>% map(~tibble("segTot"=.x))
#   noms_groupes <- names(maListe_indice$annee04$decompo_groupes)
#   
#   liste_res <- list()
#   for (g in noms_groupes) {
#     lcompo <- maListe_indice %>%
#       map("decompo_groupes") %>% 
#       map(g) %>% 
#       map(as_tibble)
#     liste_res[[g]] <- map2_dfr(lseg,lcompo,cbind,.id = "annee")
#   }
#   
#   liste_res <- liste_res %>% map(~ .x %>% mutate(annee=paste0("20",annee %>% str_remove("annee")))) %>% 
#     map(~.x %>% mutate(contrib_binaire=teta_binaire*indice_binaire,
#                        contrib_multi=teta_multi*indice_multi))
#   
#   return(liste_res)
# 
# }
# 
# 
# tableau_contribGroupes <- function(maTable){
#   couleurs_cols <- c("black",couleurVert,rep("white",4),couleurOrange,couleurBleuClair)
#   
#   maTable <- maTable %>% mutate_if(is.numeric,round,3)
#   
#   res <- plot_ly(
#     type = 'table',
#     header = list(
#       values = names(maTable),
#       align = rep('center', ncol(maTable)),
#       line = list(width = 1, color = 'black'),
#       fill = list(color = couleurs_cols),
#       font = list(family = "Arial", size = 14, color = c("white","black"))
#     ),
#     cells = list(
#       values = unname(maTable),
#       align = rep('center', ncol(maTable)),
#       line = list(color = "black", width = 1),
#       fill = list(color = couleurs_cols),
#       font = list(family = "Arial", 
#                   size = 12, 
#                   color = c("white","black"))
#     ))
#   return(res)
# }
# #tableau_contribGroupes(tabContribGroupes_iim$g1)
# 
# graph_contribBinaire <- function(liste){#liste=tabContribGroupes_iim
#   data0 <- liste %>% map(~.x %>% mutate(part_binaire=100*contrib_binaire/segTot) %>% select(annee,part_binaire))
#   data1 <- map2_dfr(data0,names(data0),~.x %>% mutate(groupe=.y))
#   
#   pal <- brewer.pal(length(data0), "RdYlBu")
#   if(length(data0)%%2==1) pal[floor(length(data0)/2)+1] <- "#7f8c8d"
#   
#   
#   res <- plot_ly(data = data1, x=~annee, y=~part_binaire,
#           color = ~groupe,type = "scatter",mode="lines+markers",
#           colors = pal
#   ) %>% 
#     layout(
#       title="Contribution de la composante binaire de chaque groupe a la segregation totale",
#       yaxis=list(
#         title="Part de la composante binaire (en %)"
#       ),
#       xaxis=list(
#         title=""
#       )
#     )
#   return(res)
# }
# 
# graph_indiceBinaire <- function(liste,base100=F){#liste=tabContribGroupes_theil
#   
#   data1 <- map2_dfr(liste,names(liste),~.x %>% select(annee,indice_binaire) %>% mutate(groupe=.y))
#   
#   nom_axeY <- "Composante binaire"
#   
#   # Transformation en base 100 en 2004 si demandé
#   if(base100){
#     data1 <- data1 %>% 
#       group_by(groupe) %>% 
#       mutate(indice_binaire=100*indice_binaire/indice_binaire[annee=='2004'])
#     nom_axeY <- "Composante binaire en base 100"
#   }
#   
#   pal <- brewer.pal(length(liste), "RdYlBu")
#   if(length(liste)%%2==1) pal[floor(length(liste)/2)+1] <- "#7f8c8d"
#   
#   
#   res <- plot_ly(data = data1, x=~annee, y=~indice_binaire,
#                  color = ~groupe,type = "scatter",mode="lines+markers",
#                  colors = pal
#   ) %>% 
#     layout(
#       title="Composantes binaires de chaque groupe",
#       yaxis=list(
#         title=nom_axeY
#       ),
#       xaxis=list(
#         title=""
#       )
#     )
#   return(res)
# }
# 
# 
# 
# 
# 
# #### Compléments ***********************
# 
# 
# 
# 
# tab_effectif <- function(maCouche){
#   
#   maCouche_carreau <- maCouche %>% map("carreaux")
#   maCouche_carreau <- maCouche_carreau %>% lapply(function(x) c(`Nombre d'individus`=sum(x$nbpers_carreau),`Nombre de menages`=sum(x$nbmen_carreau)))
#   
#   tab <- do.call(rbind,maCouche_carreau) %>% 
#     as.data.frame() %>% 
#     rownames_to_column("annee") %>% 
#     mutate(annee=paste0("20",annee %>% str_sub(6,8)) )
#   
#   
#   tab <- plot_ly(tab,
#                  type = "table",
#                  header = list(
#                    values = colnames(tab),
#                    align = rep('center', ncol(tab)),
#                    line = list(width = 1, color = 'black'),
#                    fill=list(color=c("grey",rep('#119DFF',ncol(tab)-1))),
#                    font = list(family = "Arial", size = 14, color = "white")
#                  ),
#                  cells = list(
#                    values = unname(tab),
#                    align = rep('center', ncol(tab)),
#                    line = list(color = "black", width = 1),
#                    fill=list(color=c("grey",rep('white',ncol(tab)-1))),
#                    font = list(family = "Arial", size = 12, color = c("white","black"))
#                  )
#   ) %>% 
#     layout(title="Effectifs totaux dans la zone d'etude")
#   
#   
#   
#   return(tab)
# }
# 
# 
# 
# 
# 
# ########## Tableau des déciles de ménages/individus
# 
# tab_deciles <- function(maCouche,type="men"){
#   
#   
#   if(type=="men"){
#     variab <- "nbmen_carreau"
#     tabTitre <- "Deciles du nombre de menages par carreau"
#     tabTitre2 <- "Nombre de menages contenus dans chaque decile de carreaux"
#   }else{
#       variab <- "nbpers_carreau"
#       tabTitre <- "Deciles du nombre d'individus par carreau"
#       tabTitre2 <- "Nombre d'individus contenus pour chaque decile de carreaux"
#       }
#   
#   
#   # Calculs des déciles et création d'une table
#   classif17 <- maCouche$annee17$carreaux[[variab]] %>% classIntervals(n = 10)
#   classif04 <- maCouche$annee04$carreaux[[variab]] %>% classIntervals(n = 10)
#   
#   
#   maTable <- cbind('2004'=classif04$brks %>% round(1),'2017'=classif17$brks %>% round(1)) %>% t() %>% as.data.frame()
#   colnames(maTable) <- c("min",paste0("decile",1:9),"max")
#   maTable$annee <- row.names(maTable)
#   pos <- (colnames(maTable)=="annee") %>% which()
#   maTable <- maTable[,c(pos,(1:ncol(maTable))[-pos])]
#   rownames(maTable) <-NULL 
#   
#   # Mise en forme de la table des déciles avec plotly
#   res_def <- plot_ly(maTable,
#           type = "table",
#           header = list(
#             values = colnames(maTable),
#             align = rep('center', ncol(maTable)),
#             line = list(width = 1, color = 'black'),
#             fill=list(color=c("grey",rep('#119DFF',ncol(maTable)-1))),
#             font = list(family = "Arial", size = 14, color = "white")
#           ),
#           cells = list(
#             values = unname(as.matrix(maTable)) %>% t() ,
#             align = rep('center', ncol(maTable)),
#             line = list(color = "black", width = 1),
#             fill=list(color=c("grey",rep('white',ncol(maTable)-1))),
#             font = list(family = "Arial", size = 12, color = c("white","black"))
#           )
#   ) %>% 
#     layout(title=tabTitre)
#   
#   
#   # Calcul des effectifs de population par déciles
#   
#   maTable_pop <- maCouche$annee17$carreaux %>% cbind(deciles=findCols(classif17)) %>% 
#     group_by(deciles) %>% 
#     summarise(nombre=sum(.data[[variab]])) %>% 
#     mutate(proportion=(100*nombre/sum(nombre)) %>% round(1))
#   
#   # Mise en forme de la table des effectifs par décile avec plotly
#   res_effect <- plot_ly(maTable_pop,
#                  type = "table",
#                  header = list(
#                    values = colnames(maTable_pop),
#                    align = rep('center', ncol(maTable_pop)),
#                    line = list(width = 1, color = 'black'),
#                    fill=list(color=c("grey",rep('#119DFF',ncol(maTable_pop)-1))),
#                    font = list(family = "Arial", size = 14, color = "white")
#                  ),
#                  cells = list(
#                    values = unname(as.matrix(maTable_pop)) %>% t(),
#                    align = rep('center', ncol(maTable_pop)),
#                    line = list(color = "black", width = 1),
#                    fill=list(color=c("grey",rep('white',ncol(maTable_pop)-1))),
#                    font = list(family = "Arial", size = 12, color = c("white","black"))
#                  )
#   ) %>% 
#     layout(title=tabTitre2)
#   
#   return(list(res_def=res_def,res_effect=res_effect))
# }
# 
# 
# 
# 
# ##################################################################
# # Table du nombre de carreaux par année (ayant au moins un ménage)
# 
# nbCarreaux_partiel <- function(liste_annee){
#   res <- liste_annee$carreaux %>% nrow()
#   return(res)
# }
# 
# nbCarreaux <- function(maCouche){
#   liste_nb <- lapply(maCouche, nbCarreaux_partiel)
#   tab_nb <- liste_nb %>% as.data.frame() %>%  
#     t() %>% as.data.frame() %>% 
#     rownames_to_column(var=".")
#   colnames(tab_nb) <- c("Annee","Nombre de carreaux")
#   tab_nb <- tab_nb %>% mutate(Annee=paste0("20",str_sub(Annee,6,7)))
#   
#   # Mise en forme plotly
#   
#   tab_nb <- plot_ly(tab_nb,
#                         type = "table",
#                         header = list(
#                           values = colnames(tab_nb),
#                           align = rep('center', ncol(tab_nb)),
#                           line = list(width = 1, color = 'black'),
#                           fill=list(color=c("grey",rep('#119DFF',ncol(tab_nb)-1))),
#                           font = list(family = "Arial", size = 14, color = "white")
#                         ),
#                         cells = list(
#                           values = unname(tab_nb),
#                           align = rep('center', ncol(tab_nb)),
#                           line = list(color = "black", width = 1),
#                           fill=list(color=c("grey",rep('white',ncol(tab_nb)-1))),
#                           font = list(family = "Arial", size = 12, color = c("white","black"))
#                         )
#   ) %>% 
#     layout(title="Nombre de carreaux dans la zone d'etude (avec 1 menage au moins)")
#   
#   return(tab_nb)
#   
#   
#   
# }
# 
# # def_quantiles <- function(maCouche){
# #   
# #   vec_annees <- paste0("20",str_remove_all(maCouche %>% names(),"annee"))
# #   vec_quant0 <- maCouche$annee04$quant
# #   nb_groupes <- length(vec_quant0)-1
# #   bornes <- c("min",paste0("q",1:(nb_groupes-1)),"max")
# #   
# #   dta <- maCouche %>% map2_dfr(vec_annees,~ c(annee=.y,setNames(.x[["quant"]] %>% round(),bornes) ))
# # 
# #   fig <- plot_ly(dta,
# #                     type = "table",
# #                     header = list(
# #                       values = colnames(dta),
# #                       align = rep('center', ncol(dta)),
# #                       line = list(width = 1, color = 'black'),
# #                       fill=list(color=c("grey",rep('#119DFF',ncol(dta)-1))),
# #                       font = list(family = "Arial", size = 14, color = "white")
# #                     ),
# #                     cells = list(
# #                       values = unname(as.matrix(dta)) %>% t(),
# #                       align = rep('center', ncol(dta)),
# #                       line = list(color = "black", width = 1),
# #                       fill=list(color=c("grey",rep('white',ncol(dta)-1))),
# #                       font = list(family = "Arial", size = 12, color = c("white","black"))
# #                     )
# #   ) %>% 
# #     layout(title="Definition des quantiles de revenus declares")
# #   
# #   return(fig)
# # }
# 
