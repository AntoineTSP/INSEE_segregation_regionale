#### *********************************************
####            Importation inputs           -----
source("R/00a_global.R")
# data <- readRDS("df_centro_smooth")
#Path du projet
Path_AU33 <- "U:/AU33"
#Pallette de couleur
Palette <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
#Initialisation
datadash_base_100 <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>% select(annee)
#Chargement d'une fonction
generHisto <- function(p1,p2,p3,p4,p5,titre){
  
  x <- c('q1','q2','q3','q4','q5')
  y <- c(p1,p2,p3,p4,p5)
  res <- NULL
  
  if(p1+p2+p3+p4+p5==100){
    y <- round(y, 0)
    data <- data.frame(x, y)
    
    fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                   text = y, textposition = 'auto',
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(title = titre,
                          xaxis = list(title = ""),
                          yaxis = list(title = "Part de la population (%)",
                                       range=c(0,100)))
    res <- fig
  }
  return(res)
}

#### *********************************************
#### end ####

#### *********************************************
####            Cartographie                 -----

#Début du travail
for (year in millesi){
  #Répertoire de la carto pour l'année en question
  Repertoire_annee <- paste0(Path_AU33, "/Carto/20", as.character(year))
  
  # Vérifie si le répetoire existe
  if (file.exists(Repertoire_annee)){
    for(taille in liste_taille_carreaux){
      data <- df_centro_region_smooth %>% filter(reg %in% c(reg) & annee %in% c(year) & tcar %in% c(as.character(taille)))
      
      data <- data %>%mutate(Polygone = paste0("POLYGON ((", 
                                               as.character(data$long_hg), " ", as.character(data$lat_hg), ",",
                                               as.character(data$long_hg), " ", as.character(data$lat_hg + taille), ",",
                                               as.character(data$long_hg +taille), " ", as.character(data$lat_hg + taille), ",",
                                               as.character(data$long_hg + taille), " ", as.character(data$lat_hg), ",",
                                               as.character(data$long_hg), " ", as.character(data$lat_hg) , "))")
                             )
      
      data <- data %>%mutate(centre_x=data$long_hg + taille/2,
                             centre_y=data$lat_hg + taille/2
                             )
      
      write.csv(data,paste0("U:/AU33/Carto/20", year, "/df_", as.character(taille) , ".csv"))
    }
  } else {
    # Crée le répertoire
    dir.create(file.path(paste0(Path_AU33, "/Carto"), paste0("20", as.character(year))))
  }
}

#### *********************************************
#### end ####

#### *********************************************
####            Statistique Filtre           -----

#####Visualisation datadash
#Statistiques qui éclairent le filtrage (>20 ménages par carreau)
for(tcar in liste_taille_carreaux){
  taille <- tcar
  source("R/05_data_dashboard_region.R")
  #Proportion gardée
  #Nb_carreaux_gardés
  Nb_total_carreaux_gardes <-datadash[["44"]][["data_nb_carreaux"]]
  Nb_total_carreaux_gardes <- setNames(Nb_total_carreaux_gardes, c ("Annee" , paste0("Carreaux de taille ",taille, " retenus")))
  Nb_total_carreaux_gardes <- Nb_total_carreaux_gardes %>% select(-Annee)
  row.names(Nb_total_carreaux_gardes) <- paste0("Annee 20",millesi)
  
  
  Nb_total_menages_gardes <- datadash[["44"]][["data_effectifs"]]
  Nb_total_menages_gardes <- setNames(Nb_total_menages_gardes, c ("Annee" , paste0("Nb_individu_retenus_", as.character(taille)), paste0("Nb_men_retenus_", as.character(taille))))
  Nb_total_menages_gardes <- Nb_total_menages_gardes %>% select(-Annee)
  row.names(Nb_total_menages_gardes) <- paste0("Annee 20",millesi)
  
  # #Save statistiques gardes
  # write.csv(Nb_total_carreaux_gardes,paste0("U:/AU33/Statistiques/Nb_total_carreaux_retenus_" , as.character(taille) , ".csv"))
  # write.csv(Nb_total_menages_gardes,paste0("U:/AU33/Statistiques/Nb_total_menages_retenus_", as.character(taille) , ".csv"))
  
  
  #Jointure
  Statistiques <- Nb_total_population %>% mutate(rn = row_number()) %>% 
    left_join(Nb_total_menages %>% mutate(rn = row_number())) %>% 
    left_join(Nb_total_carreaux %>% select(paste0("Carreau de taille ",taille)) %>% mutate(rn = row_number())) %>% 
    left_join(Nb_total_menages_gardes %>% mutate(rn = row_number())) %>% 
    left_join(Nb_total_carreaux_gardes %>% mutate(rn = row_number())) %>% 
    select (-rn)
  row.names(Statistiques) <- paste0("Annee 20",millesi)
  
  Names <- names(Statistiques)
  
  Statistiques <- Statistiques %>% mutate(
    "%_men_retenus" = round(Statistiques[[5]] *100 / Statistiques[[2]],1) ,
    "%_individus_retenus" = round(Statistiques[[4]] *100 / Statistiques[[1]],1) ,
    "%_carreaux_retenus" = round(Statistiques[[6]] *100 / Statistiques[[3]],1)
  )
  write.csv(Statistiques,paste0("U:/AU33/Statistiques/Panorama_", as.character(taille) , ".csv"))
}

#### *********************************************
#### end ####

#### *********************************************
#### Différents Plots Global et Rural/Urbain -----

#############Visualisation Application

for (tcar in liste_taille_carreaux){
  #Répertoire de la carto pour l'année en question
  Repertoire_tcar <- paste0(Path_AU33, "/Plot/", as.character(tcar))
  
  # Vérifie si le répetoire existe
  if (file.exists(Repertoire_tcar)){
    #On calcul le datadash
    taille <- tcar
    source("R/05_data_dashboard_region.R")
    Palette <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
    
    #On réalise les graphiques
    #Segregation Totale au fil des années
    datadash_test <- bind_rows(datadash[["44"]][["indices_segreg_theil"]])
    ggplot(datadash_test) +
      geom_line(aes(x = annee,
                    y = segTot,
                    group = 1,
                    colour = "Grand Est"),
                size=2) +
      scale_color_manual(name = "Indice de ségrégation", values = c("Grand Est" = Palette[1])) +
      labs(x = "Année", y = "Ségrégation Totale")
    datadash_test <- setNames(datadash_test,
                              c("Annee",
                                "Segregation_totale"))
    
    write.csv(datadash_test,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/segregation_totale.csv"))
    
    ggsave(paste0("Segregation_totale_", as.character(taille), ".png"), 
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"))
    
    
    #Composante de chaque groupe
    datadash_test <- data.frame(datadash[["44"]][["data_contribGroupes_theil"]])
    ggplot(datadash_test) +
      geom_line(aes(x = g1.annee,y = g1.indice_binaire,group = 1,colour="« modestes »"),
                size = 2) +
      geom_line(aes(x = g2.annee,y = g2.indice_binaire,group = 1,colour="« moyens-modestes »"),
                size = 2) +
      geom_line(aes(x = g3.annee,y = g3.indice_binaire,group = 1,colour="« moyens »"),
                size = 2) +
      geom_line(aes(x = g4.annee,y = g4.indice_binaire,group = 1,colour="« moyens-aisés »"),
                size = 2) +
      geom_line(aes(x = g5.annee,y = g5.indice_binaire,group = 1,colour="« aisés »"),
                size = 2) +
      scale_color_manual(name = "Groupe Social", 
                         breaks = c("« modestes »",
                                    "« moyens-modestes »",
                                    "« moyens »",
                                    "« moyens-aisés »",
                                    "« aisés »"),
                         values = c("« modestes »" = Palette[1], "« moyens-modestes »" = Palette[2], "« moyens »" = Palette[3], "« moyens-aisés »" = Palette[4], "« aisés »" = Palette[5]))+
      labs(x = "Année", y = "Composante de ségrégation")
    
    datadash_test <-datadash_test %>% 
      select(c(g1.annee, 
               g1.indice_binaire,
               g2.indice_binaire,
               g3.indice_binaire,
               g4.indice_binaire,
               g5.indice_binaire))
    datadash_test <- setNames(datadash_test,
                                       c("Annee",
                                       "Composante_modeste",
                                       "Composante_moyens-modestes",
                                       "Composante_moyens",
                                       "Composante_moyens-aises",
                                       "Composante_aises"))
    write.csv(datadash_test,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/composante_groupe.csv"))
    
    ggsave(paste0("Composante_binaire_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
           height = 10,
           width = 10)
    
    
    #Contribution de chaque groupe
    datadash_test <- data.frame(datadash[["44"]][["data_contribGroupes_theil"]])
    ggplot(datadash_test) +
      geom_line(aes(x = g1.annee,y = g1.contrib_binaire *100 / g1.segTot,group  = 1,colour="« modestes »"),
                size = 2) +
      geom_line(aes(x = g2.annee,y = g2.contrib_binaire *100/ g2.segTot,group = 1,colour="« moyens-modestes »"),
                size = 2) +
      geom_line(aes(x = g3.annee,y = g3.contrib_binaire *100/ g3.segTot,group = 1,colour="« moyens »"),
                size = 2) +
      geom_line(aes(x = g4.annee,y = g4.contrib_binaire *100/ g4.segTot,group = 1,colour="« moyens-aisés »"),
                size = 2) +
      geom_line(aes(x = g5.annee,y = g5.contrib_binaire *100/ g5.segTot,group = 1,colour="« aisés »"),
                size = 2) +
      scale_color_manual(name = "Groupe Social", 
                         breaks = c("« modestes »",
                                    "« moyens-modestes »",
                                    "« moyens »",
                                    "« moyens-aisés »",
                                    "« aisés »"),
                         values = c("« modestes »" = Palette[1], "« moyens-modestes »" = Palette[2], "« moyens »" = Palette[3], "« moyens-aisés »" = Palette[4], "« aisés »" = Palette[5]))+
      labs(x = "Année", y = "% de contribution à la ségrégation totale")
    
    datadash_test <-datadash_test %>% 
      select(c(g1.annee, 
               g1.segTot,
               g1.contrib_binaire,
               g2.contrib_binaire,
               g3.contrib_binaire,
               g4.contrib_binaire,
               g5.contrib_binaire))
    datadash_test <- setNames(datadash_test,
                              c("Annee",
                                "Segregation_totale",
                                "Contribution_modeste",
                                "Contribution_moyens-modestes",
                                "Contribution_moyens",
                                "Contribution_moyens-aises",
                                "Contribution_aises"))
    
    write.csv(datadash_test,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/contribution_groupe.csv"))
    
    ggsave(paste0("Contribution_composante_binaire_", as.character(taille), ".png"),
          path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
          height = 10,
          width = 10)
    
    #Plot en base 100
    df_append <- datadash[["44"]][["indices_segreg_theil"]] %>% setNames(c("annee",paste0("Taille_",as.character(tcar))))
    df_append[[2]] <- df_append[[2]] *100 / df_append[[2]][1]
    datadash_base_100 <- left_join(datadash_base_100, df_append)
    
    ##############Rural/Urbain
    Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
    #Distribution des revenus dans le rural
    datadash_revenu_rural <- datadash[["44"]][["data_distriQP"]] %>% rename(part_rurale = part)
    datadash_revenu_urbain <- datadash[["44"]][["data_distri_hors_QP"]] %>% rename(part_urbaine = part)
    
    
    # Define custom colors for the groups
    group_colors <- c("g1" = Palette[1], "g2" = Palette[3], "g3" = Palette[5], "g4" = Palette[7], "g5" = Palette[9])
    # Plotting side-by-side bar charts with custom colors
    ggplot(datadash_revenu_rural, aes(x = annee, y = part_rurale, fill = groupe)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Annee", y = "% de population rurale dans les groupes sociaux", fill = "Groupes sociaux") +
      ggtitle("Distribution des groupes sociaux des populations rurales en 2004 et en 2019") +
      scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                          "« moyens »", "« moyens-aisés »", "«aisés»")) +
      theme_minimal()+
      scale_y_continuous(breaks = seq(0, 100, by = 20))
    
    datadash_revenu_rural <- setNames(datadash_revenu_rural,
                              c("Annee",
                                "Groupe_social",
                                "Pourcentage"))
    
    write.csv(datadash_revenu_rural,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/histo_rural.csv"))


    ggsave(paste0("Histogramme_revenu_rural_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"))
    
    ggplot(datadash_revenu_urbain, aes(x = annee, y = part_urbaine, fill = groupe)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Annee", y = "% de population urbaine dans les groupes sociaux", fill = "Groupes sociaux") +
      ggtitle("Distribution des groupes sociaux des populations urbaines en 2004 et en 2019") +
      scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                          "« moyens »", "« moyens-aisés »", "«aisés»"))+
      theme_minimal()+
      scale_y_continuous(breaks = seq(0, 100, by = 20))
    
    datadash_revenu_urbain <- setNames(datadash_revenu_urbain,
                                      c("Annee",
                                        "Groupe_social",
                                        "Pourcentage"))
    
    write.csv(datadash_revenu_urbain,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/histo_urbain.csv"))
    
    ggsave(paste0("Histogramme_revenu_urbain_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"))
    
    
    #Evolution de la part de la population vivant dans le monde rural
    datadash_part_rural <- datadash[["44"]][["data_evolPartQP"]]
    ggplot(datadash_part_rural) +
      geom_line(aes(x = annee,
                    y = round(teta_QP,4) *100,
                    group = 1,
                    colour = "Grand Est"),
                size=2) +
      ylim(25, 50) +
      scale_color_manual(name = "% rural", values = c("Grand Est" = Palette[1])) +
      labs(x = "Année", y = "% de population", 
           title = "Part de la population vivant dans les communes rurales du Grand Est")
    
    datadash_part_rural <- setNames(datadash_part_rural,
                                    c("Annee",
                                      "Part_rural"))
    
    write.csv(datadash_part_rural,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/part_rural.csv"))
    
    ggsave(paste0("Evolution_part_population_rural_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"))
    
    #Evolution inter/intra
    df <- datadash[["44"]][["tabContrib_theil"]]
    # Create a new dataframe for stacking
    stacked_df <- data.frame(
      annee = rep(df$annee, 3),
      Composition = rep(c("Contribution Rurale", "Contribution Urbaine", "Ségrégation inter"), each = nrow(df)),
      value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
    )
    
    # Create the stacked bar chart using ggplot
    ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
      geom_col() +
      labs(x = "Annee", y = "Ségrégation totale") +
      scale_fill_manual(values = c("Contribution Rurale" = Palette[1], 
                                   "Ségrégation inter" = Palette[3],
                                   "Contribution Urbaine" = Palette[9]),
                        breaks = c("Contribution Rurale",
                                   "Ségrégation inter",
                                   "Contribution Urbaine"),
                        labels = c("Contribution Rurale","Ségrégation inter", "Contribution Urbaine")) +
      # geom_text(aes(label = value), vjust = -0.5, size = 4, color = "white") +
      theme_minimal()
    
    df <-df %>% 
      select(c(annee, 
               contrib_QP,
               contrib_horsQP,
               indice_inter))
    df <- setNames(df,
                      c("Annee",
                        "Contribution_rurale",
                        "Contribution_urbaine",
                        "Indice_inter"))
    
    write.csv(df,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/evolution_inter_intra.csv"))
    
    ggsave(paste0("Evolution_inter_intra_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
           height = 10,
           width = 10)
    
    
    #Evolution inter/intra en %
    df <- datadash[["44"]][["tabContrib_theil"]]
    df$indice_inter <- df$indice_inter *100 / df$segTot
    df$contrib_QP<- df$contrib_QP *100 / df$segTot
    df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
    df$segTot <- df$segTot / df$segTot
    # Create a new dataframe for stacking
    stacked_df <- data.frame(
      annee = rep(df$annee, 3),
      Composition = rep(c("Contribution Rurale", "Contribution Urbaine", "Ségrégation inter"), each = nrow(df)),
      value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
    )
    
    # Create the stacked bar chart using ggplot
    ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
      geom_col() +
      labs(x = "Annee", y = " % Ségrégation Totale") +
      scale_fill_manual(values = c("Contribution Rurale" = Palette[1],
                                   "Ségrégation inter" = Palette[3],
                                   "Contribution Urbaine" = Palette[9]),
                        breaks = c("Contribution Rurale",
                                   "Ségrégation inter",
                                   "Contribution Urbaine"),
                        labels = c("Contribution Rurale","Ségrégation inter", "Contribution Urbaine")) +
      # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
      theme_minimal()
    
    df <-df %>% 
      select(c(annee, 
               contrib_QP,
               contrib_horsQP,
               indice_inter))
    df <- setNames(df,
                   c("Annee",
                     "Contribution_rurale",
                     "Contribution_urbaine",
                     "Indice_inter"))
    
    write.csv(df,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/evolution_inter_intra_pourcent.csv"))
    
    ggsave(paste0("Evolution_inter_%_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
           height = 10,
           width = 10)
    
    #Evolution inter/intra en base 100
    df <- datadash[["44"]][["tabContrib_theil"]]
    ggplot(df) +
      geom_line(aes(x =   annee,
                    y =   segTot *100 / segTot[1] ,
                    group = 1,
                    color ="Ségrégation totale"),
                size=1.5) +
      geom_line(aes(x =   annee,
                    y =   indice_inter *100 / indice_inter[1] ,
                    group = 1,
                    color ="Ségrégation inter"),
                size=1.5) +
      geom_line(aes(x =   annee,
                    y =   contrib_QP * 100 / contrib_QP[1] ,
                    group = 1,
                    color ="Contribution Rurale"),
                size=1.5) +
      geom_line(aes(x =   annee,
                    y =   contrib_horsQP * 100 / contrib_horsQP[1] ,
                    group = 1,
                    color ="Contribution Urbaine"),
                size=1.5) +
      scale_color_manual(name = "Composition en base 100",
                         breaks = c("Contribution Rurale",
                                    "Ségrégation inter",
                                    "Ségrégation totale",
                                    "Contribution Urbaine"),
                         values = c("Contribution Rurale" = Palette[1],
                                    "Ségrégation inter" = Palette[3],
                                    "Ségrégation totale" = Palette[6],
                                    "Contribution Urbaine" = Palette[9])) +
      labs(x = "Année", y = "Contribution inter/intra en base 100 ")
    
    df <-df %>% 
      select(c(annee, 
               segTot,
               contrib_QP,
               contrib_horsQP,
               indice_inter))
    df$segTot <- df$segTot *100 /df$segTot[1]
    df$contrib_QP <- df$contrib_QP *100 /df$contrib_QP[1]
    df$contrib_horsQP <- df$contrib_horsQP *100 /df$contrib_horsQP[1]
    df$indice_inter <- df$indice_inter *100 /df$indice_inter[1]
    
    df <- setNames(df,
                   c("Annee",
                     "Segregation_totale",
                     "Contribution_rurale",
                     "Contribution_urbaine",
                     "Indice_inter"))
    
    write.csv(df,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/base_100.csv"))
    
    ggsave(paste0("Evolution_inter_intra_base_100_taille_", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
           height = 10,
           width = 10)
    
    #Evolution indice rural/urbain
    df <- datadash[["44"]][["tabContrib_theil"]]
    ggplot(df) +
      geom_line(aes(x =   annee,
                    y =   indice_QP ,
                    group = 1,
                    color ="Indice rural"),
                size=1.5) +
      geom_line(aes(x =   annee,
                    y =   indice_horsQP ,
                    group = 1,
                    color ="Indice urbain"),
                size=1.5) +
      scale_color_manual(name = "Indice de ségrégation",
                         breaks = c("Indice rural",
                                    "Indice urbain"),
                         values = c("Indice rural" = Palette[1],
                                    "Indice urbain" = Palette[9])) +
      labs(x = "Année", y = "Indice de ségrégation ")
    
    ggsave(paste0("Evolution_indice_rural_urbain", as.character(taille), ".png"),
           path = paste0("U:/AU33/Plot/", as.character(taille), "/"),
           height = 10,
           width = 10)
    
    df <-df %>% 
      select(c(annee, 
               indice_QP,
               indice_horsQP))
    df <- setNames(df,
                   c("Annee",
                     "Indice_rural",
                     "Indice_urbain"))
    write.csv(df,
              paste0("U:/AU33/output_csv/",as.character(taille) ,"/rural/indice_rural_urbain.csv"))
    
    
  } else {
    # Crée le répertoire
    dir.create(file.path(paste0(Path_AU33, "/Plot/"),  as.character(tcar)))
  }
}


#### *********************************************
#### end ####


#### *********************************************
#### Comparaison tailles carreaux base 100   -----

# Comparaison toutes les tailles de carreaux en base 100

# Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
# 
# ggplot(datadash_base_100) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                 y =   names(datadash_base_100)[2] ,
#                 group = 1,
#                 color =shQuote(as.character(liste_taille_carreaux[1]))),
#             size=1.5) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                        y =   names(datadash_base_100)[3] ,
#                        group = 1,
#                        color =shQuote(as.character(liste_taille_carreaux[2]))),
#             size=1.5) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                        y =   names(datadash_base_100)[4] ,
#                        group = 1,
#                        color =shQuote(as.character(liste_taille_carreaux[3]))),
#             size=1.5) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                        y =   names(datadash_base_100)[5] ,
#                        group = 1,
#                        color =shQuote(as.character(liste_taille_carreaux[4]))),
#             size=1.5) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                        y =   names(datadash_base_100)[6] ,
#                        group = 1,
#                        color =shQuote(as.character(liste_taille_carreaux[5]))),
#             size=1.5) +
#   geom_line(aes_string(x =   names(datadash_base_100)[1],
#                        y =   names(datadash_base_100)[7] ,
#                        group = 1,
#                        color =shQuote(as.character(liste_taille_carreaux[6]))),
#             size=1.5) +
#   scale_color_manual(name = "Ségrégation totale",
#                      breaks = c(as.character(liste_taille_carreaux[1]),
#                                 as.character(liste_taille_carreaux[2]),
#                                 as.character(liste_taille_carreaux[3]),
#                                 as.character(liste_taille_carreaux[4]),
#                                 as.character(liste_taille_carreaux[5]),
#                                 as.character(liste_taille_carreaux[6])),
#                      values = c("2000" = Palette[1],
#                                 "3000" = Palette[2],
#                                 "4000" = Palette[3],
#                                 "5000" = Palette[7],
#                                 "10000" = Palette[8],
#                                 "20000" = Palette[9])) +
#   labs(x = "Année", y = "Ségrégation Totale en base 100 pour une taille de maille donnée",
#        title = "Ségrégation totale en base 100 dans le Grand Est pour différentes tailles de maille")
# ggsave(paste0("Segregation_totale_base_100.png"),
#        path = paste0("U:/AU33/Plot/"))

#### *********************************************
#### end ####


#### *********************************************
#### Evolution revenu médian rural/urbain    -----

#Revenu médian rural/urbain:
df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
                        select(annee) %>%
                        mutate(mediane_rurale= as.numeric(liste_mediane_rurale),
                               mediane_urbaine = as.numeric(liste_mediane_urbain),
                               mediane_region = as.numeric(liste_mediane_region))

ggplot(df) +
  geom_line(aes(x =   annee,
                y =   mediane_rurale ,
                group = 1,
                color ="Ruraux"),
            size=1.5) +
  geom_line(aes(x =   annee,
                y =   mediane_urbaine ,
                group = 1,
                color ="Urbains"),
            size=1.5) +
  geom_line(aes(x =   annee,
                y =   mediane_region ,
                group = 1,
                color ="Tous ensemble"),
            size=1.5) +
  scale_color_manual(name = "Mediane des niveaux de vie des ménages",
                     breaks = c("Ruraux",
                                "Urbains",
                                "Tous ensemble"),
                     values = c("Ruraux" = Palette[1],
                                "Urbains" = Palette[9],
                                "Tous ensemble"= Palette[4])) +
  labs(x = "Année", y = "Médiane des niveaux de vie des ménages "
       # ,
       # title = "Evolution de la médiane des niveaux de vie des ménages ruraux/urbains"
       )

ggsave(paste0("Evolution_mediane_revenus.png"),
       path = paste0("U:/AU33/Plot/"))

#Revenu médian rural/urbain en base 100:
df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
  select(annee) %>%
  mutate(mediane_rurale= as.numeric(liste_mediane_rurale) *100 / liste_mediane_rurale[[1]],
         mediane_urbaine = as.numeric(liste_mediane_urbain) * 100 / liste_mediane_urbain[[1]],
         mediane_region = as.numeric(liste_mediane_region) *100 / liste_mediane_region[[1]])

ggplot(df) +
  geom_line(aes(x =   annee,
                y =   mediane_rurale ,
                group = 1,
                color ="Ruraux"),
            size=1.5) +
  geom_line(aes(x =   annee,
                y =   mediane_urbaine ,
                group = 1,
                color ="Urbains"),
            size=1.5) +
  geom_line(aes(x =   annee,
                y =   mediane_region ,
                group = 1,
                color ="Tous ensemble"),
            size=1.5) +
  scale_color_manual(name = "Mediane des niveaux de vie des ménages",
                     breaks = c("Ruraux",
                                "Urbains",
                                "Tous ensemble"),
                     values = c("Ruraux" = Palette[1],
                                "Urbains" = Palette[9],
                                "Tous ensemble"= Palette[4])) +
  labs(x = "Année", y = "Médiane des niveaux de vie des ménages en base 100 "
       # ,
       # title = "Evolution de la médiane des niveaux de vie des ménages ruraux/urbains en base 100"
       )

ggsave(paste0("Evolution_mediane_revenus_base_100.png"),
       path = paste0("U:/AU33/Plot/"))

#### *********************************************
#### end ####

# 
# #### **************************************************************************
# #### Différents seuils de nombre d'habitants pour la coupe inter/intra    -----
# 
# Liste_seuil <- seq(2000,20000,1000)
Liste_seuil <- seq(8000,8000,2000)
Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')

#Fonction pour les communes gardées
bind_cols_fill <- function(df_list) {

  max_rows <- map_int(df_list, nrow) %>% max()

  map(df_list, function(df) {
    if(nrow(df) == max_rows) return(df)
    first <- names(df)[1] %>% sym()
    df %>% add_row(!!first := rep(NA, max_rows - nrow(df)))
  }) %>% bind_cols()
}

#Contours précis
# com_contours <- st_read("U:/AU33/rural/commune_bdtopo_franceentiere_2022.shp",quiet = T)
#Contours normal
com_contours <- st_read("U:/AU33/coupe_seuil/commune_franceentiere_2019.shp",quiet = T) %>%
  rename_at('code', ~'codgeo')
#On filtre le Grand Est
com_contours <- com_contours %>% filter(reg %in% c(code_region)) %>% st_transform(proj_lambert93)
#Source : P:\PSAR_AT\ZONAGES\BasesCommunales\an_2022
com_region <- read.csv("U:/AU33/coupe_seuil/base_commune.csv", sep = ";") %>%
  select (DEPCOM, LIBGEO, REG, DEP, Pop_tot_2019) %>%
  filter(REG == as.integer(code_region)) %>%
  rename_at('DEPCOM', ~'CODGEO')

#Evolution inter/intra en %
df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>%
  mutate(Seuil = c(seuil_nb_hab))
df$indice_inter <- df$indice_inter *100 / df$segTot
df$contrib_QP<- df$contrib_QP *100 / df$segTot
df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
df$segTot <- df$segTot / df$segTot
# Create a new dataframe for stacking
df_plot_seuil <- data.frame(
  Seuil = rep(df$Seuil, 3),
  composition = rep(c("contribution_<_seuil", "contribution_>_seuil", "indice_inter"), each = nrow(df)),
  value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
) %>% tail(0)

df_liste_com_au_dessus <- data.frame(test=c(1))

for (seuil_nb_hab in Liste_seuil){
  source("R/12_seuil_nb_habitant.R")

  taille <- 4000
  source("R/05_data_dashboard_region.R")

  #Evolution inter/intra en %
  df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>%
    mutate(Seuil = c(seuil_nb_hab))
  df$indice_inter <- df$indice_inter *100 / df$segTot
  df$contrib_QP<- df$contrib_QP *100 / df$segTot
  df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
  df$segTot <- df$segTot / df$segTot
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    Seuil = rep(df$Seuil, 3),
    composition = rep(c("contribution_<_seuil", "contribution_>_seuil", "indice_inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  df_plot_seuil <- rbind(df_plot_seuil, stacked_df)

  #CSV des communes gardées

  df_liste_com_au_dessus<-bind_cols_fill(list(tibble(df_liste_com_au_dessus), tibble(Liste_com_au_dessus_seuil)))

  #Revenu médian sous/au dessus du seuil:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
                          select(annee) %>%
                          mutate(mediane_sous_seuil= as.numeric(liste_mediane_rurale),
                                 mediane_sur_seuil = as.numeric(liste_mediane_urbain),
                                 mediane_region = as.numeric(liste_mediane_region))

  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_sous_seuil ,
                  group = 1,
                  color ="Territoire sous seuil"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_sur_seuil ,
                  group = 1,
                  color ="Territoire au dessus du seuil"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des revenus \n des ménages",
                       breaks = c("Territoire sous seuil",
                                  "Territoire au dessus du seuil",
                                  "Tous ensemble"),
                       values = c("Territoire sous seuil" = Palette[9],
                                  "Territoire au dessus du seuil" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des revenus des ménages ",
         title = paste0("Evolution de la médiane des revenus des ménages \n des territoires au dessus /sous le seuil ", seuil_nb_hab))

  ggsave(paste0("Evolution_mediane_revenus_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/", as.character(seuil_nb_hab), "/"))

  #Revenu médian rural/urbain en base 100:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
    select(annee) %>%
    mutate(mediane_sous_seuil= as.numeric(liste_mediane_rurale) *100 / liste_mediane_rurale[[1]],
           mediane_sur_seuil = as.numeric(liste_mediane_urbain) * 100 / liste_mediane_urbain[[1]],
           mediane_region = as.numeric(liste_mediane_region) *100 / liste_mediane_region[[1]])

  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_sous_seuil ,
                  group = 1,
                  color ="Territoire sous seuil"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_sur_seuil ,
                  group = 1,
                  color ="Territoire au dessus du seuil"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des revenus \n des ménages",
                       breaks = c("Territoire sous seuil",
                                  "Territoire au dessus du seuil",
                                  "Tous ensemble"),
                       values = c("Territoire sous seuil" = Palette[9],
                                  "Territoire au dessus du seuil" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des revenus des ménages en base 100 ",
         title = paste0("Evolution de la médiane des revenus en base 100 des ménages \n des territoires au dessus /sous le seuil ", seuil_nb_hab))

  ggsave(paste0("Evolution_mediane_revenus_base_100_seuil_", as.character(seuil_nb_hab), ".png"),
  path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/", as.character(seuil_nb_hab), "/"))

  ##############Rural/Urbain
  Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
  #Distribution des revenus dans le rural
  datadash_revenu_rural <- datadash[["44"]][["data_distriQP"]] %>% rename(part_rurale = part)
  datadash_revenu_urbain <- datadash[["44"]][["data_distri_hors_QP"]] %>% rename(part_urbaine = part)

  # Define custom colors for the groups
  group_colors <- c("g1" = Palette[1], "g2" = Palette[3], "g3" = Palette[5], "g4" = Palette[7], "g5" = Palette[9])
  # Plotting side-by-side bar charts with custom colors
  ggplot(datadash_revenu_rural, aes(x = annee, y = part_rurale, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des teritoires en dessous du seuil", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires en dessous du seuil de ", as.character(seuil_nb_hab), " habitants")) +
    scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,50)+
    theme_minimal()


  ggsave(paste0("Histogramme_revenu_sous_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/", as.character(seuil_nb_hab), "/"))

  ggplot(datadash_revenu_urbain, aes(x = annee, y = part_urbaine, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des teritoires au dessus du seuil", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires au dessus du seuil de ", as.character(seuil_nb_hab), " habitants")) +
    scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,50)+
    theme_minimal()


  ggsave(paste0("Histogramme_revenu_au_dessus_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/", as.character(seuil_nb_hab), "/"))


  #Evolution de la part de la population vivant dans le monde en dessous du seuil
  datadash_part_rural <- datadash[["44"]][["data_evolPartQP"]]
  ggplot(datadash_part_rural) +
    geom_line(aes(x = annee,
                  y = round(teta_QP,4) *100,
                  group = 1,
                  colour = "Grand Est"),
              size=2) +
    ylim(30, 100) +
    scale_color_manual(name = paste0("% de population dans les communes \n en dessous du seuil"),
                       values = c("Grand Est" = Palette[1])) +
    labs(x = "Année", y = "% de population",
         title = paste0("Part de la population vivant dans les communes\n en dessous du seuil de ", as.character(seuil_nb_hab),
                        " habitants"))

  ggsave(paste0("Evolution_part_population_sous_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/", as.character(seuil_nb_hab), "/"))
}

# Create the stacked bar chart using ggplot
ggplot(df_plot_seuil, aes(x = Seuil, y = value, fill = composition)) +
  geom_col() +
  labs(x = "Seuil", y = " % segTot", title = "Décomposition en part inter/intra de la ségrégation totale") +
  scale_fill_manual(values = c("contribution_<_seuil" = Palette[1], "contribution_>_seuil" = Palette[3], "indice_inter" = Palette[9]),
                    labels = c("contribution_<_seuil", "contribution_>_seuil", "indice_inter")) +
  # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
  theme_minimal()

ggsave(paste0("Comparaison_seuil.png"),
       path = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/"))

#Communes gardées pour chaque seuil
df_liste_com_au_dessus <- df_liste_com_au_dessus %>% select(-test) %>% as.data.frame()
colnames(df_liste_com_au_dessus) <- Liste_seuil
write.csv(df_liste_com_au_dessus , file = paste0("U:/AU33/Plot/", as.character(taille), "/seuil/liste_communes.csv")
          , row.names=TRUE)

#### *********************************************
#### end ####


#### **************************************************************************
#### Différentes parts de frontalier pour la coupe inter/intra            -----

Liste_seuil <- seq(50,50,5)
Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')

#Fonction pour les communes gardées
bind_cols_fill <- function(df_list) {
  
  max_rows <- map_int(df_list, nrow) %>% max()
  
  map(df_list, function(df) {
    if(nrow(df) == max_rows) return(df)
    first <- names(df)[1] %>% sym()
    df %>% add_row(!!first := rep(NA, max_rows - nrow(df)))
  }) %>% bind_cols()
}

#Contours précis
# com_contours <- st_read("U:/AU33/rural/commune_bdtopo_franceentiere_2022.shp",quiet = T)
#Contours normal
com_contours <- st_read("U:/AU33/coupe_seuil/commune_franceentiere_2019.shp",quiet = T) %>% 
  rename_at('code', ~'codgeo')
#On filtre le Grand Est
com_contours <- com_contours %>% filter(reg %in% c(code_region)) %>% st_transform(proj_lambert93)
#Source : P:\PSAR_AT\ZONAGES\BasesCommunales\an_2022
com_region <- read.csv("U:/AU33/coupe_seuil/base_commune.csv", sep = ";") %>% 
  select (DEPCOM, LIBGEO, REG, DEP, Pop_tot_2019) %>% 
  filter(REG == as.integer(code_region)) %>% 
  rename_at('DEPCOM', ~'CODGEO')

#Evolution inter/intra en %
df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>% 
  mutate(Seuil = c(seuil_nb_hab))
df$indice_inter <- df$indice_inter *100 / df$segTot
df$contrib_QP<- df$contrib_QP *100 / df$segTot
df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
df$segTot <- df$segTot / df$segTot
# Create a new dataframe for stacking
df_plot_seuil <- data.frame(
  Seuil = rep(df$Seuil, 3),
  composition = rep(c("contribution_<_seuil", "contribution_>_seuil", "indice_inter"), each = nrow(df)),
  value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
) %>% tail(0)

df_liste_com_frontalier <- data.frame(test=c(1))

for (seuil_nb_hab in Liste_seuil/100){
  source("R/13_frontalier.R")
  
  taille <- 4000
  source("R/05_data_dashboard_region.R")

  #Evolution inter/intra en %
  df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>%
    mutate(Seuil = c(seuil_nb_hab))
  df$indice_inter <- df$indice_inter *100 / df$segTot
  df$contrib_QP<- df$contrib_QP *100 / df$segTot
  df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
  df$segTot <- df$segTot / df$segTot
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    Seuil = rep(df$Seuil, 3),
    composition = rep(c("contribution_frontaliere", "contribution_non_frontaliere", "indice_inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  df_plot_seuil <- rbind(df_plot_seuil, stacked_df)

  #CSV des communes gardées

  df_liste_com_frontalier<-bind_cols_fill(list(tibble(df_liste_com_frontalier), tibble(Liste_com_frontalieres)))
  
  #Revenu médian sous/au dessus du seuil:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
    select(annee) %>%
    mutate(mediane_frontalier= as.numeric(liste_mediane_frontalier),
           mediane_non_frontalier = as.numeric(liste_mediane_non_frontalier),
           mediane_region = as.numeric(liste_mediane_region))
  
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_frontalier ,
                  group = 1,
                  color ="Territoire Frontalier"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_non_frontalier ,
                  group = 1,
                  color ="Territoire non Frontalier"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des niveaux de vie \n des ménages",
                       breaks = c("Territoire Frontalier",
                                  "Territoire non Frontalier",
                                  "Tous ensemble"),
                       values = c("Territoire Frontalier" = Palette[9],
                                  "Territoire non Frontalier" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des niveaux de vie des ménages ")
  
  df <- setNames(df,
                    c("Annee",
                    "Mediane_frontalier",
                    "Mediane_non_frontalier",
                    "Mediane_region"))
  write.csv(df,
            paste0("U:/AU33/output_csv/",as.character(taille) ,"/frontalier/mediane.csv"))
  
  ggsave(paste0("Evolution_mediane_revenus_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  #Revenu médian rural/urbain en base 100:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
    select(annee) %>%
    mutate(mediane_frontalier= as.numeric(liste_mediane_frontalier) *100 / liste_mediane_frontalier[[1]],
           mediane_non_frontalier = as.numeric(liste_mediane_non_frontalier) * 100 / liste_mediane_non_frontalier[[1]],
           mediane_region = as.numeric(liste_mediane_region) *100 / liste_mediane_region[[1]])
  
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_frontalier ,
                  group = 1,
                  color ="Territoire Frontalier"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_non_frontalier ,
                  group = 1,
                  color ="Territoire non Frontalier"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des niveaux de vie \n des ménages",
                       breaks = c("Territoire Frontalier",
                                  "Territoire non Frontalier",
                                  "Tous ensemble"),
                       values = c("Territoire Frontalier" = Palette[9],
                                  "Territoire non Frontalier" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des niveaux de vie des ménages en base 100 ")
  
  df <- setNames(df,
                 c("Annee",
                   "Mediane_frontalier_base_100",
                   "Mediane_non_frontalier_base_100",
                   "Mediane_region_base_100"))
  write.csv(df,
            paste0("U:/AU33/output_csv/",as.character(taille) ,"/frontalier/mediane_base_100.csv"))
  
  ggsave(paste0("Evolution_mediane_revenus_base_100_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  ##############Rural/Urbain
  Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
  #Distribution des revenus dans le rural
  datadash_revenu_rural <- datadash[["44"]][["data_distriQP"]] %>% rename(part_rurale = part)
  datadash_revenu_urbain <- datadash[["44"]][["data_distri_hors_QP"]] %>% rename(part_urbaine = part)

  # Define custom colors for the groups
  group_colors <- c("g1" = Palette[1], "g2" = Palette[3], "g3" = Palette[5], "g4" = Palette[7], "g5" = Palette[9])
  # Plotting side-by-side bar charts with custom colors
  ggplot(datadash_revenu_rural, aes(x = annee, y = part_rurale, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des territoires frontaliers", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires frontaliers (part de frontaliers >= ", as.character(seuil_nb_hab), ")")) +
    scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,50)+
    theme_minimal()
  
  datadash_revenu_rural <- setNames(datadash_revenu_rural,
                 c("Annee",
                   "Groupe_social",
                   "Part"))
  write.csv(datadash_revenu_rural,
            paste0("U:/AU33/output_csv/",as.character(taille) ,"/frontalier/histo_frontalier.csv"))

  ggsave(paste0("Histogramme_revenu_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))

  ggplot(datadash_revenu_urbain, aes(x = annee, y = part_urbaine, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des territoires non frontaliers", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires non frontaliers (part de frontaliers < ", as.character(seuil_nb_hab), ")")) +
    scale_fill_manual(values = group_colors, labels = c("«modestes»", "« moyens-modestes »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,50)+
    theme_minimal()
  
  datadash_revenu_urbain <- setNames(datadash_revenu_urbain,
                                    c("Annee",
                                      "Groupe_social",
                                      "Part"))
  write.csv(datadash_revenu_urbain,
            paste0("U:/AU33/output_csv/",as.character(taille) ,"/frontalier/histo_non_frontalier.csv"))

  ggsave(paste0("Histogramme_revenu_non_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))


  #Evolution de la part de la population vivant dans le monde frontalier
  datadash_part_rural <- datadash[["44"]][["data_evolPartQP"]]
  ggplot(datadash_part_rural) +
    geom_line(aes(x = annee,
                  y = round(teta_QP,4) *100,
                  group = 1,
                  colour = "Grand Est"),
              size=2) +
    ylim(0, 100) +
    scale_color_manual(name = paste0("% de population dans les communes \n frontalières"),
                       values = c("Grand Est" = Palette[1])) +
    labs(x = "Année", y = "% de population",
         title = paste0("Part de la population vivant dans les communes\n frontalières (part de frontaliers >= ", as.character(seuil_nb_hab),
                        ")"))

  ggsave(paste0("Evolution_part_population_frontalieres.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  
  ###Evolution temporelle
  #Evolution inter/intra
  df <- datadash[["44"]][["tabContrib_theil"]]
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    annee = rep(df$annee, 3),
    Composition = rep(c("Contribution Frontalière", "Contribution non Frontalière", "Ségrégation inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  
  # Create the stacked bar chart using ggplot
  ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
    geom_col() +
    labs(x = "Annee", y = "Ségrégation totale") +
    scale_fill_manual(values = c("Contribution Frontalière" = Palette[1], "Contribution non Frontalière" = Palette[3], "Ségrégation inter" = Palette[9]),
                      labels = c("Contribution Frontalière", "Contribution non Frontalière", "Ségrégation inter")) +
    # geom_text(aes(label = value), vjust = -0.5, size = 4, color = "white") +
    theme_minimal()
  
  ggsave(paste0("Evolution_inter_intra.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  
  #Evolution inter/intra en %
  df <- datadash[["44"]][["tabContrib_theil"]]
  df$indice_inter <- df$indice_inter *100 / df$segTot
  df$contrib_QP<- df$contrib_QP *100 / df$segTot
  df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
  df$segTot <- df$segTot / df$segTot
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    annee = rep(df$annee, 3),
    Composition = rep(c("Contribution Frontalière", "Contribution non Frontalière", "Ségrégation inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  
  # Create the stacked bar chart using ggplot
  ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
    geom_col() +
    labs(x = "Annee", y = " % Ségrégation Totale") +
    scale_fill_manual(values = c("Contribution Frontalière" = Palette[1], "Contribution non Frontalière" = Palette[3], "Ségrégation inter" = Palette[9]),
                      labels = c("Contribution Frontalière", "Contribution non Frontalière", "Ségrégation inter")) +
    # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
    theme_minimal()
  
  ggsave(paste0("Evolution_inter_intra_pourcent.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  #Evolution inter/intra en base 100
  df <- datadash[["44"]][["tabContrib_theil"]]
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   segTot *100 / segTot[1] ,
                  group = 1,
                  color ="Ségrégation totale"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   indice_inter *100 / indice_inter[1] ,
                  group = 1,
                  color ="Ségrégation inter"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   contrib_QP * 100 / contrib_QP[1] ,
                  group = 1,
                  color ="Contribution Frontalière"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   contrib_horsQP * 100 / contrib_horsQP[1] ,
                  group = 1,
                  color ="Contribution non Frontalière"),
              size=1.5) +
    scale_color_manual(name = "Composition en base 100",
                       breaks = c("Ségrégation totale",
                                  "Ségrégation inter",
                                  "Contribution Frontalière",
                                  "Contribution non Frontalière"),
                       values = c("Ségrégation totale" = Palette[1],
                                  "Ségrégation inter" = Palette[3],
                                  "Contribution Frontalière" = Palette[6],
                                  "Contribution non Frontalière" = Palette[9])) +
    labs(x = "Année", y = "Contribution inter/intra en base 100 ")
  
  ggsave(paste0("Evolution_inter_intra_base_100.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
  
  #Evolution indice rural/urbain
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   indice_QP ,
                  group = 1,
                  color ="Indice Frontalier"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   indice_horsQP ,
                  group = 1,
                  color ="Indice non Frontalier"),
              size=1.5) +
    scale_color_manual(name = "Indice de ségrégation",
                       breaks = c("Indice Frontalier",
                                  "Indice non Frontalier"),
                       values = c("Indice Frontalier" = Palette[1],
                                  "Indice non Frontalier" = Palette[9])) +
    labs(x = "Année", y = "Indice de ségrégation ")
  
  ggsave(paste0("Evolution_indice_frontalier_non_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/", as.character(seuil_nb_hab), "/"))
}

# Create the stacked bar chart using ggplot
ggplot(df_plot_seuil, aes(x = Seuil, y = value, fill = composition)) +
  geom_col() +
  labs(x = "Seuil", y = " % ségrégation totale", title = "Décomposition en part inter/intra de la ségrégation totale") +
  scale_fill_manual(values = c("contribution_frontaliere" = Palette[1], "contribution_non_frontaliere" = Palette[3], "indice_inter" = Palette[9]),
                    labels = c("contribution_frontaliere", "contribution_non_frontaliere", "indice_inter")) +
  # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
  theme_minimal()

ggsave(paste0("Comparaison_seuil.png"),
       path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/"))

#Communes gardées pour chaque seuil
df_liste_com_frontalier <- df_liste_com_frontalier %>% select(-test) %>% as.data.frame()
colnames(df_liste_com_frontalier) <- Liste_seuil
write.csv(df_liste_com_frontalier , file = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier/liste_communes.csv")
          , row.names=TRUE)

#### *********************************************
#### end ####

#### **************************************************************************
#### Différentes parts de frontalier suisse pour la coupe inter/intra            -----

Liste_seuil <- seq(5,60,5)
Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')

#Fonction pour les communes gardées
bind_cols_fill <- function(df_list) {
  
  max_rows <- map_int(df_list, nrow) %>% max()
  
  map(df_list, function(df) {
    if(nrow(df) == max_rows) return(df)
    first <- names(df)[1] %>% sym()
    df %>% add_row(!!first := rep(NA, max_rows - nrow(df)))
  }) %>% bind_cols()
}

#Contours précis
# com_contours <- st_read("U:/AU33/rural/commune_bdtopo_franceentiere_2022.shp",quiet = T)
#Contours normal
com_contours <- st_read("U:/AU33/coupe_seuil/commune_franceentiere_2019.shp",quiet = T) %>% 
  rename_at('code', ~'codgeo')
#On filtre le Grand Est
com_contours <- com_contours %>% filter(reg %in% c(code_region)) %>% st_transform(proj_lambert93)
#Source : P:\PSAR_AT\ZONAGES\BasesCommunales\an_2022
com_region <- read.csv("U:/AU33/coupe_seuil/base_commune.csv", sep = ";") %>% 
  select (DEPCOM, LIBGEO, REG, DEP, Pop_tot_2019) %>% 
  filter(REG == as.integer(code_region)) %>% 
  rename_at('DEPCOM', ~'CODGEO')

#Evolution inter/intra en %
df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>% 
  mutate(Seuil = c(seuil_nb_hab))
df$indice_inter <- df$indice_inter *100 / df$segTot
df$contrib_QP<- df$contrib_QP *100 / df$segTot
df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
df$segTot <- df$segTot / df$segTot
# Create a new dataframe for stacking
df_plot_seuil <- data.frame(
  Seuil = rep(df$Seuil, 3),
  composition = rep(c("contribution_<_seuil", "contribution_>_seuil", "indice_inter"), each = nrow(df)),
  value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
) %>% tail(0)

df_liste_com_frontalier <- data.frame(test=c(1))

for (seuil_nb_hab in Liste_seuil/100){
  source("R/14_frontalier_suisse.R")
  
  taille <- 4000
  source("R/05_data_dashboard_region.R")
  
  #Evolution inter/intra en %
  df <- datadash[["44"]][["tabContrib_theil"]] %>% filter(annee == 2019) %>%
    mutate(Seuil = c(seuil_nb_hab))
  df$indice_inter <- df$indice_inter *100 / df$segTot
  df$contrib_QP<- df$contrib_QP *100 / df$segTot
  df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
  df$segTot <- df$segTot / df$segTot
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    Seuil = rep(df$Seuil, 3),
    composition = rep(c("contribution_frontaliere_suisse", "contribution_non_frontaliere_suisse", "indice_inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  df_plot_seuil <- rbind(df_plot_seuil, stacked_df)
  
  #CSV des communes gardées
  
  df_liste_com_frontalier<-bind_cols_fill(list(tibble(df_liste_com_frontalier), tibble(Liste_com_frontalieres)))
  
  #Revenu médian sous/au dessus du seuil:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
    select(annee) %>%
    mutate(mediane_frontalier= as.numeric(liste_mediane_frontalier),
           mediane_non_frontalier = as.numeric(liste_mediane_non_frontalier),
           mediane_region = as.numeric(liste_mediane_region))
  
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_frontalier ,
                  group = 1,
                  color ="Territoire Frontalier Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_non_frontalier ,
                  group = 1,
                  color ="Territoire non Frontalier Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des revenus \n des ménages",
                       breaks = c("Territoire Frontalier Suisse",
                                  "Territoire non Frontalier Suisse",
                                  "Tous ensemble"),
                       values = c("Territoire Frontalier Suisse" = Palette[9],
                                  "Territoire non Frontalier Suisse" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des revenus des ménages ",
         title = paste0("Evolution de la médiane des revenus des ménages \n des territoires frontaliers/non frontaliers Suisse \n selon la part de frontalier ", seuil_nb_hab))
  
  ggsave(paste0("Evolution_mediane_revenus_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  #Revenu médian rural/urbain en base 100:
  df <- bind_rows(datadash[["44"]][["indices_segreg_theil"]]) %>%
    select(annee) %>%
    mutate(mediane_frontalier= as.numeric(liste_mediane_frontalier) *100 / liste_mediane_frontalier[[1]],
           mediane_non_frontalier = as.numeric(liste_mediane_non_frontalier) * 100 / liste_mediane_non_frontalier[[1]],
           mediane_region = as.numeric(liste_mediane_region) *100 / liste_mediane_region[[1]])
  
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   mediane_frontalier ,
                  group = 1,
                  color ="Territoire Frontalier Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_non_frontalier ,
                  group = 1,
                  color ="Territoire non Frontalier Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   mediane_region ,
                  group = 1,
                  color ="Tous ensemble"),
              size=1.5) +
    scale_color_manual(name = "Mediane des revenus \n des ménages",
                       breaks = c("Territoire Frontalier Suisse",
                                  "Territoire non Frontalier Suisse",
                                  "Tous ensemble"),
                       values = c("Territoire Frontalier Suisse" = Palette[9],
                                  "Territoire non Frontalier Suisse" = Palette[1],
                                  "Tous ensemble"= Palette[4])) +
    labs(x = "Année", y = "Médiane des revenus des ménages en base 100 ",
         title = paste0("Evolution de la médiane des revenus en base 100 des ménages \n des territoires frontaliers/non frontaliers Suisse \n selon la part de frontalier ", seuil_nb_hab))
  
  ggsave(paste0("Evolution_mediane_revenus_base_100_seuil_", as.character(seuil_nb_hab), ".png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  ##############Rural/Urbain
  Palette <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd')
  #Distribution des revenus dans le rural
  datadash_revenu_rural <- datadash[["44"]][["data_distriQP"]] %>% rename(part_rurale = part)
  datadash_revenu_urbain <- datadash[["44"]][["data_distri_hors_QP"]] %>% rename(part_urbaine = part)
  
  # Define custom colors for the groups
  group_colors <- c("g1" = Palette[9], "g2" = Palette[7], "g3" = Palette[5], "g4" = Palette[3], "g5" = Palette[1])
  # Plotting side-by-side bar charts with custom colors
  ggplot(datadash_revenu_rural, aes(x = annee, y = part_rurale, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des territoires frontaliers Suisse", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires frontaliers Suisse (part de frontaliers >= ", as.character(seuil_nb_hab), ")")) +
    scale_fill_manual(values = group_colors, labels = c("«pauvres»", "« moyens-pauvres »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,60)+
    theme_minimal()
  
  
  ggsave(paste0("Histogramme_revenu_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  ggplot(datadash_revenu_urbain, aes(x = annee, y = part_urbaine, fill = groupe)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept=20)+
    labs(x = "Annee", y = "% de population des territoires non frontaliers Suisse", fill = "Groupes sociaux") +
    ggtitle(paste0("Distribution en 2004 et en 2019 des groupes sociaux \ndes territoires non frontaliers Suisse (part de frontaliers < ", as.character(seuil_nb_hab), ")")) +
    scale_fill_manual(values = group_colors, labels = c("«pauvres»", "« moyens-pauvres »",
                                                        "« moyens »", "« moyens-aisés »", "«aisés»")) +
    ylim(0,60)+
    theme_minimal()
  
  
  ggsave(paste0("Histogramme_revenu_non_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  
  #Evolution de la part de la population vivant dans le monde frontalier Suisse
  datadash_part_rural <- datadash[["44"]][["data_evolPartQP"]]
  ggplot(datadash_part_rural) +
    geom_line(aes(x = annee,
                  y = round(teta_QP,4) *100,
                  group = 1,
                  colour = "Grand Est"),
              size=2) +
    ylim(0, 30) +
    scale_color_manual(name = paste0("% de population dans les communes \n frontalières Suisse"),
                       values = c("Grand Est" = Palette[1])) +
    labs(x = "Année", y = "% de population",
         title = paste0("Part de la population vivant dans les communes\n frontalières Suisse (part de frontaliers >= ", as.character(seuil_nb_hab),
                        ")"))
  
  ggsave(paste0("Evolution_part_population_frontalieres.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  
  ###Evolution temporelle
  #Evolution inter/intra
  df <- datadash[["44"]][["tabContrib_theil"]]
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    annee = rep(df$annee, 3),
    Composition = rep(c("Contribution Frontalière Suisse", "Contribution non Frontalière Suisse", "Ségrégation inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  
  # Create the stacked bar chart using ggplot
  ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
    geom_col() +
    labs(x = "Annee", y = "Ségrégation totale") +
    scale_fill_manual(values = c("Contribution Frontalière Suisse" = Palette[1], "Contribution non Frontalière Suisse" = Palette[3], "Ségrégation inter" = Palette[9]),
                      labels = c("Contribution Frontalière Suisse", "Contribution non Frontalière Suisse", "Ségrégation inter")) +
    # geom_text(aes(label = value), vjust = -0.5, size = 4, color = "white") +
    theme_minimal()
  
  ggsave(paste0("Evolution_inter_intra.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  
  #Evolution inter/intra en %
  df <- datadash[["44"]][["tabContrib_theil"]]
  df$indice_inter <- df$indice_inter *100 / df$segTot
  df$contrib_QP<- df$contrib_QP *100 / df$segTot
  df$contrib_horsQP <- df$contrib_horsQP *100 / df$segTot
  df$segTot <- df$segTot / df$segTot
  # Create a new dataframe for stacking
  stacked_df <- data.frame(
    annee = rep(df$annee, 3),
    Composition = rep(c("Contribution Frontalière Suisse", "Contribution non Frontalière Suisse", "Ségrégation inter"), each = nrow(df)),
    value = c(df$contrib_QP, df$contrib_horsQP, df$indice_inter)
  )
  
  # Create the stacked bar chart using ggplot
  ggplot(stacked_df, aes(x = annee, y = value, fill = Composition)) +
    geom_col() +
    labs(x = "Annee", y = " % Ségrégation Totale") +
    scale_fill_manual(values = c("Contribution Frontalière Suisse" = Palette[1], "Contribution non Frontalière Suisse" = Palette[3], "Ségrégation inter" = Palette[9]),
                      labels = c("Contribution Frontalière Suisse", "Contribution non Frontalière Suisse", "Ségrégation inter")) +
    # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
    theme_minimal()
  
  ggsave(paste0("Evolution_inter_intra_pourcent.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  #Evolution inter/intra en base 100
  df <- datadash[["44"]][["tabContrib_theil"]]
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   segTot *100 / segTot[1] ,
                  group = 1,
                  color ="Ségrégation totale"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   indice_inter *100 / indice_inter[1] ,
                  group = 1,
                  color ="Ségrégation inter"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   contrib_QP * 100 / contrib_QP[1] ,
                  group = 1,
                  color ="Contribution Frontalière Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   contrib_horsQP * 100 / contrib_horsQP[1] ,
                  group = 1,
                  color ="Contribution non Frontalière Suisse"),
              size=1.5) +
    scale_color_manual(name = "Composition en base 100",
                       breaks = c("Ségrégation totale",
                                  "Ségrégation inter",
                                  "Contribution Frontalière Suisse",
                                  "Contribution non Frontalière Suisse"),
                       values = c("Ségrégation totale" = Palette[1],
                                  "Ségrégation inter" = Palette[3],
                                  "Contribution Frontalière Suisse" = Palette[6],
                                  "Contribution non Frontalière Suisse" = Palette[9])) +
    labs(x = "Année", y = "Contribution inter/intra en base 100 ")
  
  ggsave(paste0("Evolution_inter_intra_base_100.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
  
  #Evolution indice rural/urbain
  ggplot(df) +
    geom_line(aes(x =   annee,
                  y =   indice_QP ,
                  group = 1,
                  color ="Indice Frontalier Suisse"),
              size=1.5) +
    geom_line(aes(x =   annee,
                  y =   indice_horsQP ,
                  group = 1,
                  color ="Indice non Frontalier Suisse"),
              size=1.5) +
    scale_color_manual(name = "Indice de ségrégation",
                       breaks = c("Indice Frontalier Suisse",
                                  "Indice non Frontalier Suisse"),
                       values = c("Indice Frontalier Suisse" = Palette[1],
                                  "Indice non Frontalier Suisse" = Palette[9])) +
    labs(x = "Année", y = "Indice de ségrégation ")
  
  ggsave(paste0("Evolution_indice_frontalier_non_frontalier.png"),
         path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/", as.character(seuil_nb_hab), "/"))
}

# Create the stacked bar chart using ggplot
ggplot(df_plot_seuil, aes(x = Seuil, y = value, fill = composition)) +
  geom_col() +
  labs(x = "Seuil", y = " % ségrégation totale", title = "Décomposition en part inter/intra de la ségrégation totale") +
  scale_fill_manual(values = c("contribution_frontaliere Suisse" = Palette[1], "contribution_non_frontaliere Suisse" = Palette[3], "indice_inter" = Palette[9]),
                    labels = c("contribution_frontaliere Suisse", "contribution_non_frontaliere Suisse", "indice_inter")) +
  # geom_text(aes(label = value), vjust = -0.5, size = 2, color = "black") +
  theme_minimal()

ggsave(paste0("Comparaison_seuil.png"),
       path = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/"))

#Communes gardées pour chaque seuil
df_liste_com_frontalier <- df_liste_com_frontalier %>% select(-test) %>% as.data.frame()
colnames(df_liste_com_frontalier) <- Liste_seuil
write.csv(df_liste_com_frontalier , file = paste0("U:/AU33/Plot/", as.character(taille), "/frontalier_suisse/liste_communes.csv")
          , row.names=TRUE)

#### *********************************************
#### end ####

