# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                       Application de cartographie 3D
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @auteur : Julien PRAMIL
#' @description Application Shiny permettant de faire varier le millésime des cartes 3D


# Spécifications pour lancement par .exe
setwd("X:/HAB-PsarAU-segregation/au33") 
.libPaths("packages")
library(stats)
library(grDevices)


# Packages 
library(tidyverse)
library(htmlwidgets)
library(sf)
library(deckgl)
library(shiny)
library(classInt)
library(RColorBrewer)
library(leaflet)
library(htmltools)

# Extension du package DeckGl uniquement dispo sur Github (fonction set_map_view)
# devtools::install_local("Y:/Logiciels/R/LocalRepository_specifique/github/deckgl-feature-update-view-state.zip")

source("R/00a_chemins.R")

# Paramètre fond de carte (token Mapbox)
Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1IjoianVsaWVucDM0NyIsImEiOiJja2dxbHdhdnYxb2JnMzlyeDJ2MmM0anR5In0.sDpsQylkzGsGF5m-SGl_mA")

# Importation des villes (vecteur nommé pour l'ui)
linputVille <- readRDS(paste0(repo_couches,"liste_AAV_app.RDS"))


# ****************************************************
# ====           Interface utilisateur            ====
# ****************************************************

# input <- list()
# input$zaav="013";input$taille_carreau=200;input$annee=2010

ui <- navbarPage("Segregation en 3D",
                 tabPanel("Carte",
                          fluidPage(
                            fluidRow(
                              column(3,
                                     selectInput(
                                       "zaav",
                                       "Ville :",
                                       linputVille,
                                       selected = "013"
                                     )
                                     ),
                              column(
                                2,
                                selectInput(
                                  "taille_carreau",
                                  "Taille des carreaux :",
                                  c(
                                    "200m" = "200",
                                    "300m" = "300",
                                    "500m" = "500",
                                    "1000m" = "1000",
                                    "2000m" = "2000"
                                  ),
                                  selected = "1000m"
                                ),
                              ),
                              column(
                                3,
                                selectInput(
                                  inputId = "hauteurVar",
                                  label = "Variable d'altitude",
                                  choices = list(
                                    "Versions brutes"=list("Segregation brute","Contribution brute"),
                                    "Versions lissees"=list("Segregation lissee","Contribution lissee")
                                  )
                                )
                                
                              ),
                              column(
                                4,
                                sliderInput(
                                  "annee",
                                  "Annee :",
                                  min = 2004,
                                  max = 2017,
                                  value = 2004,
                                  animate = T,
                                  sep = ""
                                )
                              )
                              
                            ),
                            deckglOutput("rdeck")
                          ))#,
                 # tabPanel("Explications",
                 #          fluidRow(
                 #                   #htmlOutput("explications")
                 #                   # includeMarkdown("explications_cartes3d.Rmd")
                 #                   # includeHTML("explications_cartes3d.html")
                 # 
                 #          ))
                 )
                    


# *******************************************************
# ====                   Serveur                     ====
# *******************************************************

server <- function(input, output) {
  
  #### Importation de la liste correspondant à la ville*grille 
  liste_centroSmooth2 <- reactive({
    readRDS(paste0(repo_couches,"liste_centroSmooth_",
                "ville",input$zaav,
                "_c",input$taille_carreau,".RDS"))
  })
  
  #### Récupération coordonnées centre de l'agglo (pour centrer la carte)
  centre_agglo <- reactive({
    liste_centroSmooth2()$annee04$geometry %>% st_union() %>% st_centroid() %>%
      st_coordinates()
  })
  
  #### Selection de l'année dans la liste "liste_centroSmooth2
  mydata <- reactive({
    annee_sym <- as.character(input$annee) %>% str_sub(3, 4)
    liste_centroSmooth2()[[paste0("annee", annee_sym)]]
  })
  
  #### Selection de la variable à représenter en altitude
  varAltitude <- reactive({
    switch(input$hauteurVar,
           "Segregation brute"="segregStandar",
           "Contribution brute"="contribSegreg",
           "Segregation lissee"="segregStandar_liss",
           "Contribution lissee"="contribSegreg_liss"
           )
  })
  
  #### Paramètre d'élévation (en fonction de la zone d'étude)
  elevationScale <- reactive({
    # Hateur max d'une barre = dixième du "périmètre" max de la ville
    hauteur_cyble_max <- liste_centroSmooth2()$annee04 %>% st_bbox() %>% st_as_sfc() %>% 
      st_cast("POINT") %>% st_distance() %>% max() %>% as.numeric()/10
    
    # Entropie maximale prise par un carreau
    segreg_max <- liste_centroSmooth2()$annee04[[varAltitude()]] %>% max()
    
    # Coefficient de proportionnalité pour passer de l'indice de ségrégation à la hauteur du carreau 
    hauteur_cyble_max/segreg_max
  })
  
  #### Création de la carte de base
  output$rdeck <- renderDeckgl({
    deckgl(zoom = 11, pitch = 35) %>%
      add_legend(colors = RColorBrewer::brewer.pal(5,"RdYlBu"),
                 labels = c("pauvre","pauvre intermédiaire","moyen","aisé intermédiaire","aisé"),
                 title = "Revenu par UC (mediane)") %>% 
      add_mapbox_basemap(style = "mapbox://styles/mapbox/satellite-streets-v11",
                         token = Sys.getenv("MAPBOX_API_TOKEN")) 
  })
  
  #### Adaptations dynamiques de la carte
  observe({
    deckgl_proxy("rdeck") %>%
      add_grid_cell_layer(
        data = mydata(),
        getPosition = ~long + lat,
        getElevation = as.formula(paste0("~",varAltitude())),
        getFillColor = ~couleur,
        elevationScale = elevationScale(),
        cellSize = input$taille_carreau,
        extruded = TRUE,
        opacity=0.6,
        getTooltip = JS("object => `Segregation standard. brute : ${object.segregStandar_lab} <br> Segregation standard. lissee: ${object.segregStandar_liss_lab} <br> Contribution segreg. brute : ${object.contribSegreg_lab} <br> Contribution segreg. lissee: ${object.contribSegreg_liss_lab} <br> Mediane de revenu par UC : ${object.median_carreau_lab} <br> Nombre de menages : ${object.nbmen_carreau} <br> Identifiant du carreau : ${object.idcarreau}`")
      ) %>% 
      set_view_state(latitude = centre_agglo()[2],longitude = centre_agglo()[1]) %>%  
      update_deckgl(it = "works") 
  })

  
  
  # getPage<-function() {
  #   return(includeHTML("explications_cartes3d.html"))
  # }
  # output$explications<-renderUI({getPage()})

   }

# *****************************************
#       Lancement de l'application     ----
# *****************************************

shinyApp(ui, server,options = c(launch.browser = TRUE)) 
