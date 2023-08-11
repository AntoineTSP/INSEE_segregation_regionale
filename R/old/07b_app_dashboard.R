# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::: Tableau de bord ::::::::::::::::::::: 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#' @auteur : Julien PRAMIL
#' @description : Application Shiny de tableau de bord
#' Appel des figures et fonctions créées dans le script 07a_fonctions_dashboard.R



#### Spécifications pour lancement par .exe
setwd("X:/HAB-PsarAU-segregation/au33") 
.libPaths("packages")
library(stats)
library(base)
library(grDevices)

##### Packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(purrr)
library(classInt)
library(RColorBrewer)

#### Importation des chemins et fonctions
source("R/00a_chemins.R")
source("R/00b_fonctions.R")
source("R/07a_fonctions_dashboard.R") # pour les figures du Tableau de bord

#### Importation des données en input
lcouches <- readRDS(paste0(repo_couches,"lcouches.RDS"))

#### Chargement de la liste nommée des noms des agglo
linputVille <- readRDS(paste0(repo_couches,"liste_AAV_app.RDS"))
AAVpassage <- data.frame(AAV20=linputVille,nomAAV20=names(linputVille))

#### Pour avoir les COG de l'agglo (et tester présence de QP)
passageZaav <- readRDS(paste0(chemin_defZaav,"AAV_definitif_09092020.rds"))


# ********************************************
#           Interface utilisateur         ----
# ********************************************

cat("debut UI")

ui <- dashboardPage(
  dashboardHeader(title = "AU33 : Tableau de bord - Segregation",
                  titleWidth = 450),
  
  dashboardSidebar(
    selectInput("zaav",
                "Ville :",
                linputVille)
    # selectInput("taille_carreau", "Taille des carreaux :",
    #             c("200m" = "200",
    #               "300m" = "300",
    #               "500m" = "500",
    #               "1000m" = "1000",
    #               "2000m" = "2000"),
    #             selected = "1000m"),
    # selectInput("nivAgglo", "Niveau :",
    #             c("Pole uniquement" = "pole",
    #               "ZAAV entiere" ="zaavTot" )),
    # selectInput("filtrage", "Filtrage des carreaux :",
    #             c("Pas de filtrage" = "sansFiltrage",
    #               "Seulement carreaux > 20 menages" ="avecFiltrage" ))
  ),
  dashboardBody(
    navbarPage("",
               navbarMenu("Segregation Theil",
                          tabPanel("Indices",
                                   
                                   
                                   fluidPage(
                                     fluidRow(
                                       column(2,h5("Rebasage")),
                                       column(2,checkboxInput("rebase1", label = "Base 100 en 2004", value = F)) 
                                     ),
                                     fluidRow(plotlyOutput("plotSimpleTheil"))
                                     )
                                   
                                   
                          ),
                          tabPanel("Comparaisons",
                                   fluidPage(
                                     plotlyOutput("plotComparTheil")
                                   )
                          ),
                          tabPanel(
                            "Contributions QPV",
                            # conditionalPanel(condition = "input.taille_carreau != 200",
                            #                  textOutput("messageContrib1")),
                            #conditionalPanel(
                              #condition = "input.taille_carreau == 200",
                              plotlyOutput("graphDistriTheil"),
                              plotlyOutput("graph_evolPartQPTheil"),
                              hr(),
                              plotlyOutput("tableauContribTheil"),
                              plotlyOutput("graphContribBarTheil"),
                              plotlyOutput("graphContribBarPctTheil"),
                              plotlyOutput("graphContribEvolTheil"),
                              plotlyOutput("graphIndiceIntraTheil")
                           # )
                          ),
                          tabPanel(
                            "Decomposition par groupes",
                            plotlyOutput("indicesBinaires_theil"),
                            hr(),
                            plotlyOutput("indicesBinaires_theil_base100"),
                            hr(),
                            plotlyOutput("contribBinaires_theil"),
                            hr(),
                            h1("Tableaux detailles"),
                            plotlyOutput("tableauContribGroupes_theil1"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_theil2"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_theil3"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_theil4"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_theil5")
                          )
               ),
               navbarMenu("Segregation IIM",
                          tabPanel("Indices",
                                   fluidPage(
                                     fluidRow(
                                       column(2,h5("Rebasage")),
                                       column(2,checkboxInput("rebase2", label = "Base 100 en 2004", value = F)) 
                                     ),
                                     fluidRow(plotlyOutput("plotSimpleIIM"))
                                   )
                                   
                                  
                          ),
                          tabPanel("Comparaisons",
                                   fluidPage(
                                     plotlyOutput("plotComparIIM")
                                   )
                          ),
                          tabPanel("Contributions QPV",
                                   # conditionalPanel(condition = "input.taille_carreau != 200",
                                   #                  textOutput("messageContrib2")),
                                   # conditionalPanel(condition = "input.taille_carreau == 200",
                                                    plotlyOutput("graphDistriIIM"),
                                                    plotlyOutput("graph_evolPartQPIIM"),
                                                    hr(),
                                                    plotlyOutput("tableauContribIIM"),
                                                    plotlyOutput("graphContribBarIIM"),
                                                    plotlyOutput("graphContribBarPctIIM"),
                                                    plotlyOutput("graphContribEvolIIM"),
                                                    plotlyOutput("graphIndiceIntraIIM")
                                   #)
                                   
                          ),
                          tabPanel(
                            "Decomposition par groupes",
                            plotlyOutput("indicesBinaires_iim"),
                            hr(),
                            plotlyOutput("indicesBinaires_iim_base100"),
                            hr(),
                            plotlyOutput("contribBinaires_iim"),
                            hr(),
                            h1("Tableaux detailles"),
                            plotlyOutput("tableauContribGroupes_iim1"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_iim2"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_iim3"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_iim4"),
                            hr(),
                            plotlyOutput("tableauContribGroupes_iim5")
                          )
                 
               ),
               tabPanel("Complements",
                        fluidPage(
                          plotlyOutput("nb_carreaux"),
                          # htmlOutput("effectifs_moyens"),
                          plotlyOutput("tabDecilesMen",height = "150px"),
                          plotlyOutput("tabDecilesMen_eff",height = "350px"),
                          plotlyOutput("tabDecilesIndiv",height = "150px"),
                          plotlyOutput("tabDecilesIndiv_eff",height = "350px"),
                          hr(),
                          plotlyOutput("tab_effectifs"),
                          hr(),
                          plotlyOutput("def_quantiles")
                        )
               )
    )
               
    
  )
)

# ************************************
#             Serveur  ---------------
# ************************************

server <- function(input, output) {
  
  #### On restreint lcouches à la ZAAV
  maCouche <- reactive({
    # maCouche <- restricCouches(input$zaav,input$nivAgglo,input$taille_carreau,input$filtrage)
    maCouche <- restricCouches(input$zaav)
    maCouche
  })
  
  #### Liste de deux éléments : approche Theil et approche IIM
  maListe <- reactive({
    maListe <- extracListes(maCouche())
  })
  
  #### Graph des indices ------
  
  output$plotSimpleTheil <- renderPlotly({
    data <- maListe()$maListeTheil %>% 
      transfo_tabIndiceTot() %>% 
      rebaseTable(input$rebase1)
    
    graphSimple(data)
  })
  
  output$plotSimpleIIM <- renderPlotly({
    data <- maListe()$maListeIIM %>% 
      transfo_tabIndiceTot() %>% 
      rebaseTable(input$rebase2)
    graphSimple(data)
  })
  
  #### Graph de comparaison des indices
  
  output$plotComparTheil <- renderPlotly({
    dataComparTheil <- extrac_compar(lcouches,typeIndic = "theil")
    graphCompar(dataComparTheil,input$zaav)
  })
  
  output$plotComparIIM <- renderPlotly({
    dataComparIIM <- extrac_compar(lcouches,typeIndic = "iim")
    graphCompar(dataComparIIM,input$zaav)
  })
  
  
  
  #### Décomposition QP/hors QP -----------------------------------------
  
  # output$messageContrib1 <- renderText({
  #   "Calcul des contributions des QPV seulement sur une maille carroyée de 200m"
  # })
  
  # output$messageContrib2 <- renderText({
  #   "Calcul des contributions des QPV seulement sur une maille carroyée de 200m"
  # })
  
  # Graphiques de distribution
  
  output$graphDistriTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_distriQP(maCouche())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graphDistriIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_distriQP(maCouche())
    }else{
      res <- NULL
    }
    res
  })
  
  # Evolution part des QP
  
  output$graph_evolPartQPTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_evolPartQP(maListe())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graph_evolPartQPIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_evolPartQP(maListe())
    }else{
      res <- NULL
    }
    res
  })
  
  # Création des tables détaillées
  
  tabContribTheil <- reactive({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returnTabContrib(maListe()$maListeTheil)
    }else{
      res <- NULL
    }
    res
  })
  
  tabContribIIM <- reactive({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returnTabContrib(maListe()$maListeIIM)
    }else{
      res <- NULL
    }
    res
  })
  
  # Mise en forme des tableaux avec Plotly
  
  output$tableauContribTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- tableau_contribQP(tabContribTheil())
    }else{
      res <- NULL
    }
    res
  })
  
  output$tableauContribIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- tableau_contribQP(tabContribIIM())
    }else{
      res <- NULL
    }
    res
  })
  
  # Graphiques des contributions en barres
  
  output$graphContribBarTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returngraphContribBar(tabContribTheil())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graphContribBarIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returngraphContribBar(tabContribIIM())
    }else{
      res <- NULL
    }
    res
  })
  
  # Part des contributions
  
  output$graphContribBarPctTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returngraphContribBarPct(tabContribTheil())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graphContribBarPctIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- returngraphContribBarPct(tabContribIIM())
    }else{
      res <- NULL
    }
    res
  })
  
  # Evolutions des contributions
  
  output$graphContribEvolTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graphContribEvol(tabContribTheil())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graphContribEvolIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graphContribEvol(tabContribIIM())
    }else{
      res <- NULL
    }
    res
  })
  
  # Graph des indices intra
  
  output$graphIndiceIntraTheil <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_IndiceIntra(tabContribTheil())
    }else{
      res <- NULL
    }
    res
  })
  
  output$graphIndiceIntraIIM <- renderPlotly({
    if(zoneAvQP(passageZaav %>% filter(AAV20==input$zaav) %>% pull(CODGEO) %>% unique())){
      res <- graph_IndiceIntra(tabContribIIM())
    }else{
      res <- NULL
    }
    res
  })
  
  
  
  
  
  #### Décomposition par groupes de revenus ---------
  
  # Création des tables détaillées
  
  tabContribGroupes_theil <- reactive({
      returnTabContrib_groupes(maListe()$maListeTheil)
  })
  
  tabContribGroupes_iim <- reactive({
  returnTabContrib_groupes(maListe()$maListeIIM)
  })
  
  # Génération des tableaux mis en forme
  output$tableauContribGroupes_theil1 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_theil()$g1) %>% 
      layout(title="Decomposition groupe (1) versus groupes (2,3,4,5) ")
  })
  
  output$tableauContribGroupes_theil2 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_theil()$g2) %>% 
      layout(title="Decomposition groupe (2) versus groupes (1,3,4,5) ")
  })
  
  output$tableauContribGroupes_theil3 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_theil()$g3) %>% 
      layout(title="Decomposition groupe (3) versus groupes (1,2,4,5) ")
  })
  
  output$tableauContribGroupes_theil4 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_theil()$g4) %>% 
      layout(title="Decomposition groupe (4) versus groupes (1,2,3,5) ")
  })
  
  output$tableauContribGroupes_theil5 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_theil()$g5) %>% 
      layout(title="Decomposition groupe (5) versus groupes (1,2,3,4) ")
  })
  
  output$tableauContribGroupes_iim1 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_iim()$g1) %>% 
      layout(title="Decomposition groupe (1) versus groupes (2,3,4,5) ")
  })
  
  output$tableauContribGroupes_iim2 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_iim()$g2) %>% 
      layout(title="Decomposition groupe (2) versus groupes (1,3,4,5) ")
  })
  
  output$tableauContribGroupes_iim3 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_iim()$g3) %>% 
      layout(title="Decomposition groupe (3) versus groupes (1,2,4,5) ")
  })
  
  output$tableauContribGroupes_iim4 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_iim()$g4) %>% 
      layout(title="Decomposition groupe (4) versus groupes (1,2,3,5) ")
  })
  
  output$tableauContribGroupes_iim5 <- renderPlotly({
    tableau_contribGroupes(tabContribGroupes_iim()$g5) %>% 
      layout(title="Decomposition groupe (5) versus groupes (1,2,3,4) ")
  })
  
  
  # Graphique sur les composantes binaires
  output$indicesBinaires_theil <- renderPlotly({
    graph_indiceBinaire(tabContribGroupes_theil())
  })
  
  output$indicesBinaires_theil_base100 <- renderPlotly({
    graph_indiceBinaire(tabContribGroupes_theil(),base100 = T)
  })
  
  output$indicesBinaires_iim <- renderPlotly({
    graph_indiceBinaire(tabContribGroupes_iim())
  })
  
  output$indicesBinaires_iim_base100 <- renderPlotly({
    graph_indiceBinaire(tabContribGroupes_iim(),base100 = T)
  })
  
  # Graphique sur les contributions binaires
  output$contribBinaires_theil <- renderPlotly({
    graph_contribBinaire(tabContribGroupes_theil())
  })
  
  output$contribBinaires_iim <- renderPlotly({
    graph_contribBinaire(tabContribGroupes_iim())
  })
  
  
  
  
  # Compléments ------------------------- 
  
  output$tab_effectifs <- renderPlotly({
    tab_effectif(maCouche())
  })
  
  output$nb_carreaux <- renderPlotly({
    nbCarreaux(maCouche())
  })
  

  
  output$tabDecilesMen <- renderPlotly({
    tab_deciles(maCouche(),type="men")$res_def
  })
  
  output$tabDecilesMen_eff <- renderPlotly({
    tab_deciles(maCouche(),type="men")$res_effect
  })
  
  output$tabDecilesIndiv <- renderPlotly({
    tab_deciles(maCouche(),type="indiv")$res_def
  })
  
  output$tabDecilesIndiv_eff <- renderPlotly({
    tab_deciles(maCouche(),type="indiv")$res_effect
  })
  
  output$def_quantiles <- renderPlotly({
    def_quantiles(maCouche())
  })
  
}

# Create Shiny app ------------------- 
shinyApp(ui, server,options = c(launch.browser = TRUE)) 
