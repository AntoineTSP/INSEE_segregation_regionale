# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                          Appli entropie                     
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#' @author Julien PRAMIL
#' @description Application de visualisation de la ségrégation en choisissant 
#' la composition sociale des carreaux (uniquement à visée pédagogique)

# Spécifications pour lancement par .exe
setwd("X:/HAB-PsarAU-segregation/au33_dev") 
.libPaths("packages")

# Packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)


# Importation des chemins
source("R/00a_chemins.R")
source("R/00b_fonctions.R")



# fonctions : 
fentropie <- function(p1,p2,p3,p4,p5){
  if(p1+p2+p3+p4+p5==100) {
    res <- f_entro_partielle(p1/100)+
      f_entro_partielle(p2/100)+
      f_entro_partielle(p3/100)+
      f_entro_partielle(p4/100)+
      f_entro_partielle(p5/100)
  }else{
    res <- "La somme des proportions != 100 %"
  }
  return(res)
}

generHisto <- function(p1,p2,p3,p4,p5,titre){
  
  x <- c('q1','q2','q3','q4','q5')
  y <- c(p1,p2,p3,p4,p5)
  res <- NULL
  
  if(p1+p2+p3+p4+p5==100){
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

# *****************************************
#          Interface utilisateur       ----
# *****************************************


ui <- fluidPage(
  theme = "simplex.min.css",
  tags$style(type = "text/css",
             "label {font-size: 12px;}",
             ".recalculating {opacity: 1.0;}"),
  
  fluidRow(
    column(6, offset = 3,
           h1("Simulateur d'indice de segregation", 
              style = "font-family: 'Source Sans Pro';"),
           h5("Dans le cas d'une ville avec un seul carreau")
    )
  ),
  hr(),
  fluidRow(
    column(2,
           h4("Situation initiale")),
    column(2,
           numericInputIcon("s1_p1", label = h6("Part du groupe 1"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))
           ),
    column(2,
           numericInputIcon("s1_p2", label = h6("Part du groupe 2"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
    column(2,
           numericInputIcon("s1_p3", label = h6("Part du groupe 3"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
    column(2,
           numericInputIcon("s1_p4", label = h6("Part du groupe 4"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
    column(2,
           numericInputIcon("s1_p5", label = h6("Part du groupe 5"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent")))
            
    ),
  hr(),
  fluidRow(
      column(2,
             h4("Situation finale")),
      column(2,
             numericInputIcon("s2_p1", label = h6("Part du groupe 1"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))
      ),
      column(2,
             numericInputIcon("s2_p2", label = h6("Part du groupe 2"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
      column(2,
             numericInputIcon("s2_p3", label = h6("Part du groupe 3"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
      column(2,
             numericInputIcon("s2_p4", label = h6("Part du groupe 4"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent"))),
      column(2,
             numericInputIcon("s2_p5", label = h6("Part du groupe 5"), value = 20,min = 0,max = 100,step = 5,icon=icon("percent")))
    
  ),
  hr(),
  hr(),
  fluidRow(column(6,
                  wellPanel(
                    fluidRow(column(12,plotlyOutput("s1_histo",height = "300px"))),
                    br(),
                    fluidRow(column(6,h4("Entropie initiale :")),
                             column(6,verbatimTextOutput("s1_entrop")))
                  )
                  ),
           column(6,
                  wellPanel(
                    fluidRow(column(12,plotlyOutput("s2_histo",height = "300px"))),
                    br(),
                    fluidRow(column(6,h4("Entropie finale :")),
                             column(6,verbatimTextOutput("s2_entrop")))
                    
                    
                  ))
  ),

  fluidRow(column(6,offset = 3,
                  wellPanel(
                    fluidRow(column(8,h2("Segregation")),
                             column(4,
                             verbatimTextOutput("perte_entrop"),
                             tags$head(tags$style("#perte_entrop{color: red;
                                 font-size: 25px;
                                 }"))
                                    )
                    
                    )
                  )
                  )
  )
)


# *****************************************
#                serveur               ----
# *****************************************

server <- function(input, output) {
  
  output$s1_histo <- renderPlotly({
    generHisto(input$s1_p1,input$s1_p2,input$s1_p3,input$s1_p4,input$s1_p5,titre = "Situation initiale")
  })
  
  output$s2_histo <- renderPlotly({
    generHisto(input$s2_p1,input$s2_p2,input$s2_p3,input$s2_p4,input$s2_p5,titre = "Situation finale")
  })
  
  output$s1_entrop <- renderText({
    fentropie(input$s1_p1,input$s1_p2,input$s1_p3,input$s1_p4,input$s1_p5)
  })
  
  output$s2_entrop <- renderText({
    fentropie(input$s2_p1,input$s2_p2,input$s2_p3,input$s2_p4,input$s2_p5)
  })
  
  output$perte_entrop <- renderText({
    entro_fin <- fentropie(input$s2_p1,input$s2_p2,input$s2_p3,input$s2_p4,input$s2_p5)
    entro_init <- fentropie(input$s1_p1,input$s1_p2,input$s1_p3,input$s1_p4,input$s1_p5)
    
    if(!is.na(as.numeric(entro_fin)) & !is.na(as.numeric(entro_init))){
      chiffre <- -100*(entro_fin/entro_init-1) %>% round(4)
      paste0(chiffre," %")
    }
    
  })
}

# *****************************************
#       Lacement de l'application      ----
# *****************************************


shinyApp(ui, server,options = c(launch.browser = TRUE)) 





