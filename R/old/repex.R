# Bug Plotly : exemple reproductible
# ::::::::::::::::::::::::::::::::::



#### Packages --------------------------------------------

listpack <- c("tidyverse",
              "data.table",
              "plotly")

lapply(listpack, function(pkg) {
  if (system.file(package = pkg) == '') install.packages(pkg)
  require(pkg,character.only = T,quietly = T)
})


#### Donn�es ---------------------------------------------

chemin <- "X:/HAB-PSAR-AU-AU33-DEV/au33_v2/data/output/cylindrage/"
data_deciles <- fread(paste0(chemin,"data_deciles.csv"))


#### Version compl�te bugu�e -------------------------------------------

data <- data_deciles

#### Cr�ation du widget (liste des r�gions) ===============
buttons <- unique(data$reg) %>% map( ~ list(
  method = "restyle",
  args = list("transforms[0].value", .x),
  label = .x
))

updatemenus <-  list(
  list(
    type = 'dropdown',
    active = 0,
    buttons = buttons
  )
)

#### Cr�ation du graphique avec plotly =================

chart <- plot_ly(
  data = data,
  x = ~ decile,
  y = ~ val,
  color =  ~ annee,
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '=',
      value = data$reg[1]
    )
  )) %>% layout(
  updatemenus = updatemenus
) 

chart

#### Version non-buguée (Bretagne + IDF) -------------------------------------------

#### Version buguée (France + IDF) -------------------------------------------

vecdep=c("France","11")
data <- data_deciles[reg %in% vecdep]

#### Création du widget (liste des régions) --------------------------------
buttons <- unique(data$reg) %>% map( ~ list(
  method = "restyle",
  args = list("transforms[0].value", .x),
  label = .x
))

updatemenus <-  list(
  list(
    type = 'dropdown',
    active = 0,
    buttons = buttons
  )
)

#### Création du graphique avec plotly -------------------------------------------

chart <- plot_ly(
  data = data,
  x = ~ decile,
  y = ~ val,
  color =  ~ annee,
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '='
      # value = passage$libReg[passage$reg==vecdep[1]]
    )
  ))

chart <-  chart %>% layout(
  updatemenus = updatemenus
) 

chart
