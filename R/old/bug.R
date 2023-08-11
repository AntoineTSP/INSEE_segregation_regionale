#### Packages --------------------------------------------

listpack <- c("tidyverse",
              "data.table",
              "plotly")

lapply(listpack, function(pkg) {
  if (system.file(package = pkg) == '') install.packages(pkg)
  require(pkg,character.only = T,quietly = T)
})

#### Données ---------------------------------------------
# Jeu trié
jeu1 <- list("men04","53","decile1",7,
             "men04","53","decile2",10,
             "men04","53","decile3",12,
             "men04","11","decile1",6,
             "men04","11","decile2",10,
             "men04","11","decile3",13,
             "men04","France","decile1",8,
             "men04","France","decile2",9,
             "men04","France","decile3",10,
             "men05","53","decile1",8,
             "men05","53","decile2",11,
             "men05","53","decile3",14,
             "men05","11","decile1",7,
             "men05","11","decile2",10,
             "men05","11","decile3",14,
             "men05","France","decile1",9,
             "men05","France","decile2",11,
             "men05","France","decile3",12)

# Jeu mal trié
jeu2 <- list("men04","53","decile1",7,
             "men04","53","decile2",10,
             "men04","53","decile3",12,
             "men04","11","decile1",6,
             "men04","11","decile2",10,
             "men04","11","decile3",13,
             "men05","53","decile1",8,
             "men05","53","decile2",11,
             "men05","53","decile3",14,
             "men05","11","decile1",7,
             "men05","11","decile2",10,
             "men05","11","decile3",14,
             "men04","France","decile1",8,
             "men04","France","decile2",9,
             "men04","France","decile3",10,
             "men05","France","decile1",9,
             "men05","France","decile2",11,
             "men05","France","decile3",12)

# Mise en forme des données
data1 <- jeu1 %>% matrix(ncol=4,byrow = T) %>% as.data.table()
data2 <- jeu2 %>% matrix(ncol=4,byrow = T) %>% as.data.table()

colnames(data1) <- c("annee","reg","decile","val")
colnames(data2) <- c("annee","reg","decile","val")

data1[,':='(reg=as.character(reg),annee=as.character(annee),decile=as.character(decile),val=as.numeric(val))]
data2[,':='(reg=as.character(reg),annee=as.character(annee),decile=as.character(decile),val=as.numeric(val))]


#### Graphique non-bugué --------------------------------------

buttons1 <- data1[, unique(reg)] %>% map( ~ list(
  method = "restyle",
  args = list("transforms[0].value", .x),
  label = .x
))

updatemenus1 <-  list(
  list(
    type = 'dropdown',
    active = 0,
    buttons = buttons1
  )
)


plot_ly(
  data = data1,
  x = ~ decile,
  y = ~ val,
  color =  ~ annee,
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '=',
      value = "53"
    )
  )) %>% layout(
    updatemenus = updatemenus1
  ) 

#### Graphique bugué--------------------------------------

buttons2 <- data2[, unique(reg)] %>% map( ~ list(
  method = "restyle",
  args = list("transforms[0].value", .x),
  label = .x
))

updatemenus2 <-  list(
  list(
    type = 'dropdown',
    active = 0,
    buttons = buttons2
  )
)


plot_ly(
  data = data2,
  x = ~ decile,
  y = ~ val,
  color =  ~ annee,
  transforms = list(
    list(
      type = 'filter',
      target = ~reg,
      operation = '=',
      value = "53"
    )
  )) %>% layout(
    updatemenus = updatemenus2
  ) 





