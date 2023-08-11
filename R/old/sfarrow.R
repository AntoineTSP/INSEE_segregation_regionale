source("R/00a_global.R",encoding = "UTF-8")

readRDS(paste0(chemin_output,"centroides_smooth/","010.rds"))

# df_centro_smooth <- st_sf(df_centro_smooth)

install.packages("sfarrow")
library(sfarrow)
library(arrow)

sfarrow::write_sf_dataset(
  df_centro_smooth,
  partitioning = c("aav","tcar"),
  path = paste0(chemin_output,"centroides_smooth/centroides_smooth")
  )

toto <- arrow::open_dataset(paste0(chemin_output,"centroides_smooth/centroides_smooth"))

tata <- toto %>% 
  filter(aav=="010",tcar=="500") %>%
  read_sf_dataset()
