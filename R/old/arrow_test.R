
fichiers <- fichiers[1:3]

lmen <- lapply(fichiers, function(tab){
  fread(paste0(chemin_basesmen,tab))
})
names(lmen) <- str_remove_all(fichiers,".csv")

chem_temp <- paste0(chemin_input,"temp")
dir.create(chem_temp)

install.packages("arrow")
library(arrow)

men <- rbindlist(lmen,idcol = "annee")

write_dataset(men, paste0(chem_temp,"/bases_men_parquet"), partitioning = c("annee"))

# Read data from directory
men_parquet <- open_dataset(paste0(chem_temp,"/bases_men_parquet"))

men_parquet %>% filter(annee=="men04" & str_sub(depcom,1,2)=="01") %>% collect()
men_parquet04 <- men_parquet %>% filter(annee=="men04") %>% collect()


data_parquet <- open_dataset(paste0(chem_temp,"/csv/csv01001.csv"),format = "csv")
data_parquet <- data_parquet %>% mutate(depcom=as.character(depcom))

# data_parquet %>% mutate(depcom=as.character(depcom)) %>% filter(depcom=="01001") %>% collect()
arrow::write_dataset(data_parquet,partitioning = c("annee"),path = paste0(chem_temp,"/myparketcsv"))
toto <- open_dataset(paste0(chem_temp,"/myparketcsv"))


data_parquet <- open_dataset(
  chemin_basesmen,
  schema = schema(id=string(),revdecucm =string(),nbpersm =string(),depcom=string(),x=string(),y=string()),
  format = "csv")
# data_parquet <- data_parquet %>% 
#   mutate(id=as.character(id),depcom=as.character(depcom))
arrow::write_dataset(data_parquet,path = paste0(chem_temp,"/myparketcsv"))
toto <- open_dataset(paste0(chem_temp,"/myparketcsv"))
toto %>% summarise(n()) %>% collect()
toto %>%  collect()
men04 <- fread(paste0(chem_temp,"/csv2/men04.csv") )
men05 <- fread(paste0(chem_temp,"/csv2/men04.csv") )
fwrite(men04,paste0(chem_temp,"/csv2/men04.csv"))
fwrite(men05,paste0(chem_temp,"/csv2/men05.csv"))

men04 <- men04 %>% mutate(id=as.character(id),depcom=as.character(depcom))
men05[3536576,]



