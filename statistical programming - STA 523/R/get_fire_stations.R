library(tidyverse)
library(jsonlite)

setwd("C:/Users/MangoLee/Documents/R/Duke-Fall2019/statistical programming")

url <- "https://data.okc.gov/services/portal/api/data/records/Fire%20Stations"
page <- read_json(url)
Records <- page$Records
field <- map_dfr(map(page$Fields,unlist),as.list)

change_name <- function(x){
  names(x) <- field$FieldName
  return(x)
}
fire <- map_dfr(map(map(Records,change_name),unlist),as.list)

saveRDS(fire, "data/fire_stations.rds")
