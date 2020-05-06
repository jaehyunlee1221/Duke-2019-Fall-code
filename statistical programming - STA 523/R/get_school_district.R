library(tidyverse)
library(jsonlite)

setwd("C:/Users/MangoLee/Documents/R/Duke-Fall2019/statistical programming")

url <- "https://data.okc.gov/services/portal/api/data/records/School%20District%20Boundaries"
page <- read_json(url)
Records <- page$Records
field <- map_dfr(map(page$Fields,unlist),as.list)

change_name <- function(x){
    names(x) <- field$FieldName
    return(x)
}
school <- map_dfr(map(map(Records,change_name),unlist),as.list)

saveRDS(school, "data/school_districts.rds")
