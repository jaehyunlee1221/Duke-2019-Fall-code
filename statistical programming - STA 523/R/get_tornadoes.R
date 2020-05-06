library(rvest)
library(tidyverse)
library(jsonlite)

setwd("C:/Users/MangoLee/Documents/R/Duke-Fall2019/statistical programming")

empty_table <- list(NA)
years <- 1998:2017
state <- "Oklahoma"
for(i in 1:length(years)){
  
  url <- paste("http://www.tornadohistoryproject.com/tornado",state,years[i],"table",sep = "/")
  
  table <- url %>% 
    read_html() %>% 
    html_nodes(css = "#table") %>% 
    html_children() %>% 
    html_table(header = T)  %>% 
    as.data.frame()
  index <- table$E == "E"
  table <- table[!index,]
  empty_table[[i]] <- table
  Sys.sleep(1)
}

tornadoes <- empty_table[[1]]

for(i in 2:length(years)){
  tornadoes <- rbind.data.frame(tornadoes, empty_table[[i]])
}

saveRDS(tornadoes, file = "data/ok_tor.rds")
