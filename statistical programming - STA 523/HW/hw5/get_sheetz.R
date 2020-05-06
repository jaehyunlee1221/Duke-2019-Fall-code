# Scrape Sheetz data

library(rvest)
library(tidyverse)
library(stringr)
library(jsonlite)

base_url <- "http://www2.stat.duke.edu/~sms185/data/fuel/bystore/zteehs/regions.html"

urls <- read_html(base_url) %>% 
  html_nodes("a") %>% 
  html_attrs() %>% 
  unlist %>% 
  head(10)

data = list()
for(i in 1:10){
  page_content = read_html(urls[i]) %>% 
    html_nodes("body") %>% 
    html_text() %>% 
    gsub("\\[\\[", "[", .) %>% 
    gsub("\\]\\]", "]", .) %>% 
    parse_json()

  data = append(data, page_content)
  
  Sys.sleep(0.01 + rexp(1, 100))
}


saveRDS(data, "data/sheetz/sheetz_data.rds")
