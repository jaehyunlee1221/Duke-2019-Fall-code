library(tidyselect)
library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
url <- "http://web.mit.edu"

response <- GET(url)

content(response)


### Exercise

brewery <- list(NA)
for (i in 1:30){
url <- paste("https://api.openbrewerydb.org/breweries?page=",i,"&per_page=30")
temp <- read_json(url)
brewery[[i]] <- map_dfr(map(temp,unlist),as.list)
}
brewery2 <- brewery[[1]]
for (i in 2:30){
  brewery2 <- rbind.data.frame(brewery2,brewery[[i]])
}
map(brewery,rbind.data.frame)
bind_rows(brewery)
brewery <- map_dfr(map(brewery,unlist),as.list)
brewery <- read_json("https://api.openbrewerydb.org/breweries")
brewery <- map_dfr(map(brewery,unlist), as.list)


### How many breweries are located in durham, NC?
brewery %>% 
  filter(city == "Durham") %>% 
  group_by(city) %>% 
  summarise(count = n())

### Which city in North Carolina has the most micro breweries? 
### How many micro breweries do they have?

brewery %>% 
  filter(brewery_type == "micro") %>% 
  group_by(city) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

### In what cities are Founders, Yuengling, and Boulevard brewed?