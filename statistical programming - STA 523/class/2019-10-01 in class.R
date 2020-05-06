#install.packages("rvest")
library(rvest)
library(tidyverse)
library(tidyr)
rt <- read_html("https://www.rottentomatoes.com/")

tibble(
movie = rt %>%
  html_nodes("#Top-Box-Office .middle_col a") %>% 
  html_text(),

gross = rt %>%
  html_nodes("#Top-Box-Office .right a") %>% 
  html_text() %>%
  str_remove_all("\\$|M"),

t_score = rt %>%
  html_nodes("#Top-Box-Office .tMeterScore") %>%
  html_text() %>% 
  str_remove_all("%"),

rating = rt %>% 
  html_nodes("#Top-Box-Office .tiny") %>% 
  html_attr("class") %>%
  str_remove_all("icon |tiny ") %>% 
  str_replace("_"," ")
)
rt %>%
  html_nodes()

##Exercise

store <- read_html("https://www.wegmans.com/stores.html")
#name of each store
name <- store %>%
  html_nodes(".left-image a") %>% 
  html_text() 
name <- name[9:20]

#website of each store, remove ununity and unify it same format
website <- store %>% 
  html_nodes(".left-image a") %>% 
  html_attr("href") 
website <- website[9:20]
http <- "https://www.wegmans.com"
website <- map(website,~str_remove(.x,pattern = http))
website <- unlist(map(website,~str_c(http,.x)))

#parsing function
parsing <- function(url){
  temp <- read_html(url) %>%
    html_nodes(".tel-desktop") %>%
    html_text()
  return(temp)
}
result <- map(website,parsing)
for(i in 1:length(result)){
  names(result[[i]]) <- paste("V",1:length(result[[i]]))
}
result2 <- map_dfr(result,as.list)
result2 <- result2[,c(5,6)]
result2 <- result2 %>%
  mutate(`V 5` = str_remove_all(`V 5`,"[\n\t]+")) %>%
  mutate(`V 6` = str_remove_all(`V 6`,"[\\(|\\)]")) %>%
  mutate(`V 6` = str_replace(`V 6`," ","-")) %>% 
  separate(`V 5`, sep = "[\\,\\s] ", into = c("full address","state")) %>%
  mutate(state = str_remove_all(state, "[\\s\\d{5}]")) %>%
  rename(phone = `V 6` )
final <- cbind(name,result2,website)
