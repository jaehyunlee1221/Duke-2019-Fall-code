#install.packages("repurrrsive")
library(repurrrsive)
library(tidyr)
library(tidyverse)
library(dplyr)
#Exercise 1
usa_pop <- world_bank_pop %>%
  filter(country == "USA")

usa_pop %>%
  gather(key = "year", value = "value", `2000`:`2017`) %>%
  spread(key = "indicator", value = value)

#Exercise 2 - mine
name <- rep(0,length(sw_people))
for(i in 1:length(name)){
  name[i] <- sw_people[[i]]$name
}
listbreak <- function(x){
  return(unlist(x)["name"])
}
name2 <- sapply(sw_people,listbreak)




#Exercise 3
#get the calss of each variable
map_df(mtcars, class)
#get the fourth row such that result is a character vector
#map_df(mtcars, function(x){if(class(x) == "character") return(x[4])})
map_chr(mtcars, 4)
#compute the mean of each variable
map_df(mtcars, mean)
#compute the mean and median for each variable such that the result is a data frame
map_df(mtcars, function(x) {return(c(mean(x),median(x)))})
