library(lobstr)
library(parallel)
devtools::install_github("tidyverse/multidplyr")
library(multidplyr)
install.packages("vroom")
#install.packages("lobstr")
x <- 1:10; y <- x; z <- x
tracemem(x)

c(obj_addr(x), obj_addr(y), obj_addr(z))

rm(z)
c(obj_addr(x), obj_addr(y), obj_addr(z))

y[1] <- 3L
 

#exercise
#
clust <- new_cluster(3)
base_url <- "http://www2.stat.duke.edu/~sms185/data/bike/"
files <- c("cbs_2015.csv", "cbs_2016.csv", "cbs_2017.csv")
cluster_assign_partition(clust, file_name = paste0(base_url, files))
cluster_send(clust, cbs_data <- vroom::vroom(file_name))
cbs <- party_df(clust, "cbs_data")
