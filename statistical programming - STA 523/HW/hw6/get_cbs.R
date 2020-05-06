library(vroom)
library(parallel)
cores <- detectCores()

url <- paste0("http://www2.stat.duke.edu/~sms185/data/bike/cbs_",2013:2017,".csv",sep = "")
bike<-list()
data.clust <- makeCluster(cores)
bike <- parLapply(data.clust, url, vroom)
stopCluster(data.clust)

#we use only 2017 because bike data of 2013~2016 does not have same number of station with 2018
#we could find that station number was increasing by year
saveRDS(bike[[5]], file = "cbs2017.rds")

test <- list()
test <- vroom("http://www2.stat.duke.edu/~sms185/data/bike/cbs_test.csv")

saveRDS(test, file = "test.rds")