#model 2

library(nnet)
#training data
cbs_2017 <- bike %>% 
  dplyr::select(Duration, `Start date`, `Start station`, `End station`, `Member type`) %>% 
  separate(col = `Start date`, into = paste("Start",c("year","month","day"), sep = "_")) %>% 
  filter(`Start station` %in% unique(test$start_station))

weather <- weather %>% 
  dplyr::select(time, precipIntensity, temperatureHigh, dewPoint, humidity, pressure, windSpeed, windGust, cloudCover, uvIndex) %>% 
  separate(col = time, into = c("year","month","day"))

full_data <- left_join(cbs_2017, weather, by = c("Start_year" = "year","Start_month" = "month","Start_day" = "day"))


test_2018 <- test %>% 
  dplyr::select(duration, start_date, start_station, member_type) %>% 
  separate(col = `start_date`, into = paste("Start",c("year","month","day"), sep = "_")) %>% 
  `colnames<-`(c("Duration", "Start_year", "Start_month", "Start_day", "Start station", "Member type"))

weather2 <- weather2 %>% 
  dplyr::select(time, precipIntensity, temperatureHigh, dewPoint, humidity, pressure, windSpeed, windGust, cloudCover, uvIndex)  %>% 
  separate(col = time, into = c("year","month","day"))

full_test <- left_join(test_2018, weather2, by = c("Start_year" = "year","Start_month" = "month","Start_day" = "day")) 

newbike <- lapply(unique(cbs_2017$`Start station`), function(x){full_data %>% filter(`Start station` == x)})
models <- rep(list(0),length(newbike)) 

fit.multi <- function(data,test,distance.matrix) {
  library(nnet)
  library(dplyr)
  library(stringr)
  library(MASS)
  if(sum(colnames(distance.matrix) == unique(data$`Start station`))==0){
    model <- multinom(`End station` ~ Duration + `Member type` + precipIntensity + temperatureHigh + dewPoint + humidity + pressure + windSpeed + windGust + cloudCover + uvIndex, data = data, MaxNWts = 10000, maxit = 10 )
  } else{
    distance.temp <- cbind.data.frame(rownames(distance.matrix),distance.matrix[,colnames(distance.matrix) == unique(data$`Start station`)]) %>% `colnames<-`(c("End station","distance")) %>% arrange(distance)
    distance.temp$`End station` <- str_remove_all(as.character(distance.temp$`End station`),"\\s$")
    distance.temp$rank <- 1:nrow(distance.temp)
    distance <- left_join(data, distance.temp, by = c("End station" = "End station"))
    distance$rank[is.na(distance$rank)] <- "others"
    distance$rank <- as.factor(distance$rank)
    
    model <- polr(rank ~ Duration + `Member type` + precipIntensity + temperatureHigh + dewPoint + humidity + pressure + windSpeed + windGust + cloudCover + uvIndex, data = distance)
  }
  
  test_set <- test %>% filter(`Start station` == unique(data$`Start station`))
  
  pred <- predict(model, test_set, type = "probs")
  
  if(sum(colnames(distance.matrix) == unique(data$`Start station`))==0){
    result <- cbind(test_set,pred)
  } else if(dim(test_set)[1] == 1) {
    names(pred) <- distance$`End station`[match(names(pred),distance$rank)]
    result <- cbind.data.frame(c(test_set,pred))
  } else{
    colnames(pred) <- distance$`End station`[match(colnames(pred), distance$rank)]
    result <- cbind(test_set,pred)
  }
  return(result)
}

library(parallel)
cores <- detectCores()
data.clust <- makeCluster(cores)
system.time(models <- parLapply(data.clust, newbike, fit.multi, test = full_test, distance.matrix = distance.matrix))
stopCluster(data.clust)

result <- bind_rows(models)
remaincols <- colnames(test)[!(colnames(test) %in% colnames(result))]
result[,remaincols] <- 0
result[is.na(result)] <- 0
result <- result %>% 
  unite("start_date", sep = "-", Start_year:Start_day)
pred_dist <- result %>% dplyr::select(contains(" ")) %>% dplyr::select(-c(`Start station`,`Member type`))
pred_dist <- pred_dist %>% dplyr::select(colnames(test %>% dplyr::select(contains(" "))))

temp_result <- cbind.data.frame(result[,c("start_date","Duration","Start station", "Member type")],pred_dist)
temp_result2 <- test %>% select(start_date,end_date,duration,start_station_number,start_station, bike_number, member_type) %>% separate(start_date, into = c("start_date","start_time"), sep = " ")
final_result <- left_join(temp_result2, temp_result, by = c("start_date" = "start_date","duration" = "Duration", "start_station" = "Start station", "member_type" = "Member type")) %>% unite("start_date",c("start_date","start_time"), sep =" ")
final_result <- final_result[-which(duplicated(final_result)),]
final_result <- final_result %>% mutate(start_date = as.POSIXct(start_date))
write.csv(final_result, "cbs_byte-me.csv", row.names = F)
