library(jsonlite)
library(dplyr)
library(stringr)
library(rvest)

API_key <- "eeb903c418d19b22e9e73c34cf79e673"

# D.C. location
latitude <- "38.9072"
longitude <- "-77.0369"

#
time <- "YYYY-MM-DDT12:00:00"

base_url <- paste0("https://api.darksky.net/forecast/", API_key, "/",latitude,",", longitude, ",",time)

Date <- as.Date("2017-01-01",format="%Y-%m-%d")

weather_data <- NULL

# Year 2017
for (i in 1:365){
  url <- base_url %>%
    str_replace("YYYY-MM-DD", format(Date, "%Y-%m-%d"))
  Date <- Date + 1
  weather_data[[i]] <-read_json(url)
}

darksky_data <- bind_rows(sapply(weather_data, function(x) x$daily$data))

darksky_data$time <- as.POSIXct(darksky_data$time,origin = "1970-01-01",tz = "GMT")
darksky_data$sunriseTime <- as.POSIXct(darksky_data$sunriseTime, origin = "1970-01-01",tz = "GMT")
darksky_data$sunsetTime <- as.POSIXct(darksky_data$sunsetTime, origin = "1970-01-01",tz = "GMT")
darksky_data$temperatureHighTime <- as.POSIXct(darksky_data$temperatureHighTime,origin = "1970-01-01",tz = "GMT")
darksky_data$temperatureLowTime <- as.POSIXct(darksky_data$temperatureLowTime,origin = "1970-01-01",tz = "GMT")
darksky_data$apparentTemperatureHighTime <- as.POSIXct(darksky_data$apparentTemperatureHighTime,origin = "1970-01-01",tz = "GMT")
darksky_data$apparentTemperatureLowTime <- as.POSIXct(darksky_data$apparentTemperatureLowTime,origin = "1970-01-01",tz = "GMT")
darksky_data$windGustTime <- as.POSIXct(darksky_data$windGustTime,origin = "1970-01-01",tz = "GMT")
darksky_data$temperatureMinTime <- as.POSIXct(darksky_data$temperatureMinTime,origin = "1970-01-01",tz = "GMT")
darksky_data$temperatureMaxTime <- as.POSIXct(darksky_data$temperatureMaxTime,origin = "1970-01-01",tz = "GMT")
darksky_data$apparentTemperatureMinTime <- as.POSIXct(darksky_data$apparentTemperatureMinTime,origin = "1970-01-01",tz = "GMT")
darksky_data$apparentTemperatureMaxTime <- as.POSIXct(darksky_data$precipIntensityMaxTime,origin = "1970-01-01",tz = "GMT")
darksky_data$precipIntensityMaxTime <- as.POSIXct(darksky_data$precipIntensityMaxTime,origin = "1970-01-01",tz = "GMT")

# Year 2018
time <- "YYYY-MM-DDT01:00:00-0500"

base_url <- paste0("https://api.darksky.net/forecast/", API_key, "/",latitude,",", longitude, ",",time)

Date <- as.Date("2018-01-01",format="%Y-%m-%d")

weather_data2 <- NULL

for (i in 1:365){
  url <- base_url %>%
    str_replace("YYYY-MM-DD", format(Date, "%Y-%m-%d"))
  Date <- Date + 1
  weather_data2[[i]] <-read_json(url)
}

darksky_data2 <- bind_rows(sapply(weather_data2, function(x) x$daily$data))

darksky_data2$time <- as.POSIXct(darksky_data2$time,origin = "1970-01-01",tz = "GMT")
darksky_data2$sunriseTime <- as.POSIXct(darksky_data2$sunriseTime, origin = "1970-01-01",tz = "GMT")
darksky_data2$sunsetTime <- as.POSIXct(darksky_data2$sunsetTime, origin = "1970-01-01",tz = "GMT")
darksky_data2$temperatureHighTime <- as.POSIXct(darksky_data2$temperatureHighTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$temperatureLowTime <- as.POSIXct(darksky_data2$temperatureLowTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$apparentTemperatureHighTime <- as.POSIXct(darksky_data2$apparentTemperatureHighTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$apparentTemperatureLowTime <- as.POSIXct(darksky_data2$apparentTemperatureLowTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$windGustTime <- as.POSIXct(darksky_data2$windGustTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$temperatureMinTime <- as.POSIXct(darksky_data2$temperatureMinTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$temperatureMaxTime <- as.POSIXct(darksky_data2$temperatureMaxTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$apparentTemperatureMinTime <- as.POSIXct(darksky_data2$apparentTemperatureMinTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$apparentTemperatureMaxTime <- as.POSIXct(darksky_data2$precipIntensityMaxTime,origin = "1970-01-01",tz = "GMT")
darksky_data2$precipIntensityMaxTime <- as.POSIXct(darksky_data2$precipIntensityMaxTime,origin = "1970-01-01",tz = "GMT")


#Save results
saveRDS(darksky_data2, "darksky2.rds")
saveRDS(darksky_data,"darksky.rds")
