library(sf)
library(mapview)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

#Exercise
#Download sf data
download.file("https://opendata.arcgis.com/datasets/faaad7fcca8d4f67abdbb1bd4697f055_0.zip",
              destfile = "statistical programming/data/Gamelands.zip")
unzip("statistical programming/data/Gamelands.zip", exdir = "statistical programming/data/")
download.file("https://opendata.arcgis.com/datasets/faaad7fcca8d4f67abdbb1bd4697f055_0.zip",
              destfile = "statistical programming/data/Gamelands.zip")

nc_gamelands <- read_sf("statistical programming/data/Gamelands.shp")
nc_hazard_waste <- read_sf("statistical programming/data/Hazardous_Waste_sites.shp")

#EDA
st_crs(nc)
st_crs(nc_gamelands)
st_crs(nc_hazard_waste)

nc_mapview <- mapview(nc, alpha.regions = .2, alpha = .9,
                      label = nc[, "NAME", drop = T],
                      layer.name = "NC Counties")
nc_gamelands_mapview <- mapview(nc_gamelands, col.regions = "#ff6700",
                                label = round(nc_gamelands[, "SUM_ACRES", drop = T], 2),
                                layer.name = "NC Gamelands")
nc_hazard_waste_mapview <- mapview(nc_hazard_waste, col.regions = "#cf4300",
                                   label = nc_hazard_waste[, "SITE_NAME", drop = T],
                                   layer.name = "NC hazard waste site")

nc_mapview + nc_gamelands_mapview + nc_hazard_waste_mapview

##Manipulate data
nc <- st_transform(nc, st_crs(32119))
nc_gamelands <- st_transform(nc_gamelands, st_crs(32119))

durham_county <- nc %>% 
  dplyr::filter(as.character(NAME) == "Durham")

str(nc)
nc[durham_county, ]




## Exercise solution
waste <- st_read("statistical programming/data/Hazardous_Waste_Sites.shp", quiet = T)
waste <- st_transform(waste, st_crs(nc_gamelands))
close_waste_lgl <- st_is_within_distance(waste, nc_gamelands, 
                                         dist = 100, sparse = F)
close_waste <- waste %>% 
  dplyr::filter(apply(close_waste_lgl, 1, sum) > 0)
typeof(close_waste_lgl)
nc_gamelands_mapview <- mapview(nc_gamelands, col.regions = "#ff6700",
                                label = nc_gamelands$GML_HAB,
                                layer.name = "NC Gamelands")
nc_waste_mapview <- mapview(close_waste, col.regions = "#65ff00",
                            alpha = .3,
                            alpha.regions = .3,
                            label = waste[, "SITE_NAME", drop = T],
                            layer.name = "NC Waste Sites")
nc_gamelands_mapview + nc_waste_mapview
ggplot(nc) +
  geom_sf(alpha = .3) +
  geom_sf(data = close_waste, color = "#65ff00", size = 3) +
  geom_sf(data = nc_gamelands, fill = "#ff6700", alpha = .5) +
  theme_bw()
