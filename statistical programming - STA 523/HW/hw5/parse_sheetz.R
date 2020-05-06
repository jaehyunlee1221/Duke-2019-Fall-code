library(tidyverse)

sheetz = readRDS("data/sheetz/sheetz_data.rds")

get <- function(str, list_data) {
  unlist(sapply(list_data,function(x) {
    unlist(x)[str]
  }))
}

sheetz_geo_df = bind_cols(
  state = get("state", sheetz),
  zip = get("zip", sheetz),
  latitude = as.numeric(get("latitude", sheetz)),
  longitude = as.numeric(get("longitude", sheetz)),
  diesel = as.logical(get("diesel", sheetz)),
  kerosene = as.logical(get("kerosene", sheetz))
) %>%
  slice(1:277) %>% 
  mutate(has_fuel = diesel | kerosene)

saveRDS(sheetz_geo_df, "data/sheetz/sheetz_geo_df.rds")