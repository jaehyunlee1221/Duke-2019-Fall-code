library(tidyverse)

wawa = readRDS("data/wawa/wawa_data.rds")

get <- function(str, list_data) {
  unlist(sapply(list_data,function(x) {
    unlist(x)[str]
  }))
}

wawa_geo_df = bind_cols(
  state = get("addresses.state", wawa),
  zip = get("addresses.zip", wawa),
  latitude = as.numeric(get("addresses.loc1", wawa)),
  longitude = as.numeric(get("addresses.loc2", wawa)),
  has_fuel = as.logical(get("amenities.fuel", wawa))
)

saveRDS(wawa_geo_df, "data/wawa/wawa_geo_df.rds")