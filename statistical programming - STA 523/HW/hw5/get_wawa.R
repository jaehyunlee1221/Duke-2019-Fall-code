# Get Wawa store data.

library(jsonlite)
library(httr)

base_url = "http://www2.stat.duke.edu/~sms185/data/fuel/bystore/awaw/awawstore="
# Store identifier that has 5 digit form
digits = c(formatC(0:1000, width = 5, flag="0"),
           formatC(8000:9000, width = 5, flag="0")
           )

data = list()
for (digit in digits) {
  url = paste0(base_url, digit, ".json")
  
  if(HEAD(url)$status_code != 404) {
    data = append(data, list(read_json(url)))
  }
  
  Sys.sleep(0.01 + rexp(1, 100))
}

saveRDS(data, "data/wawa/wawa_data.rds")

