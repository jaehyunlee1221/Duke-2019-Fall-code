library(parallel)
link1 <- "http://bit.ly/nz-data"
link2 <- "http://www2.stat.duke.edu/~sms185/data/bike/2017q3.csv"
system.time(
  migration <- read.csv(link1)
)

system.time(
  migration2 <- readr::read_csv(link1)
)

system.time(
  migration <- data.table::fread(link1)
)

system.time(
  bike <- read.csv(link2)
)

system.time(
  bike2 <- readr::read_csv(link2)
)

system.time(
  bike3 <- data.table::fread(link2)
)

detectCores()
