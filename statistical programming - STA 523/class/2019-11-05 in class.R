library(dbplyr)
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
copy_to(con, df = nycflights13::airports, "airports")
DBI::dbListTables(con)
#> [1] "airports"     "sqlite_stat1" "sqlite_stat4"
airports_db <- tbl(con, "airports")
airports_db

airport_car <- airports_db %>% 
  filter(lat >= 33.7666, lat <= 36.588, lon >= -84.3201, lon <= -75.4129) %>% 
  arrange(desc(alt)) %>% 
  select(name,alt)

airport_car %>% 
  show_query()
DBI::dbDisconnect(con)


library(RSQLite)
library(Lahman)
con <- dbConnect(RSQLite::SQLite(), ":memory:")

dbWriteTable(con, "batting", Batting)
dbWriteTable(con, "pitching", Pitching)
dbWriteTable(con, "teams", Teams)
dbWriteTable(con, "salaries",Salaries)

dbListTables(con) 
dbListFields(con, "teams") %>% head()
#> [1] "yearID"   "lgID"     "teamID"   "franchID" "divID"    "Rank"
dbListFields(con, "pitching")


dbListFields(con, "salaries") %>% head()

dbGetQuery(con, paste("SELECT teamID, SUM(salary) as salaries",
                      "FROM salaries",
                      "WHERE yearID == 2016",
                      "GROUP BY teamID",
                      "ORDER BY salaries DESC",
                      "LIMIT 5"))

dbGetQuery(con, paste("SELECT teamID, W,G, W/G as winpercent",
                      "FROM pitching",
                      "WHERE yearID >= 1990",
                      "GROUP BY teamID",
                      "ORDER BY winpercent DESC",
                      "LIMIT 10"))


dbGetQuery(con, paste("SELECT teamID, sum(W) as W, sum(G) as G, yearID",
                      "FROM pitching",
                      "GROUP BY teamID",
                      "LIMIT 10"))

pitching <- tbl(con,"pitching")           

pitching <- pitching %>% 
  select(teamID,W,G,yearID) %>% 
  filter(yearID>=1990) %>% 
  group_by(teamID) %>% 
  summarise(TW = sum(W), TG = sum(G))# %>% 
pitching %>% 
  mutate(winpercent = TW/TG)

pitching$`TW/TG`
  mutate(winpercent = TW/TG)
c(664,1)/c(4454,1)
