oil <- readr::read_table("http://users.stat.ufl.edu/~winner/data/oilimport.dat",
                         col_names = c("year", "month", "month_series", "barrels_purchased",
                                       "total_value", "unit_price", "cpi")
)

mydb <- dbConnect(RSQLite::SQLite(), "mydb.sqlite")
dbWriteTable(mydb2, "oil", oil)

nukes <- readr::read_table("http://users.stat.ufl.edu/~winner/data/nuketest.dat",
                           col_names = c("year", "month", "month_series", "tests"))
readr::write_csv(nukes, path = "nukes.csv")
