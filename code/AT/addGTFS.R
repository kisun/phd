## Because the gtfsdb-load function isn't working ...

library(RSQLite)
con <- dbConnect(SQLite(), "gtfs-static.db")

tables <- dbListTables(con)
files <- gsub(".txt", "", list.files("gtfs"))
available <- tables %in% files

for (table in tables[available]) {
  f <- read.csv(paste0("gtfs/", table, ".txt"), header = TRUE)
  wrt <- dbWriteTable(con, value = f, name = table, append = FALSE, overwrite = TRUE)

  if (wrt)
    cat("Written data to table", table, "\n")
  else
    stop("Unable to save table ", table)
}
