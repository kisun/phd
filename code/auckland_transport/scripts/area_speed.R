## Here we attempt to obtain speed information for an area.
## Start off with 07X, 08X, and 09X busses.


setwd("~/Documents/uni/phd/code/auckland_transport")
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))
loadall()

## CON <- dbConnect(SQLite(), "db/backups/gtfs-history_latest.db")
## positions070 <- getPositions(CON, route.id = "070")
## positions080 <- getPositions(CON, route.id = "080")
## positions090 <- getPositions(CON, route.id = "090")
## positions <- rbind(positions070, positions080, positions090)

loadall()
collectHistory(route = "090", day = "2016-02-19", verbose = FALSE)


dates <- c("2016-02-20", "2016-02-21", "2016-02-22", "2016-02-23", "2016-02-24", "2016-02-25",
           "2016-02-26", "2016-02-27", "2016-02-28", "2016-02-29", "2016-01-01", "2016-01-02",
           "2016-01-03", "2016-01-04", "2016-01-05", "2016-01-06", "2016-01-07", "2016-01-08")
routes <- c("090", "080", "070")

for (route in routes) {
    for (date in dates) {
        collectHistory(route = route, day = date)
    }
}
