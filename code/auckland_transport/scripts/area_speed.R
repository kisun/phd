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

## loadall()
## collectHistory(route = "090", day = "2016-02-19", verbose = FALSE)


## dates <- c("2016-02-20", "2016-02-21", "2016-02-22", "2016-02-23", "2016-02-24", "2016-02-25",
##            "2016-02-26", "2016-02-27", "2016-02-28", "2016-02-29", "2016-01-01", "2016-01-02",
##            "2016-01-03", "2016-01-04", "2016-01-05", "2016-01-06", "2016-01-07", "2016-01-08")
## routes <- c("090", "080", "070")

## for (route in routes) {
##     for (date in dates) {
##         try(collectHistory(route = route, day = date))
##     }
## }

area <- dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
                   "SELECT * FROM history")
## fix mess up
swap <- area$position_longitude < 0
pl <- area$position_longitude
area$position_longitude[swap] <- area$position_latitude[swap]
area$position_latitude[swap] <- pl[swap]
area$velocity <- pmin(30, pmax(0, area$velocity))

box <- c(174.58, 174.78, -36.8, -36.91)
area.subset <- area[area$position_longitude > box[1] &
                        area$position_longitude < box[2] &
                            area$position_latitude < box[3] &
                                area$position_latitude > box[4], ]
obj <- iNZightMap(~position_latitude, ~position_longitude, data = area.subset)
plot(obj, colby = velocity, cex.pt = 0.2, alpha = 0.2, col.fun = diverge_hcl)

