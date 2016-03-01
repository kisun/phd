setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))




loadall()
days <- dbGetQuery(dbConnect(SQLite(), "db/backups/gtfs-history_201602230919.db"),
                   "SELECT DISTINCT timestamp FROM vehicle_positions WHERE route_id LIKE '721%'")
days <- unique(tsDate(days$timestamp))

## static Date:
static.latest <- gsub("gtfs_", "", system("readlink _data/gtfs-latest", intern = TRUE))
valid.days <- days[which(days == static.latest):length(days)]
#collectHistory(route = "798", day = valid.days[1])

loadall()
lapply(valid.days, function(x) collectHistory(route = "721", day = x))






routeid <- "721%"
hist881 <- dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
                      sprintf("SELECT * FROM history WHERE route_id LIKE '%s'",
                              routeid))
hist881$time.day <- hist881$timestamp - 
    as.numeric(format(as.POSIXct(paste(hist881$trip_start_date, 
                                       "00:00:00")), 
                      format = "%s"))
hist881$time.hour <- hist881$time.day / 60 / 60
hist881$dvt <- as.factor(paste(hist881$trip_start_date, 
                               hist881$trip_id, 
                               hist881$vehicle_id, 
                               sep = ":"))
which.keep <- 
    do.call(c,
            invisible(tapply(1:nrow(hist881), hist881$dvt, function(i) {
                d <- diff(range(hist881$time.day[i]))
                dt <- diff(hist881$time.day[i])
                dx <- diff(hist881$distance[i])
                if (max(dx/dt) < 50) return(i) else numeric()
            })))
hist881 <- hist881[which.keep, ]
with(hist881, plot(time.hour, distance, type = "n",
                   main = "History of Outer Link", xlab = "Time (h)", 
                   ylab = "Distance into Trip (m)"))
invisible(tapply(1:nrow(hist881), hist881$dvt, 
                 function(i) lines(hist881$time.hour[i], hist881$distance[i])))
