## Just because I'm familiar with it ... we'll investigate historical patterns
## in the 090 route.

setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))




loadall()
## days <- dbGetQuery(dbConnect(SQLite(), "db/backups/gtfs-history_201602230919.db"),
##                    "SELECT DISTINCT timestamp FROM vehicle_positions")
## days <- unique(tsDate(days$timestamp))

## ## static Date:
## static.latest <- gsub("gtfs_", "", system("readlink _data/gtfs-latest", intern = TRUE))
## valid.days <- days[which(days == static.latest):length(days)]
### [1] "2016-02-18" "2016-02-19" "2016-02-20" "2016-02-21" "2016-02-22"
### [6] "2016-02-23"
collectHistory(day = DAY)




## ## tr <- as.numeric(format(as.POSIXct(days[1]), format = "%s")) + c(0, 60 * 60 * 24)
## allroutes <- unique(dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
##                                "SELECT DISTINCT route_id FROM history")$route_i
d)

## ##for (r in allroutes) {

## r <- allroutes[1]
## HIST <- dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
##                    ## "SELECT * FROM history WHERE route_id LIKE '09002%'")
##                    sprintf("SELECT * FROM history WHERE route_id='%s'", r))



## HIST$time.day <-
##     HIST$timestamp - as.numeric(format(as.POSIXct(paste(HIST$trip_start_date, "00:00:00")), format = "%s"))
## HIST$time.hour <- HIST$time.day / 60 / 60
## with(HIST, plot(time.hour, distance, type = "n", main = r))

## HIST$dvt <- as.factor(paste(HIST$trip_start_date, HIST$trip_id, HIST$vehicle_id, sep = ":"))
## tapply(1:nrow(HIST), HIST$dvt, function(i) lines(HIST$time.hour[i], HIST$distance[i]))

## ## Clean history:
## ## which.keep <- 
## ##     tapply(1:nrow(HIST), HIST$dvt, function(i) {
## ##         with(HIST, plot(time.hour, distance, type = "n"))
## ##         lines(HIST$time.hour[i], HIST$distance[i])
## ##         keep <- readline("Keep this entry? (Y/n) ")
## ##         if (keep %in% c("", "Y", "y")) which.keep <<- c(which.keep, i)
## ##     })

## ## visually, say max speed of 110km/h = 100 * 1000 / 60 / 60 = 28 m/s
## which.keep <- 
##     do.call(c,
##             invisible(tapply(1:nrow(HIST), HIST$dvt, function(i) {
##                 d <- diff(range(HIST$time.day[i]))
##                 ## max speed
##                 dt <- diff(HIST$time.day[i])
##                 dx <- diff(HIST$distance[i])
##                 if (max(dx/dt) < 50) return(i) else numeric()
##             })))
## KEEP <- HIST[which.keep, ]

## with(KEEP, plot(time.hour, distance, type = "n",
##                 main = r,
##                 ## xlim = c(6.2, 8), ylim = c(0, 10000),
##                 xlab = "Time (h)", ylab = "Distance into Trip (m)"))
## KEEP$dvt <- as.factor(paste(KEEP$trip_start_date, KEEP$trip_id, KEEP$vehicle_id, sep = ":"))
##     invisible(tapply(1:nrow(KEEP), KEEP$dvt, function(i) {
##         lines(KEEP$time.hour[i], KEEP$distance[i], col = "#00000040")
##         points(KEEP$time.hour[i], KEEP$distance[i], cex = 0.3, pch = 10)
##     }))
## ## stop locations:
## stops <- vehicle$new("1", c(1, 1), trip = tail(KEEP$trip_id, 1))$getSchedule()
##     abline(h = stops$distance_into_trip, lty = 3, col = "gray50")

## diffs <- invisible(tapply(1:nrow(KEEP), KEEP$dvt, function(i) {
##     diff(KEEP$time.day[i])
## }))
## diffs <- do.call(c, diffs)
## hist(diffs[diffs < 2 * 60])

## pings <- KEEP$distance
## pings <- round(pings / 10) * 10
## hist(pings, 1000)
## plot(table(pings))

## ##grid::grid.locator()
## ##}


## #### Need to fix Bayesian models to the full data once it's obtained:

## loadall()
## collectHistory2 <- function(route, data.clean = list(), hist.db = dbConnect(SQLite(), "db/historical-data.db")) {
    
##     ## route <- "09001"
    
##     con <- dbConnect(SQLite(), "db/gtfs-history.db")
##     positions <- getPositions(con, route.id = route)
##     table(positions$route_id)  ## version number has updated! 
##     table(positions$trip_id)
    
##     ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
##     ##                      name = "Auckland Busses")
##     ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    
    
    
    
##     ## filter points
##     ## loadall()
    
##     ## "started" = logical, has the trip started? (according to the schedule)
##     positions$started <- positions$timestamp -
##         as.numeric(format(as.POSIXct(paste(positions$trip_start_date, positions$trip_start_time)), format = "%s")) >= 0
    
##     dat <- positions ## [positions$started, ]
    
##     ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = dat,
##     ##                      name = "Historical Route 090")
##     ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
##     ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")#, g1 = trip_start_date, g2 = trip_id)#, g2.level = "_MULTI")
    
##     ## loadall()
    
    
    
##     for (day in unique(dat$trip_start_date)) {
        
##         ## day <- unique(dat$trip_start_date)[1]
##         cat("\n\n========================== Processing history date:", day, "\n") 
        
##         tmp <- dat[dat$trip_start_date == day, ]
        
##         trips <- unique(tmp$trip_id)
##         #if (day %in% names(data.clean))
##         #    trips <- trips[!trips %in% data.clean[[day]]]
##         trips.schedule <- lapply(trips, getSchedule, verbose = FALSE)
##         names(trips.schedule) <- trips
##         trips.start <- sapply(trips.schedule, function(x) {
##                                   t <- x$departure_time[1]
##                                   as.numeric(format(as.POSIXct(paste(day, t)), format = "%s"))
##                               })
##         trips.order <- order(trips.start)
        
##         for (trip in trips[trips.order]) {
            
##             ## trip <- trips[trips.order][7]
            
##             cat("============ Processing history trip:", trip, "\n")
##             tmp2 <- tmp[tmp$trip_id == trip, ]

##             trip.map <- iNZightMap(~position_latitude, ~position_longitude, data = tmp2,
##                                    name = "Historical Route 090")
##             ## do.call(plot, list(x = trip.map, pch = 4, cex.pt = 0.6, col.pt = "#cc0000"))
            
##             shape <- getPattern(trip, verbose = FALSE)
##             addLines(shape$shape_pt_lon, shape$shape_pt_lat)

##             if (length(unique(tmp2$vehicle_id)) > 1)
##                 cat("Multiple matches ... using the first.\n")
##             ## cat("Processing vehicles:", paste(unique(tmp2$vehicle_id), collapse = ", "), "\n")
            
##             vid <- unique(tmp2$vehicle_id)[1]
            
##             ## check to see if this day/trip is already in the database
##             if ("history" %in% dbListTables(hist.db)) {
##                 if (dbGetQuery(hist.db,
##                                sprintf("SELECT count(*) AS count FROM history WHERE trip_start_date='%s' AND trip_id='%s'",
##                                        day, trip))$count > 0) {
##                     next
##                 }
##             }
            
##             tmp3 <- tmp2[tmp2$vehicle_id == vid, ]
##             t.start <- as.numeric(format(as.POSIXct(paste(tmp3$trip_start_date[1],
##                                                           tmp3$trip_start_time[1])),
##                                          format = "%s"))

##             shape <- getPattern(trip)
##             p1 <- t(shape[-nrow(shape), c("shape_pt_lon", "shape_pt_lat")])
##             p2 <- t(shape[-1, c("shape_pt_lon", "shape_pt_lat")])
##             shape$length <- c(distanceFlat(p1, p2), 0)
##             shape$distance_into_pattern <- c(0, cumsum(shape$length[-nrow(shape)]))
##             shape$bearing <- c(bearing(p1, p2), 0)
            
##             data <- list(pos   = as.matrix(tmp3[, c("position_latitude", "position_longitude")]),
##                          t     = tmp3$timestamp - t.start,
##                          shape = shape,
##                          N     = nrow(tmp3))

##             n.iter <- 1000
##             pars <- list(x = matrix(NA, n.iter + 1, data$N))
            
##             ## Initialise
##             ## pars$x[1, ] <- rep(max(data$shape$distance_into_pattern), data$N)
##             pars$x[1, ] <- sort(runif(data$N, 0, max(data$shape$distance_into_pattern)))
##             ## plot(data$t, pars$x[1, ], ylim = range(data$shape$distance_into_pattern))

##             ## Likelihood function:
##             lhood <- function(x) {
##                 r <- sapply(x, h, shape = data$shape)
##                 d <- distanceFlat(r, t(data$pos))
##                 sum(dnorm(d, 0, 20, log = TRUE))
##             }
            
##             for (i in 1:n.iter + 1) {
##                 x.p <- pars$x[i - 1, ]
                
##                 for (j in 1:data$N) {
##                     if (j == 1)
##                         x.pj <- runif(200, 0, x.p[2])
##                     else if (j == data$N)
##                         x.pj <- runif(200, x.p[j - 1], max(shape$distance_into_pattern))
##                     else
##                         x.pj <- runif(200, x.p[j - 1], x.p[j + 1])
                    
##                     Rhat <- sapply(x.pj, h, shape = data$shape)
##                     Dhat <- distanceFlat(as.numeric(data$pos[j, ]), Rhat)
##                     tmp <- x.p
##                     tmp[j] <- x.pj[which(Dhat == min(Dhat))[1]]
##                     lr <- lhood(tmp) - lhood(x.p)
                    
##                     if (runif(1) < exp(lr)) x.p[j] <- x.pj
##                 }

##                 ## plot(data$t, pars$x[i - 1, ], ylim = range(data$shape$distance_into_pattern))
##                 ## points(data$t, x.p, col = "red", pch = 4)
##                 mm <- iNZightMap(~position_latitude, ~position_longitude, data = tmp3,
##                                  name = "Historical Route 090")
##                 ## plot(mm, pch = 4, cex.pt = 0.6, col.pt = "#cc0000")
##                 xy <- sapply(x.p, h, shape = data$shape)
##                 addPoints(xy[2, ], xy[1, ])
                
##                 pars$x[i, ] <- x.p
##             }
##         }
##     }
## }


## loadall()
## collectHistory2("09001")
