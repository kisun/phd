## Just because I'm familiar with it ... we'll investigate historical patterns
## in the 090 route.

setwd("~/Documents/uni/phd/code/auckland_transport")

loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))

loadall()
collectHistory <- function(route, data.clean = list(), hist.db = dbConnect(SQLite(), "db/historical-data.db")) {
    route = "09001"
    con <- dbConnect(SQLite(), "db/gtfs-history.db")
    positions <- getPositions(con, route.id = route)
    table(positions$route_id)  ## version number has updated! 
    table(positions$trip_id)
    
    ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
    ##                      name = "Auckland Busses")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    
    
    
    
    ## filter points
    ## loadall()
    
    ## "started" = logical, has the trip started? (according to the schedule)
    positions$started <- positions$timestamp -
        as.numeric(format(as.POSIXct(paste(positions$trip_start_date, positions$trip_start_time)), format = "%s")) >= 0
    
    dat <- positions[positions$started, ]
    
    ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = dat,
    ##                      name = "Historical Route 090")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")#, g1 = trip_start_date, g2 = trip_id)#, g2.level = "_MULTI")
    
    ## loadall()
    
    
    
    for (day in unique(dat$trip_start_date)) {
        
        ## day <- unique(dat$trip_start_date)[1]
        cat("\n\n========================== Processing history date:", day, "\n") 
        
        tmp <- dat[dat$trip_start_date == day, ]
        
        trips <- unique(tmp$trip_id)
        if (day %in% names(data.clean))
            trips <- trips[!trips %in% data.clean[[day]]]
        trips.schedule <- lapply(trips, getSchedule, verbose = FALSE)
        names(trips.schedule) <- trips
        trips.start <- sapply(trips.schedule, function(x) {
                                  t <- x$departure_time[1]
                                  as.numeric(format(as.POSIXct(paste(day, t)), format = "%s"))
                              })
        trips.order <- order(trips.start)
        
        for (trip in trips[trips.order]) {
            
            ## trip <- trips[trips.order][6]
            
            cat("============ Processing history trip:", trip, "\n")
            tmp2 <- tmp[tmp$trip_id == trip, ]

            ## trip.map <- iNZightMap(~position_latitude, ~position_longitude, data = tmp2,
            ##                        name = "Historical Route 090")
            ## do.call(plot, list(x = trip.map, pch = 4, cex.pt = 0.6, col.pt = "#cc0000"))
            
            shape <- getPattern(trip, verbose = FALSE)
            ## addLines(shape$shape_pt_lon, shape$shape_pt_lat)

            use <- "" ## readline("Use this trip? (Y/n)")
            if (use %in% c("", "Y", "y")) {
                if (length(unique(tmp2$vehicle_id)) > 1)
                    cat("Multiple matches ... using the first.\n")
                ## cat("Processing vehicles:", paste(unique(tmp2$vehicle_id), collapse = ", "), "\n")
                
                #for (vid in unique(tmp2$vehicle_id)) {
                vid <- unique(tmp2$vehicle_id)[1]
                
                ## check to see if this day/trip is already in the database
                if ("history" %in% dbListTables(hist.db)) {
                    if (dbGetQuery(hist.db,
                                   sprintf("SELECT count(*) AS count FROM history WHERE trip_start_date='%s' AND trip_id='%s'",
                                           day, trip))$count > 0) {
                        next
                    }
                }
                
                ## vid <- unique(tmp2$vehicle_id)[1]
                tmp3 <- tmp2[tmp2$vehicle_id == vid, ]
                
                v <- vehicle$new(vid, tmp3[1, c("position_latitude", "position_longitude", "timestamp")], trip)
                v$update()## $plot()
                
                pb <- txtProgressBar(1, nrow(tmp3), style = 3)
                for (i in 2:nrow(tmp3)) {
                    v$update(tmp3[i, ])
                    ## if (i %% 5 == 0) v$plot()
                    ## v$plot();grid::grid.locator()
                    setTxtProgressBar(pb, i)
                }
                close(pb)
                
                
                try({
                    
                    out <- data.frame(cbind(
                        tmp3[, c("trip_id", "vehicle_id", "route_id", "timestamp", "trip_start_date", "trip_start_time")],
                        t(apply(v$getParticles()$xhat, 3, function(x) apply(x, 1, median, na.rm = TRUE))[, -1])
                        ))
                    
                    ## only grab the positive distances:
                    out <- out[out$distance >= 0, ]
                    
                    ## pick out the top points
                    SCHED <- v$getSchedule()
                    MAX <- which(out$distance == max(out$distance))
                    MAX <- MAX[c(1, which(diff(MAX) > 1) + 1)]
                    
                    ## pick ou the bottom points
                    MIN <- which(out$distance == min(out$distance))
                    if (length(MIN) == 1) {
                        MAX <- MAX[MAX > MIN][1]
                        out <- out[MIN:MAX, ]
                    }

                    print(range(out$distance))
                    
                    
                    out$trip.timestamp <-
                        out$timestamp - as.numeric(format(as.POSIXct(paste(tmp3$trip_start_date[1],
                                                                           tmp3$trip_start_time[1])),
                                                          format = "%s"))
                    plot(distance_into_trip ~ time, data = SCHED)
                    with(out, lines(trip.timestamp, distance))
                    
                    keep <- readline("Keep this record? (Y/n) ")
                    if (keep %in% c("", "Y", "y")) {
                        
                        res <- dbWriteTable(dbConnect(SQLite(), "db/historical-data.db"),
                                            "history", out, append = TRUE)

                        if (!res) stop("Unable to write to database ...")
                    }                    
                    
                    
                }, silent = TRUE)
                
            } else cat("Skipping ...\n")
        }
    }
}


loadall()
collectHistory("09001")




HIST <- dbReadTable(dbConnect(SQLite(), "db/historical-data.db"), "historical")

HIST$time.day <-
    HIST$timestamp - as.numeric(format(as.POSIXct(paste(HIST$trip_start_date, "00:00:00")), format = "%s"))
HIST$time.hour <- HIST$time.day / 60 / 60
with(HIST, plot(time.hour, distance, type = "n"))

HIST$dvt <- as.factor(paste(HIST$trip_start_date, HIST$trip_id, HIST$vehicle_id, sep = ":"))
tapply(1:nrow(HIST), HIST$dvt, function(i) lines(HIST$time.hour[i], HIST$distance[i]))

## Clean history:
which.keep <- character()
tapply(1:nrow(HIST), HIST$dvt, function(i) {
           with(HIST, plot(time.hour, distance, type = "n"))
           lines(HIST$time.hour[i], HIST$distance[i])
           keep <- readline("Keep this entry? (Y/n) ")
           if (keep %in% c("", "Y", "y")) which.keep <<- c(which.keep, i)
       })

nrow(HIST)
KEEP <- HIST[which.keep, ]
nrow(KEEP)

KEEP$dvt <- as.factor(paste(KEEP$trip_start_date, KEEP$trip_id, KEEP$vehicle_id, sep = ":"))
tapply(1:nrow(KEEP), KEEP$dvt, function(i) lines(KEEP$time.hour[i], KEEP$distance[i]))









#### Need to fix Bayesian models to the full data once it's obtained:

loadall()
collectHistory2 <- function(route, data.clean = list(), hist.db = dbConnect(SQLite(), "db/historical-data.db")) {
    
    ## route <- "09001"
    
    con <- dbConnect(SQLite(), "db/gtfs-history.db")
    positions <- getPositions(con, route.id = route)
    table(positions$route_id)  ## version number has updated! 
    table(positions$trip_id)
    
    ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = positions,
    ##                      name = "Auckland Busses")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    
    
    
    
    ## filter points
    ## loadall()
    
    ## "started" = logical, has the trip started? (according to the schedule)
    positions$started <- positions$timestamp -
        as.numeric(format(as.POSIXct(paste(positions$trip_start_date, positions$trip_start_time)), format = "%s")) >= 0
    
    dat <- positions ## [positions$started, ]
    
    ## map090 <- iNZightMap(~position_latitude, ~position_longitude, data = dat,
    ##                      name = "Historical Route 090")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")
    ## plot(map090, pch = 19, cex.pt = 0.1, col.pt = "#00000040")#, g1 = trip_start_date, g2 = trip_id)#, g2.level = "_MULTI")
    
    ## loadall()
    
    
    
    for (day in unique(dat$trip_start_date)) {
        
        ## day <- unique(dat$trip_start_date)[1]
        cat("\n\n========================== Processing history date:", day, "\n") 
        
        tmp <- dat[dat$trip_start_date == day, ]
        
        trips <- unique(tmp$trip_id)
        if (day %in% names(data.clean))
            trips <- trips[!trips %in% data.clean[[day]]]
        trips.schedule <- lapply(trips, getSchedule, verbose = FALSE)
        names(trips.schedule) <- trips
        trips.start <- sapply(trips.schedule, function(x) {
                                  t <- x$departure_time[1]
                                  as.numeric(format(as.POSIXct(paste(day, t)), format = "%s"))
                              })
        trips.order <- order(trips.start)
        
        for (trip in trips[trips.order]) {
            
            ## trip <- trips[trips.order][7]
            
            cat("============ Processing history trip:", trip, "\n")
            tmp2 <- tmp[tmp$trip_id == trip, ]

            trip.map <- iNZightMap(~position_latitude, ~position_longitude, data = tmp2,
                                   name = "Historical Route 090")
            do.call(plot, list(x = trip.map, pch = 4, cex.pt = 0.6, col.pt = "#cc0000"))
            
            shape <- getPattern(trip, verbose = FALSE)
            addLines(shape$shape_pt_lon, shape$shape_pt_lat)

            if (length(unique(tmp2$vehicle_id)) > 1)
                cat("Multiple matches ... using the first.\n")
            ## cat("Processing vehicles:", paste(unique(tmp2$vehicle_id), collapse = ", "), "\n")
            
            vid <- unique(tmp2$vehicle_id)[1]
            
            ## check to see if this day/trip is already in the database
            if ("history" %in% dbListTables(hist.db)) {
                if (dbGetQuery(hist.db,
                               sprintf("SELECT count(*) AS count FROM history WHERE trip_start_date='%s' AND trip_id='%s'",
                                       day, trip))$count > 0) {
                    next
                }
            }
            
            tmp3 <- tmp2[tmp2$vehicle_id == vid, ]
            t.start <- as.numeric(format(as.POSIXct(paste(tmp3$trip_start_date[1], tmp3$trip_start_time[1])),
                                         format = "%s"))
            
            data <- list(lat  = tmp3$position_latitude,
                         lon  = tmp3$position_longitude,
                         t    = tmp3$timestamp - t.start,
                         dmax = )
        }
    }
}


loadall()
collectHistory2("09001")
