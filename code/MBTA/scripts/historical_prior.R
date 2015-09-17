### HISTORICAL PRIOR DISTRIBUTION FOR ARRIVAL TIME AT A STOP
setwd("~/Documents/uni/phd/code/MBTA")
set.seed(1)

## requires functions/database.R
source("functions/database.R")

## requires functions/gtfs.R
source("functions/gtfs.R")

## requires functions/datetime.R
source("functions/datetime.R")

## requires functions/tracking.R
source("functions/tracking.R")


### We would like to obtain historical data for bus arrival at a stop,
### for a given trip,
### for a given route.

## List of all (bus) routes:
routes <- gtfsQuery("routes", "route_id", rows = "route_type=3", order = "route_id")
(route.id <- sample(routes$route_id, 1))


## List all trips along that route and their shapes:
trips <- gtfsQuery("trips", "trip_id, shape_id",
                   rows = "route_id=%s", route.id,
                   order = "shape_id, trip_id")
(shape.ids <- unique(trips$shape_id))

shapes <- gtfsQuery("shapes", "shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence",
                    rows = "shape_id IN %s",
                    shape.ids, order = "shape_id, shape_pt_sequence")
head(shapes)

plot(shapes$shape_pt_lon, shapes$shape_pt_lat, type = "n", asp = 1)
sapply(shape.ids, function(id) drawRoute(id, new = FALSE, lwd = 4))

trip.ids <- trips$trip_id[trips$shape_id == shape.ids[1]]

## Select historical data:
data <- gtfsQuery("vehicle_positions",
                  "trip_id, route_id, vehicle_id, position_latitude, position_longitude, timestamp",
                  rows = "trip_id IN %s", trip.ids,
                  order = "vehicle_id, trip_id, timestamp")
data$date <- convertTimestamp(data$timestamp, "date")
data$time <- convertTimestamp(data$timestamp, "time")

points(data$position_longitude, data$position_latitude, col = "green4", pch = 19, cex = 0.2)

## Get trip schedules
stops <- gtfsQuery("stop_times AS st, stops AS s",
                   "st.trip_id, st.arrival_time, st.departure_time, st.stop_id, s.stop_lat, s.stop_lon, st.stop_sequence",
                   rows = "st.stop_id=s.stop_id AND st.trip_id IN %s", trip.ids,
                   order = "st.trip_id, st.stop_sequence")

points(stops$stop_lon, stops$stop_lat, col = "red", pch = 19, cex = 0.5)


## Filter out observations that occur before route has started: 
trip.start <- tapply(stops$departure_time, stops$trip_id, min, na.rm = TRUE)
trip.end <- tapply(stops$arrival_time, stops$trip_id, max, na.rm = TRUE)
del <- hms(data$time) < hms(trip.start[data$trip_id]) | hms(data$time) > hms(trip.end[data$trip_id])
data <- data[!del, ]

plot(shapes$shape_pt_lon, shapes$shape_pt_lat, type = "n", asp = 1)
sapply(shape.ids, function(id) drawRoute(id, new = FALSE, lwd = 4))
points(data$position_longitude, data$position_latitude, col = "green4", pch = 19, cex = 0.2)
points(stops$stop_lon, stops$stop_lat, col = "red", pch = 19, cex = 0.5)


## Now turn these into distance-into-trips:
HISTDB <- "gtfs-historical.db"
v1 <- data
tracks <- vector("list", nrow(v1))
pb <- txtProgressBar(0, nrow(v1), style = 3)
tracks[[1]] <- trackMyBus(v1$vehicle_id[1], v1$timestamp[1], origin = as.character(v1$date[1]))
for (i in 2:nrow(v1)) {
    setTxtProgressBar(pb, i)
    tracks[[i]] <- trackMyBus(v1$vehicle_id[i], v1$timestamp[i], tracks[[i-1]]$kalman.filter, origin = "2015-08-24")
}
close(pb)

tratrackscks <- dget("scripts/outputSEED1.dat")

data$DIT <- sapply(tracks, function(x) x$track$distance.into.trip)
data$timeIntoTrip <- time2seconds(data$time) - time2seconds(trip.start[data$trip_id])


stopInfo <- query(dbConnect(SQLite(), "trackers.db"),
                  "SELECT trip_id, arrival_time, departure_time, shape_dist_traveled FROM stop_times WHERE trip_id IN %s", trip.ids)
stopInfo$time <- ifelse(is.na(stopInfo$arrival_time), stopInfo$departure_time, stopInfo$arrival_time)

devAskNewPage(TRUE)
for (tid in unique(data$trip_id)) {
    plot(data$timeIntoTrip, data$DIT, type = "n")
    di <- data[data$trip_id == tid, ]
    si <- stopInfo[stopInfo$trip_id == tid, ]
    abline(h = si$shape_dist_traveled, lty = 3)
    points(time2seconds(si$time) - time2seconds(trip.start[tid]),
           si$shape_dist_traveled, pch = 19, col = "red", cex = 0.4)
    tapply(1:nrow(di), paste(di$date, di$trip_id, sep = "_"),
           function(i) lines(di$timeIntoTrip[i], di$DIT[i]))    
}
devAskNewPage(FALSE)

head(stopInfo)


plot(data$timeIntoTrip, data$DIT, type = "n")
abline(h = unique(stopInfo$shape_dist_traveled), lty = 3)
trip.start.sec <- time2seconds(trip.start)
cont.cols <- rainbow(200, end = 5/6, alpha = 0.4)
colID <- as.integer(199 * ((trip.start.sec - min(trip.start.sec)) / diff(range(trip.start.sec))) + 1)
names(colID) <- names(trip.start)
data$colour <- cont.cols[colID[data$trip_id]]
tapply(1:nrow(data), paste(data$date, data$trip_id, sep = "_"),
       function(i) lines(data$timeIntoTrip[i], data$DIT[i], col = data$colour[i]))

iNZightPlots::iNZightPlot(timeIntoTrip, DIT, data = data, plottype = "scatter", colby = timestamp, alpha = 0.4, cex.pt= 0.5,
                          g1 = trip_id)



di$ID <- factor(paste(di$date, di$trip_id, sep = "_"))
arrivalTimes <- vector("list", length(unique(data$trip_id)))
names(arrivalTimes) <- unique(data$trip_id)

devAskNewPage(TRUE)
for (tid in unique(data$trip_id)) {
    try({
    di <- data[data$trip_id == tid, ]
    si <- stopInfo[stopInfo$trip_id == tid, ]
    
    mat <- matrix(NA, ncol = length(unique(di$date)), nrow = nrow(si))
    dits <- tapply(1:nrow(di), di$date, function(j) di$DIT[j])
    
    dits <- lapply(dits, function(dit) {
                       if (length(dit) <= 1) return(NA)
                       for (k in (length(dit):2) - 1)
                           if (dit[k] > dit[k + 1]) dit[k] <- dit[k + 1]
                       dit
                   })
    Ts <- tapply(1:nrow(di), di$date, function(j) di$timeIntoTrip[j])
    #plot(1,1, xlim = range(c(di$timeIntoTrip), na.rm = TRUE),
    #     ylim = range(c(dits, si$shape_dist_traveled), na.rm = TRUE), type = "n",
    #     xlab = "Time (s)", ylab = "Distance (feet)")
    #abline(h = si$shape_dist_traveled, lty = 3, col = "#333333")
    colnames(mat) <- names(dits)
    for (d in names(dits)) {
        if (length(Ts[[d]]) <= 1) next
     #   lines(Ts[[d]], dits[[d]], col = "#666666")        
        mat[, d] <- approx(y = Ts[[d]], x = dits[[d]], xout = si$shape_dist_traveled)$y        
    }
    }, silent = TRUE)
    arrivalTimes[[tid]] <- mat

    
}
devAskNewPage(FALSE)


Dat <- vector("list", length(arrivalTimes))
names(Dat) <- names(arrivalTimes)
for (id in names(arrivalTimes)) {
    if (all(is.na(arrivalTimes[[id]])))
        next
    mat <- as.data.frame(arrivalTimes[[id]])
    new <- reshape(mat, varying = colnames(mat), v.names = "arrival_time", times = colnames(mat),
                   direction = "long", idvar = "stop_number")
    rownames(new) <- NULL
    new <- new[!is.na(new$arrival_time), ]
    new$stop_number <- paste0("s", sprintf("%02d", new$stop_number))
    Dat[[id]] <- cbind(new, trip_id = id)
}

fulldata <- do.call(rbind, Dat)
fulldata$stop_number <- as.factor(fulldata$stop_number)
fulldata$start_timeF <- as.character(trip.start[fulldata$trip_id])
fulldata$start_time <- time2seconds(fulldata$start_timeF)
iNZightPlots::iNZightPlot(arrival_time, stop_number, data = fulldata, plottype="dot", pch = "")

ests <- lm(arrival_time ~ stop_number + start_timeF, data = fulldata)
summary(ests)



library(lme4)
mdl <- lmer(arrival_times~ stop_number + (stop_number | trip_id), data = fulldata)
