source('../common/R/functions.R')

updateFeed()
feed <- read.csv('latest_feed.csv', header = TRUE)

## Draw all of the busses at the latest position:
#plotLocations(feed$lat, feed$lon)

## Now grab one single bus and look at it's route:
#trip1 <- getTrip(feed$trip_id[1])
#plot(trip1, feed = feed)


trackVehicle("1455")




tt <- read.csv("gtfs/stop_times.txt", header = TRUE)


summary(as.factor(tt$trip_id))
