require("RProtoBuf")
readProtoFiles("gtfs-realtime.proto")
url <- "http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb"

getVehicles <- function() {
  system(sprintf("curl -s -o vehiclepositions.pb %s", url))
  positions <- read(transit_realtime.FeedMessage, "vehiclepositions.pb")
  activeIDs <- sapply(positions$entity, function(vh) vh$vehicle$vehicle$id)

  list(positions = positions, activeIDs = activeIDs)
}

latest <- getVehicles()
dput(latest, paste0("latest-", as.numeric(Sys.time()), ".dat"))
cat("Wrote latest to disk ... (", date(), ")\n", sep = "")
