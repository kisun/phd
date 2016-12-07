## "fast" script to model real-time stuff
#
## only the particle filter (vehicle model) - it'll run on a cronjob

.libPaths("../../.Rlibrary")

suppressMessages(library(RPostgreSQL))
source("src/pf.R")
source("src/h.R")

### SETTINGS
ROUTES = c("27401", "27402", "25801", "25802")
N = 500
MAX.speed = 60 * 1000 / 60^2
MIN.speed = 10 * 1000 / 60^2

host = "localhost"
user = "homestead"
port = "54320"
pass = "secret"
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = host,
                user = user, port = port, password = pass)

vps = dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE route_id IN (SELECT id FROM routes WHERE route_id IN ('%s')) ORDER BY timestamp", paste0(ROUTES, sep = "','")))

infoList = lapply(unique(vps$trip_id), function(ID) {
    res = jsonlite::fromJSON(sprintf("http://localhost:8000/api/shape_schedule/%s", ID), flatten = TRUE)
    shape = res$shape
    if ("segment_info.id" %in% names(shape)) {
        Shape = shape$segment_info.shape_points
        dmax = cumsum(c(0, sapply(Shape, function(x) max(x$dist_traveled))))
        invisible(lapply(1:length(Shape), function(i) {
            Shape[[i]]$dist_traveled <<- Shape[[i]]$dist_traveled + dmax[i]
            Shape[[i]]$leg <<- i
        }))
        shape = do.call(rbind, Shape)
        rm(Shape)
    } else {
        shape$segment =
            sapply(shape$dist_traveled, function(x) which(shape$schedule$pivot.shape_dist_traveled >= x)[1])
    }
    res$shape = shape
    res
})
names(infoList) <- unique(vps$trip_id)

cat("Modeling ", length(infoList), " vehicle positions ...\n")

parallel::mclapply(1:nrow(vps), function(i) {
  pf(dbConnect(dbDriver("PostgreSQL"), dbname = "homestead", host = host,
               user = user, port = port, password = pass),
     vps[i, "vehicle_id"], 500, sig.gps = 5,
     vp = vps[i, ],  info = infoList[[vps[i, "trip_id"]]],
     SPEED.range = c(MIN.speed, MAX.speed), rho = 0.5, draw = FALSE)
}) -> results

cat("Finished.\n\n")
