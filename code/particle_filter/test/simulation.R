## A simulation

setwd("../")
.libPaths("../../.Rlibrary")

library(RPostgreSQL)
library(jsonlite)
library(iNZightPlots)
library(iNZightMaps)
library(mvtnorm)
source("src/pf.R")
source("src/mapping.R")
source("src/h.R")
source("src/figures.R")

## db connection/s
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")

rid <- "27402-20160920093629_v46.5"
tid <- dbGetQuery(con, sprintf("SELECT id FROM trips WHERE route_id = '%s' LIMIT 1", rid))$id

INFO <- fromJSON(sprintf("http://mybus.app/api/shape_schedule/%s", tid), flatten = TRUE)
shape <- INFO$shape
schedule <- INFO$schedule
shape$segment <- sapply(shape$dist_traveled, function(x) which(schedule$pivot.shape_dist_traveled >= x)[1])

times <- seq(0, 2 * 60, by = 5)

## functions:
drawSegments <- function(shape, schedule, speeds) {
    o <- par(bg = "#333333", fg = "#cccccc", col.axis = "#cccccc", col.lab = "#cccccc", col.main = "#cccccc")
    plot(shape$dist_traveled, rep(0.5, nrow(shape)), type = "l", 
         lwd = 4, xlab = "Distance (m)", ylim = 0:1, yaxt = "n", xaxs = "i", yaxs = "i")
    spd <- round(speeds / MAX.speed * 11)
    cols <- RColorBrewer::brewer.pal(11, "RdYlGn")[spd]
    rect(schedule$pivot.shape_dist_traveled[-nrow(schedule)], 0,
         schedule$pivot.shape_dist_traveled[-1], 1, border = cols,
         col = cols)
    par(o)
}

### SIMULATION
## generate fake data
MAX.speed <- 50 * (1000 / 60^2)

## each segment's speed changes slowly over time...
set.seed(25101990)
Speed <- matrix(NA, nrow = nrow(schedule), ncol = length(times))
Speed[, 1] <- msm::rtnorm(nrow(Speed), 10, 2, lower = 0, upper = MAX.speed)
drawSegments(shape, schedule, Speed[,  1])
