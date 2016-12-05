setwd(file.path(gsub(".code.particle_filter.+", "", getwd()),
                "code", "particle_filter")); getwd()
.libPaths("../../.Rlibrary")

library(RPostgreSQL)
library(jsonlite)
library(iNZightPlots)
library(iNZightMaps)
source("src/pf.R")
source("src/mapping.R")
host <- ifelse(system("whoami", TRUE) == "tell029", "localhost", "130.216.50.187")
user <- "homestead"
port <- "54320"
pass <- "secret"
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = host,
                user = user, port = port, password = pass)
con2 = dbConnect(drv, dbname = "historical", host = host,
                 user = user, port = port, password = pass)


## Get all the (latest) shapes:
vlatest <- dbGetQuery(con, "SELECT version FROM gtfs_versions ORDER BY startdate DESC LIMIT 1")$version
uniqueshapes <- dbGetQuery(con, "SELECT MIN(shape_id) AS shape_id FROM trips GROUP BY route_id")
shapepoints <- dbGetQuery(con, "SELECT * FROM shapes WHERE id IN (SELECT MIN(shape_id) AS shape_id FROM trips GROUP BY route_id) ORDER BY id, pt_sequence")
mode(shapepoints$lat) <- mode(shapepoints$lon) <- "numeric"

shobj <- iNZightMap(~lat, ~lon, data = shapepoints[c(which.min(shapepoints$lat), which.min(shapepoints$lon),
                                                     which.max(shapepoints$lat), which.max(shapepoints$lon)),],
                    name = "Route Shapes")

plot(shobj, pch = NA)
with(shapepoints, addLines(lat, lon, id = as.numeric(as.factor(id))))
