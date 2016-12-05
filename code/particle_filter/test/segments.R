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

plot(shobj, pch = NA); rezoom(0.4)
with(shapepoints, addLines(lat, lon, id = as.numeric(as.factor(id)), gpar = list(col = rgb(0,0,0,0.05))))


## Chop down to subset of segmented shapes:
segments <- dbGetQuery(con, "SELECT * FROM segments")
mode(segments$lat) <- mode(segments$lon) <- "numeric"
counts <- dbGetQuery(con,
                     sprintf("SELECT segment_id, COUNT(*) FROM segment_shapes WHERE version_id = (SELECT id FROM gtfs_versions ORDER BY startdate DESC LIMIT 1) GROUP BY segment_id ORDER BY segment_id", vlatest))
rownames(counts) <- counts$segment_id

segobj <- iNZightMap(~lat, ~lon, data = segments)
plot(segobj, pch = 1, alpha=0, colby = as.factor(counts[segments$segment_id, "count"]),
     varnames = list(colby = "Count"), col.fun = function(n) viridis::inferno(n, begin = 0.2, end = 0.8))
with(segments, addLines(lat, lon, id = segment_id,
                        gpar = list(lwd = 5, lineend = "butt",
                                    col = viridis::inferno(max(counts$count), begin = 0.2, end = 0.8)[counts$count])))
                                    #col = rgb(0, 0, 0, counts$count / max(counts$count)))))
                                    #col = sample(viridis::plasma(max(segment_id), alpha = 0.7)))))
