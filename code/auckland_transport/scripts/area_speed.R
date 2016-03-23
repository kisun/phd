## Here we attempt to obtain speed information for an area.
## Start off with 07X, 08X, and 09X busses.


setwd("~/Documents/uni/phd/code/auckland_transport")
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))
loadall()

## CON <- dbConnect(SQLite(), "db/backups/gtfs-history_latest.db")
## positions070 <- getPositions(CON, route.id = "070")
## positions080 <- getPositions(CON, route.id = "080")
## positions090 <- getPositions(CON, route.id = "090")
## positions <- rbind(positions070, positions080, positions090)

## loadall()
## collectHistory(route = "090", day = "2016-02-19", verbose = FALSE)


## dates <- c("2016-02-20", "2016-02-21", "2016-02-22", "2016-02-23", "2016-02-24", "2016-02-25",
##            "2016-02-26", "2016-02-27", "2016-02-28", "2016-02-29", "2016-01-01", "2016-01-02",
##            "2016-01-03", "2016-01-04", "2016-01-05", "2016-01-06", "2016-01-07", "2016-01-08")
## routes <- c("090", "080", "070")

## for (route in routes) {
##     for (date in dates) {
##         try(collectHistory(route = route, day = date))
##     }
## }

area <- dbGetQuery(dbConnect(SQLite(), "db/historical-data.db"),
                   "SELECT * FROM history")
## fix mess up
swap <- area$position_longitude < 0
pl <- area$position_longitude
area$position_longitude[swap] <- area$position_latitude[swap]
area$position_latitude[swap] <- pl[swap]
area$velocity <- pmin(30, pmax(0, area$velocity))

area$delta <- c(0, area$trip.timestamp[-1] - area$trip.timestamp[-nrow(area)])
area$delta <- ifelse(area$delta < 0, NA, area$delta)
area$velocity2 <- c(NA, (area$distance[-1] - area$distance[-nrow(area)]) / area$delta[-1])
area$velocity2 <- pmin(30, area$velocity)


box <- c(174.58, 174.78, -36.8, -36.91)
area.subset <- area[area$position_longitude > box[1] &
                        area$position_longitude < box[2] &
                            area$position_latitude < box[3] &
                                area$position_latitude > box[4], ]

obj <- iNZightMap(~position_latitude, ~position_longitude,
                  data = area.subset[grepl("^090", area.subset$route_id), ])
plot(obj, colby = velocity2, cex.pt = 0.2, alpha = 0.2, col.fun = diverge_hcl, g1=route_id)

ClickOnZoom(0.05)




## trip
trips <- unique(area.subset[grepl("^09001", area.subset$route_id), "trip_id"])
t1 <- gsub("-.*", "", trips[1])
con <- dbConnect(SQLite(), "db/gtfs-static.db")
s1 <- dbGetQuery(con,
                 sprintf("SELECT shape_id FROM trips WHERE trip_id LIKE '%s%%'", t1))
shape <- dbGetQuery(con,
                    sprintf("SELECT * FROM shapes WHERE shape_id='%s'", s1[1]))
mode(shape$shape_pt_lat) <- mode(shape$shape_pt_lon) <- "numeric"
mobj <- iNZightMap(~shape_pt_lat, ~shape_pt_lon, data = shape)
plot(mobj, pch = 19, cex.pt = 0.05)

trips2 <- unique(area.subset[grepl("^08001", area.subset$route_id), "trip_id"])
t2 <- gsub("-.*", "", trips2[1])
s2 <- dbGetQuery(con,
                 sprintf("SELECT shape_id FROM trips WHERE trip_id LIKE '%s%%'", t2))
shape2 <- dbGetQuery(con,
                     sprintf("SELECT * FROM shapes WHERE shape_id='%s'", s2[1]))
mode(shape2$shape_pt_lat) <- mode(shape2$shape_pt_lon) <- "numeric"
mobj <- iNZightMap(~shape_pt_lat, ~shape_pt_lon, data = shape2)
plot(mobj, pch = 19, cex.pt = 0.05, col.pt = "#0000cc")

SH1 <- shape[, c("shape_pt_lat", "shape_pt_lon")]
addPoints(SH1$shape_pt_lon, SH1$shape_pt_lat, gp = list(col = "#cc0000", cex = 0.01), pch = 19)

sh2 <- shape2[, c("shape_pt_lat", "shape_pt_lon")]

SH1$coord <- apply(SH1, 1, function(x) paste(round(x, 4), collapse = ","))
sh2$coord <- apply(sh2, 1, function(x) paste(round(x, 4), collapse = ","))

match <- SH1$coord %in% sh2$coord
addPoints(SH1$shape_pt_lon[match], SH1$shape_pt_lat[match],
          gp = list(col = "#00ffff", cex = 0.3), pch = 1)

SEGS <- c(1, cumsum(abs(diff(match))) + 1)

plot(mobj, pch = 19, cex.pt = 0.05, col.pt = "#0000cc40")
addPoints(SH1$shape_pt_lon, SH1$shape_pt_lat, gp = list(col = "#cc000040", cex = 0.01), pch = 19)
with(SH1, addPoints(shape_pt_lon, shape_pt_lat,
                    gp = list(col = ifelse(match, "#009999", "#990099"), cex = 0.2), pch = 19))

ni <- tapply(1:length(SEGS), SEGS, length)
mi <- tapply(match, SEGS, mean)
## any "short" non-matches -> matches
mi[mi == 0 & ni < 10] <- 1
match.new <- as.logical(rep(mi, ni))
SEGS <- c(1, cumsum(abs(diff(match.new))) + 1)

plot(mobj, pch = 19, cex.pt = 0.05, col.pt = "#0000cc40")
addPoints(SH1$shape_pt_lon, SH1$shape_pt_lat, gp = list(col = "#cc000040", cex = 0.01), pch = 19)
with(SH1, addPoints(shape_pt_lon, shape_pt_lat,
                    gp = list(col = ifelse(match.new, "#009999", "#990099"), cex = 0.2), pch = 19))

shape.segments <- data.frame(SH1[, 1:2], id = paste0("S", SEGS),
                             coord = as.character(
                                 apply(SH1[, 1:2], 1,
                                       function(x) paste(round(x, 4), collapse = ","))
                             ), stringsAsFactors = FALSE)

shape1 <- as.character(unique(shape.segments$id))

## and now do shape 2:
match <- sh2$coord %in%
    with(shape.segments, paste(round(shape_pt_lat, 4), round(shape_pt_lon, 4), sep = ","))
SEGS <- c(1, cumsum(abs(diff(match))) + 1)
ni <- tapply(1:length(SEGS), SEGS, length)
mi <- tapply(match, SEGS, mean)
## any "short" non-matches -> matches
mi[mi == 0 & ni < 10] <- 1
match <- as.logical(rep(mi, ni))
SEGS <- c(1, cumsum(abs(diff(match))) + 1)
sh2$match <- match
sh2$seg <- SEGS
