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
plot(obj, colby = velocity2, cex.pt = 0.2, alpha = 0.2, col.fun = diverge_hcl)





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

for (i in unique(sh2[!sh2$match, "seg"])) {
    ID <- paste0("S", as.numeric(gsub("S", "", max(shape.segments$id))) + 1)
    new.df <- data.frame(sh2[sh2$seg == i, 1:2], id = ID, coord = sh2[sh2$seg == i, 3])
    shape.segments <- rbind(shape.segments, new.df)
}

shape2 <- c("S1", "S8", "S3", "S9", "S5", "S10", "S7") 

plotShape <- function(ids, add = FALSE, ...) {
    s <- do.call(rbind, lapply(ids, function(id) shape.segments[shape.segments$id == id, ]))
    if (add) with(s, lines(shape_pt_lon, shape_pt_lat, ...))
    else with(s, plot(shape_pt_lon, shape_pt_lat, type = "l", ...))
}
plotShape(shape1, xlim = range(shape.segments$shape_pt_lon), ylim = range(shape.segments$shape_pt_lat))
plotShape(shape2, add=TRUE, col = "blue")


## ALL the data:
hist <- dbConnect(SQLite(), "db/historical-data.db")
dat <- dbGetQuery(hist,
                  "SELECT * FROM history WHERE route_id LIKE '09001%' OR route_id LIKE '08001%'")
swap <- dat$position_longitude < 0
pl <- dat$position_longitude
dat$position_longitude[swap] <- dat$position_latitude[swap]
dat$position_latitude[swap] <- pl[swap]
dat$velocity <- pmin(30, pmax(0, dat$velocity))

dat$delta <- c(0, dat$trip.timestamp[-1] - dat$trip.timestamp[-nrow(dat)])
dat$delta <- ifelse(dat$delta < 0, NA, dat$delta)
dat$velocity2 <- c(NA, (dat$distance[-1] - dat$distance[-nrow(dat)]) / dat$delta[-1])
dat$velocity2 <- pmin(30, dat$velocity)

box <- c(174.58, 174.78, -36.8, -36.91)
dat <- dat[dat$position_longitude > box[1] &
                 dat$position_longitude < box[2] &
                     dat$position_latitude < box[3] &
                         dat$position_latitude > box[4], ]

mobj <- iNZightMap(~position_latitude, ~position_longitude, data = dat, name = "090 and 080 History")
plot(mobj, colby = route_id, col.fun = rainbow_hcl, alpha = 0.2, cex.pt = 0.5)


## "speed vs distance into segment"
shape.segments$distance_into_segment<- NA
for (s in unique(shape.segments$id)) {
    i <- shape.segments$id == s
    z <- t(shape.segments[i, 1:2])
    shape.segments$distance_into_segment[i] <- c(0, cumsum(distanceFlat(z[, -ncol(z)], z[, -1])))
}

getShape <- function(ids) {
    s <- do.call(rbind, lapply(ids, function(id) shape.segments[shape.segments$id == id, ]))
    ds <- tapply(s$distance_into_segment, s$id, max)
    ds <- ds[ids]
    dx <- cumsum(ds) - ds
    s$distance_into_shape <- s$distance_into_segment + dx[s$id]
    #z <- t(s[, 1:2])
    #s$distance_into_shape <- c(0, cumsum(distanceFlat(z[, -ncol(z)], z[, -1])))
    s
}

s1 <- getShape(shape1)
s2 <- getShape(shape2)

SHAPES <- list(s1, s2)

dat$segment_id <- NA
for (i in 1:nrow(dat)) {
    si <- ifelse(dat$route_id[i] == "09001-20160126172118_v37.18", 1, 2)
    dit <- dat$distance[i]
    s <- SHAPES[[si]]
    dat$segment_id[i]<- s$id[max(1, which((dit - s$distance_into_shape) <= 0)[1] - 1)]
}
dat$segment_id <- as.factor(dat$segment_id)

head(dat)


## segment 7 - the longest with both routes
SEG <- "S7"
segi <- dat[dat$segment_id == SEG, ]
## distance into SEGMENT = DISTANCE INTO TRIP - SEGMENT TRIP DISTANCE
seg.trip.dist <- lapply(SHAPES, function(s) {
    si <- tapply(s$distance_into_segment, s$id, max)
    si <- si[unique(s$id)]
    cumsum(si) - si
})
segi$distance_into_segment <-
    segi$distance -
    sapply(seg.trip.dist[ifelse(segi$route_id =="09001-20160126172118_v37.18", 1, 2)],
           function(x) x[SEG])
par(mar = c(5.1, 4.6, 4.1, 2.1))
with(segi, plot(distance_into_segment, velocity, ylim = c(-10, 100), type = "n",
                xlab = "Distance into Segment (m)",
                ylab = expression(paste("Velocity (", ms^-1, ")"))))
tapply(1:nrow(segi), paste(segi$trip_id, segi$trip_start_date, sep = ":"),
       function(i) {
           if (length(i) < 10) return()
           f <- splinefun(segi$trip.timestamp[i], segi$distance_into_segment[i],
                          method = "monoH.FC")
           xx <- seq(min(segi$trip.timestamp[i]), max(segi$trip.timestamp[i]), length = 5001)
           lines(f(xx), f(xx, deriv = 1),
                 col = ifelse(segi$route_id[i[1]] == "09001-20160126172118_v37.18",
                              "#00990060", "#00009960"))
       }) -> o


con <- dbConnect(SQLite(), "db/gtfs-static.db")
sched <- dbGetQuery(con, "SELECT st.trip_id, st.arrival_time, st.departure_time, st.stop_id, s.stop_lon, s.stop_lat, st.stop_sequence
FROM stop_times AS st, stops AS s
WHERE st.stop_id = s.stop_id AND st.trip_id LIKE '3090020606-%'
ORDER BY stop_sequence")
shape <- dbGetQuery(con, "SELECT DISTINCT t.trip_id, st.departure_time, t.route_id, t.shape_id, t.direction_id,
       s.shape_pt_lat, s.shape_pt_lon, s.shape_pt_sequence
FROM trips AS t, stop_times AS st, shapes AS s
WHERE t.trip_id LIKE '3090020606-%' AND t.trip_id = st.trip_id AND st.stop_sequence = 1 AND t.shape_id = s.shape_id
ORDER BY st.departure_time, s.shape_pt_sequence")
mode(shape$shape_pt_lon) <- mode(shape$shape_pt_lat) <- "numeric"
sched$distance_into_trip <- getShapeDist(sched, shape)
dd <- sched[which(sched$distance_into_trip >= min(segi$distance) &
                  sched$distance_into_trip <= max(segi$distance)), "distance_into_trip"]
dd <- dd - seg.trip.dist[[1]]["S7"]
abline(v = dd, col = "#990000", lty = 2)


sched2 <- dbGetQuery(con, "SELECT st.trip_id, st.arrival_time, st.departure_time, st.stop_id, s.stop_lon, s.stop_lat, st.stop_sequence
FROM stop_times AS st, stops AS s
WHERE st.stop_id = s.stop_id AND st.trip_id LIKE '3080026136-%'
ORDER BY stop_sequence")
shape2 <- dbGetQuery(con, "SELECT DISTINCT t.trip_id, st.departure_time, t.route_id, t.shape_id, t.direction_id,
       s.shape_pt_lat, s.shape_pt_lon, s.shape_pt_sequence
FROM trips AS t, stop_times AS st, shapes AS s
WHERE t.trip_id LIKE '3080026136-%' AND t.trip_id = st.trip_id AND st.stop_sequence = 1 AND t.shape_id = s.shape_id
ORDER BY st.departure_time, s.shape_pt_sequence")
mode(shape2$shape_pt_lon) <- mode(shape2$shape_pt_lat) <- "numeric"
sched2$distance_into_trip <- getShapeDist(sched2, shape2)
dd2 <- sched2[which(sched2$distance_into_trip >= min(segi$distance) &
                    sched2$distance_into_trip <= max(segi$distance)), "distance_into_trip"]
dd2 <- dd2 - seg.trip.dist[[2]]["S7"]
abline(v = dd2, col = "#666666", lwd = 2)

lapply(SHAPES, function(s) max(s$distance_into_shape))









############### SHAPES into DATABASE

shape.segments$shape_pt_sequence <-
    do.call(c, tapply(shape.segments$distance_into_segment, shape.segments$id, order)[unique(shape.segments$id)])
segments <- shape.segments[, c("id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "distance_into_segment")]
colnames(segments) <- c("segment_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "distance_into_segment")

con <- dbConnect(SQLite(), "db/gtfs-test.db")
dbGetQuery(con, "DROP TABLE segments")
dbGetQuery(con, "CREATE TABLE segments(
segment_id TEXT,
shape_pt_lat TEXT,
shape_pt_lon TEXT,
shape_pt_sequence INTEGER,
distance_into_segment TEXT
)")
dbWriteTable(con, "segments", segments, row.names = FALSE, append = TRUE)


shapes <- rbind(cbind("09001", shape1, 1:length(shape1)),
                cbind("08001", shape2, 1:length(shape2)))
shapes <- as.data.frame(shapes)
colnames(shapes) <- c("shape_id", "segment_id", "segment_sequence")
dbGetQuery(con, "DROP TABLE shapes")
dbGetQuery(con, "CREATE TABLE shapes(
shape_id TEXT,
segment_id TEXT,
segment_sequence INTEGER
)")
dbWriteTable(con, "shapes", shapes, row.names = FALSE, append = TRUE)
dbDisconnect(con)






###### Now add another route
trips <- unique(area.subset[grepl("^07001", area.subset$route_id), "trip_id"])
t1 <- gsub("-.*", "", trips[1])
con <- dbConnect(SQLite(), "db/gtfs-static.db")
s1 <- dbGetQuery(con,
                 sprintf("SELECT shape_id FROM trips WHERE trip_id LIKE '%s%%'", t1))
shape <- dbGetQuery(con,
                    sprintf("SELECT * FROM shapes WHERE shape_id='%s'", s1[1]))
mode(shape$shape_pt_lat) <- mode(shape$shape_pt_lon) <- "numeric"

sh3 <- shape[, c("shape_pt_lat", "shape_pt_lon")]

sh3$coord <- apply(sh3, 1, function(x) paste(round(x, 4), collapse = ","))

match <- sh3$coord %in%
    with(shape.segments, paste(round(shape_pt_lat, 4), round(shape_pt_lon, 4), sep = ","))
SEGS <- c(1, cumsum(abs(diff(match))) + 1)
ni <- tapply(1:length(SEGS), SEGS, length)
mi <- tapply(match, SEGS, mean)
## any "short" non-matches -> matches
mi[mi == 0 & ni < 10] <- 1
match <- as.logical(rep(mi, ni))
SEGS <- c(1, cumsum(abs(diff(match))) + 1)
sh3$match <- match
sh3$seg <- SEGS

mobj <- iNZightMap(~shape_pt_lat, ~shape_pt_lon, data = shape)
plot(mobj, pch = 19, cex.pt = 0.05, col.pt = "#0000cc")
addPoints(SH1$shape_pt_lon, SH1$shape_pt_lat, gp = list(col = "#cc000040", cex = 0.2), pch = 19)
addPoints(sh2$shape_pt_lon, sh2$shape_pt_lat, gp = list(col = "#cc000040", cex = 0.2), pch = 19)
with(sh3, addPoints(shape_pt_lon, shape_pt_lat,
                    gp = list(col = ifelse(match, "#009999", "#990099"), cex = 0.2), pch = 19))

## add Non-matching segments:
con <- dbConnect(SQLite(), "db/gtfs-test.db")
cur.ids <- dbGetQuery(con, "SELECT DISTINCT segment_id FROM segments")$segment_id
seg.df <- NULL
sh3$segment_id <- NA
for (i in unique(sh3[!sh3$match, "seg"])) {
    ID <- paste0("S", max(as.numeric(gsub("S", "", cur.ids))) + 1)
    cur.ids <- c(cur.ids, ID)
    new.df <- data.frame(segment_id = ID, sh3[sh3$seg == i, 1:2])
    z <- t(new.df[, 2:3])
    new.df$shape_pt_sequence <- 1:nrow(new.df)
    new.df$distance_into_segment <- c(0, cumsum(distanceFlat(z[, -1], z[, -ncol(z)])))
    seg.df <- rbind(seg.df, new.df)
    sh3$segment_id[sh3$seg == i] <- ID
}
dbWriteTable(con, "segments", seg.df, row.names = FALSE, append = TRUE)
dbDisconnect(con)



## fix up matching segments:
shape3 <- numeric()
for (i in unique(sh3$seg)) { ##[sh3$match])) {
    if (!i %in% sh3$seg[match]) {
        shape3 <- c(shape3, sh3[sh3$seg == i, ]$segment_id[1])
        next
    }
    segments <- dbGetQuery(dbConnect(SQLite(), "db/gtfs-test.db"),
                           "SELECT * FROM segments")
    mode(segments$shape_pt_lat) <- mode(segments$shape_pt_lon) <- mode(segments$distance_into_segment) <- "numeric"
    ri <- sh3$seg == i
    ## we know it matches - which segment(s) does it match??
    seg.match <- tapply(with(segments, paste(round(shape_pt_lat, 4), round(shape_pt_lon, 4), sep = ",")),
                        segments$segment_id,
                        function(x) sum(x %in% sh3[ri, "coord"]) > 10)
    seg.match <- names(seg.match)[seg.match]

    for (s in seg.match) {
        ss <- segments[segments$segment_id == s, ]
        SEGcoord <- with(ss, paste(round(shape_pt_lat, 4), round(shape_pt_lon, 4), sep = ","))
        match1 <- sh3[ri, "coord"] %in% SEGcoord
        SEGS <- c(1, cumsum(abs(diff(match1))) + 1)
        ni <- tapply(1:length(SEGS), SEGS, length)
        mi <- tapply(match1, SEGS, mean)
        mi[mi == 0 & ni < 10] <- 1
        match2 <- as.logical(rep(mi, ni))      ## NEW in EXISTING

        match3 <- SEGcoord %in% sh3$coord[ri][match2]
        SEGS <- c(1, cumsum(abs(diff(match3))) + 1)
        ni <- tapply(1:length(SEGS), SEGS, length)
        mi <- tapply(match3, SEGS, mean)
        ## any "short" non-matches -> matches
        mi[mi == 0 & ni < 10] <- 1
        match4 <- as.logical(rep(mi, ni))      ## EXISTING in NEW
        
        if (all(match4)) {
            ## Yay! everything matches, so set that as the ID
            ## sh3$segment_id[ri][match2] <- s
            shape3 <- c(shape3, s)
        } else {
            con <- dbConnect(SQLite(), "db/gtfs-test.db")
            cur.ids <- dbGetQuery(con, "SELECT DISTINCT segment_id FROM segments")$segment_id
            ## it's a partial match - need to split the segment in the database!!
            SEGS <- c(1, cumsum(abs(diff(match4))) + 1)
            ## create NEW shapes
            seg.df <- NULL
            new.ids <- character()
            for (segj in unique(SEGS)) {
                j <- which(SEGS == segj)
                tmp <- ss[j, ]
                tmp$shape_pt_sequence <- order(tmp$shape_pt_sequence)
                tmp$distance_into_segment <- tmp$distance_into_segment - min(tmp$distance_into_segment)
                newID <- paste0("S", max(as.numeric(gsub("S", "", cur.ids))) + 1)
                cur.ids <- c(cur.ids, newID)
                new.ids <- c(new.ids, newID)
                tmp$segment_id <- newID
                seg.df <- rbind(seg.df, tmp)
                
                if (all(match4[j])) {
                    shape3 <- c(shape3, newID)
                }
            }
            dbWriteTable(con, "segments", seg.df, row.names = FALSE, append = TRUE)

            ## fix up previous shapes:
            ## s -> new.ids
            sAffected <- dbGetQuery(con, sprintf("SELECT shape_id FROM shapes WHERE segment_id='%s'", s))$shape_id
            oldShapes <- dbGetQuery(con, sprintf("SELECT * FROM shapes WHERE shape_id IN ('%s')",
                                                 paste0(sAffected, collapse = "','")))
            print(oldShapes)
            ## w <- which(oldShapes$segment_id == s)
            ## lapply(w, function(x) {
            tapply(1:nrow(oldShapes), oldShapes$shape_id,
                   function(k) {
                       oSk <- oldShapes[k, ]
                       wk <- which(oSk$segment_id == s)
                       nk <- nrow(oSk)
                       alt <- if (wk == 1) {
                           rbind(cbind(oSk$shape_id[wk], new.ids,
                                       as.numeric(oSk$segment_sequence[wk]) + (1:length(new.ids))/10),
                                 oSk[(wk+1):nrow(oSk), ])
                       } else if (wk == nk) {
                           rbind(oS[1:(wk-1), ],
                                 cbind(oSk$shape_id[wk], new.ids,
                                       as.numeric(oSk$segment_sequence[wk]) + (1:length(new.ids))/10))
                       } else {
                           rbind(oSk[1:(wk-1), ],
                                 cbind(shape_id = oSk$shape_id[wk], segment_id = new.ids,
                                       segment_sequence =
                                           as.numeric(oSk$segment_sequence[wk]) + (1:length(new.ids))/10),
                                 oSk[(wk+1):nrow(oSk), ])
                       }
                       alt$segment_sequence <- order(as.numeric(alt$segment_sequence))
                       alt
                       ## }))
                   }) -> newShapes
            newShapes <- do.call(rbind, newShapes)
            dbGetQuery(con, sprintf("DELETE FROM shapes WHERE shape_id IN ('%s')",
                                    paste0(sAffected, collapse = "','")))
            dbWriteTable(con, "shapes", newShapes, row.names = FALSE, append = TRUE)
            ## and delete old segments
            dbGetQuery(con, sprintf("DELETE FROM segments WHERE segment_id = '%s'", s))
            dbDisconnect(con)
        }
    }
}

con <- dbConnect(SQLite(), "db/gtfs-test.db")

dbWriteTable(con, "shapes", as.data.frame(cbind("shape_id" = "07001", "segment_id" = shape3,
                                                "segment_sequence" = 1:length(shape3))),
             row.names = FALSE, append = TRUE)




shapes <- dbGetQuery(con, "SELECT sh.shape_id, sh.segment_id, sh.segment_sequence,
                                  sg.shape_pt_lat, sg.shape_pt_lon, sg.shape_pt_sequence
                             FROM shapes AS sh, segments AS sg
                            WHERE sh.segment_id=sg.segment_id
                            ORDER BY sh.shape_id, sh.segment_sequence, sg.shape_pt_sequence")
mode(shapes$shape_pt_lat) <- mode(shapes$shape_pt_lon) <- "numeric"

mobj <- iNZightMap(~position_latitude, ~position_longitude, data = dat, name = "090 and 080 History")
plot(mobj, colby = route_id, col.fun = rainbow_hcl, alpha = 0.2, cex.pt = 0.5)
cex <- as.numeric(as.factor(shapes$shape_id)) / 3
tapply(1:nrow(shapes), shapes$shape_id, function(i) {
           with(shapes[i, ], addPoints(shape_pt_lon, shape_pt_lat,
                                       gp = list(cex = cex[i], col = "#444444")))
       })
