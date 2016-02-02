## functions for reading GTFS database

getPositions <- function(con, route.id, vehicle.id, date,
                         order = "timestamp", verbose = TRUE, ...) {
    ## SQL preparation ...
    where <- character()
    sql <- "SELECT * FROM vehicle_positions"

    if (!missing(route.id))
        where <- c(where, sprintf("route_id LIKE '%s%s'", route.id, "%"))
    if (!missing(vehicle.id))
        where <- c(where, sprintf("vehicle_id = '%s'", vehicle.id))
    if (!missing(date)) {
        ## convert it to MIN and MAX timestamp
        datets <- as.numeric(format(as.POSIXct(date), format = "%s")) + c(0, 86400)
        where <- c(where, sprintf("timestamp >= %s AND timestamp < %s", datets[1], datets[2]))
    }

    if (length(where))
        sql <- paste0(sql, " WHERE ", paste0(where, collapse = " AND "))

    if (verbose)
        cat(sql, "\n")

    ## SQL call
    pos <- dbGetQuery(con, sql)

    ## Value fixes
    pos$trip_start_date <- ifelse(nchar(pos$trip_start_date) == 0,
                                  tsDate(pos$timestamp),
                                  pos$trip_start_date)
    #strip.cols <- c("trip_id", "route_id")
    #versions <- lapply(pos[, strip.cols], function(x) gsub(".+_v", "", x))
    #print(do.call(cbind, versions))
    #pos[, strip.cols] <- lapply(pos[, strip.cols], function(x) gsub("-.+", "", x))

    pos
}



getTrips <- function(ids, con = "db/gtfs-static.db", verbose = TRUE,
                     ...,
                     .con = dbConnect(SQLite(), con)) {
    ## Get all of the information for a bunch of trips:
    sql <- sprintf("SELECT DISTINCT t.trip_id, route_id, shape_id, trip_headsign, service_id, departure_time
FROM trips as t, stop_times as s
WHERE t.trip_id=s.trip_id AND t.trip_id IN ('%s') AND s.stop_sequence=1
ORDER BY departure_time",
                   paste(ids, collapse = "','"))

    if (verbose) cat(sql, '\n')

    resp <- dbGetQuery(.con, sql)

    if (nrow(resp) == 0) {
      api <- readLines("apikey.txt")
      # Stop Times
      url <- sapply(ids, function(id) sprintf("http://api.at.govt.nz/v1/gtfs/stopTimes/tripId/%s?api_key=%s", id, api))

      l <- list()
      pb <- txtProgressBar(0, length(url), style=3)
      for (i in seq_along(url)) {
        l[[i]] <- jsonlite::fromJSON(url[i])$response
        setTxtProgressBar(pb, i)
      }
      close(pb)

      f <- as.data.frame(do.call(rbind, l))
      if (dbWriteTable(.con, "stop_times", f, append = TRUE)) {
          resp <- dbGetQuery(.con, sql)
      } else {
          stop("\nUnable to get the associated stop times ...\n")
      }
    }

    resp
}

getSchedule <- function(id,  con = "db/gtfs-static.db", verbose = TRUE,
                        ...,
                        .con = dbConnect(SQLite(), con)) {
    ## Get the stop schedule for a trip:
    sql <- sprintf("SELECT st.trip_id, st.arrival_time, st.departure_time, st.stop_id, s.stop_lon, s.stop_lat, st.stop_sequence
FROM stop_times AS st, stops AS s
WHERE st.stop_id = s.stop_id AND st.trip_id = '%s'
ORDER BY stop_sequence", id)

    if (verbose) cat(sql, '\n')

    resp <- dbGetQuery(.con, sql)

    if (nrow(resp) == 0) {
      api <- readLines("apikey.txt")
      ## no entries for that ID in the database - download new ones
      url <-
    if (verbose) cat(sql, '\n')

      f <- as.data.frame(jsonlite::fromJSON(url[1])$response)

      if (dbWriteTable(.con, "stop_times", f, append = TRUE)) {
          resp <- dbGetQuery(.con, sql)
      } else {
          stop("\nUnable to get the associated stop times ...\n")
      }

    }

    resp
}

getBlocksA <- function(date, con = "db/gtfs-history.db", verbose = TRUE,
                       ...,
                       .con = dbConnect(SQLite(), con)) {
    ## Grab all vehicle-blocks on a given date:

    datets <- as.numeric(format(as.POSIXct(date), format = "%s")) + c(0, 86400)

    sql <- sprintf("SELECT DISTINCT vehicle_id, trip_id, min(trip_start_time) AS start FROM vehicle_positions
WHERE timestamp >= %s AND timestamp < %s
GROUP BY vehicle_id, trip_id
ORDER BY vehicle_id, start", datets[1], datets[2])

    if (verbose) cat(sql, '\n')

    blocks <- dbGetQuery(.con, sql)
    blocks$trip_id <- gsub("-.+", "", blocks$trip_id)

    ## also get route numbers ...
    trps <- paste(blocks$trip_id, collapse = "','")
    sql <- sprintf("SELECT trip_id, route_id, trip_headsign, shape_id, service_id
FROM trips WHERE trip_id IN ('%s') GROUP BY trip_id", trps)

    trips <- dbGetQuery(dbConnect(SQLite(), "db/gtfs-static.db"), sql)

    merge(blocks, trips, by = "trip_id", all.x = TRUE, all.y = FALSE, sort = FALSE)
}

getBlock <- function(id,  con = "db/gtfs-static.db", verbose = TRUE,
                     ...,
                     .con = dbConnect(SQLite(), con)) {
    ## Take a series of trip_id's and return the block information:
    tids <- paste(id, collapse = "','")
    sql <- sprintf("SELECT t.trip_id, t.route_id, t.shape_id,
         CASE WHEN arrival_time IS NULL
              THEN departure_time
              ELSE arrival_time END AS time,
         st.stop_id, s.stop_lon, s.stop_lat, st.stop_sequence
FROM trips AS t, stop_times AS st, stops AS s
WHERE t.trip_id IN ('%s') AND t.trip_id = st.trip_id AND st.stop_id = s.stop_id
ORDER BY time, stop_sequence DESC", tids)

    if (verbose) cat(sql, '\n')

    dbGetQuery(.con, sql)
}


getPattern <- function(id,  con = "db/gtfs-static.db", verbose = TRUE,
                       ...,
                       .con = dbConnect(SQLite(), con)) {
    ## Take a series of trip_id's and return the block information:
    tids <- paste(id, collapse = "','")
    sql <- sprintf("SELECT DISTINCT t.trip_id, st.departure_time, t.route_id, t.shape_id,
       s.shape_pt_lat, s.shape_pt_lon, s.shape_pt_sequence
FROM trips AS t, stop_times AS st, shapes AS s
WHERE t.trip_id IN ('%s') AND t.trip_id = st.trip_id AND st.stop_sequence = 1 AND t.shape_id = s.shape_id
ORDER BY st.departure_time, s.shape_pt_sequence", tids)

    if (verbose) cat(sql, '\n')

    resp <- dbGetQuery(.con, sql)

    if (any(!id %in% unique(resp$trip_id))) {
      ID <- id[!id %in% unique(resp$trip_id)]
      cat("\nDownloading shape files ...\n")
      print(ID)

      api <- readLines("apikey.txt")
      # Shape files
      url <- sapply(ID, function(id) sprintf("http://api.at.govt.nz/v1/gtfs/shapes/tripId/%s?api_key=%s", id, api))

      l <- list()
      pb <- txtProgressBar(0, length(ID), style = 3)
      for (i in seq_along(ID)) {
        l[[i]] <- try(jsonlite::fromJSON(url[i])$response, TRUE)
        if (inherits(l[[i]], "try-error")) {
          l[[i]] <- NULL
          warning("Error downloading shape ", ID[i])
        }
        setTxtProgressBar(pb, i)
      }
      close(pb)

      f <- as.data.frame(do.call(rbind, l))
      if (dbWriteTable(.con, "shapes", f, append = TRUE)) {
          resp <- dbGetQuery(.con, sql)
      } else {
          stop("\nUnable to get the associated shape files ...\n")
      }
    }

    resp
}


getShapeDist <- function(sched, shape) {
    sched <- sched[, c("stop_lon", "stop_lat")]
    shape <- shape[, c("shape_pt_lon", "shape_pt_lat")]
    
    z <- t(shape[,1:2])
    di <- distanceFlat(z[, -ncol(z)], z[, -1])
    shape$distance <- c(0, cumsum(di))
    shape$length <- c(di, NA)
    shape$bearing <- c(bearing(z[, -ncol(z)], z[, -1]), NA)
    shape <- shape[shape$length > 0 | is.na(shape$length), ]
    
    ## Convert to FLAT XY
    shape$lam <- as.numeric(shape$shape_pt_lon) * pi / 180
    shape$phi <- as.numeric(shape$shape_pt_lat) * pi / 180
    sched$lam <- as.numeric(sched$stop_lon) * pi / 180
    sched$phi <- as.numeric(sched$stop_lat) * pi / 180
    
    phi1 <- mean(shape$phi)
    shape$x <- shape$lam * cos(phi1)
    shape$y <- shape$phi
    sched$x <- sched$lam * cos(phi1)
    sched$y <- sched$phi
    
    #plot(shape$x, shape$y, type = "l", asp = 1)
    
    d <- numeric(nrow(sched))
    J <- 1
    nr <- nrow(shape)
    nr1 <- nr - 1
    for (i in 1:nrow(sched)) {
        p <- as.numeric(sched[i, c("x", "y")])
        
        di <- ri <- numeric(nr - J)
        pxy <- matrix(NA, nrow = nr - J, ncol = 2)
        for (j in J:nr1) {
            ji <- j - J + 1
            q1 <- as.numeric(shape[j, c("x", "y")])
            q2 <- as.numeric(shape[j + 1, c("x", "y")])
            
            v <- q2 - q1
            w <- p - q1
        
            wv <- w %*% v
            vv <- v %*% v
            ww <- w %*% w
                        
            if (wv < 0) {
                r2 <- ww
                di[ji] <- 0
                ri[ji] <- sqrt(r2)
            } else if (wv <= vv) {
                d2 <- wv^2 / vv
                r2 <- ww - d2
                ri[ji] <- sqrt(r2)
                di[ji] <- sqrt(d2)
            } else {
                r2 <- (w - v) %*% (w - v)
                d2 <- vv
                ri[ji] <- sqrt(r2)
                di[ji] <- sqrt(d2)
            }
            
            pxy[ji, ] <- q1 + di[ji] / sqrt(vv) * v
        }
        
        wi <- which.min(ri)
        J <- J + wi - 1
        
        ## convert it:
        ## points(pxy[1], pxy[2], cex = 0.5, pch = 4, col = "blue")
        pll <- c(pxy[wi, 1] / cos(phi1), pxy[wi, 2]) * 180 / pi
        d[i] <- shape[J, "distance"] + distanceFlat(pll, as.numeric(shape[J, c("shape_pt_lon", "shape_pt_lat")]))
    }

    d
}
