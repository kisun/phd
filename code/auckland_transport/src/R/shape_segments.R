## convert shape files into segment files
shape2seg <- function(id, db = "db/gtfs-static2.db",
                      plot = FALSE, verbose = FALSE, ...,
                      .con = dbConnect(SQLite(), db)) {
    ## check ID exists:
    if (nrow(dbGetQuery(.con, sprintf("SELECT DISTINCT shape_id FROM shapes WHERE shape_id='%s'",
                                      id))) < 1) stop("Invalid shape_id")

    ## check if shapes_seg table exists:
    if (!"shapes_seg" %in% dbListTables(.con))
        stop("Create shapes_seg table first (run `createSegmentTable()`)")
    
    ## if shape ID already in shapes_seg table, delete and start again:
    dbGetQuery(.con, sprintf("DELETE FROM shapes_seg WHERE shape_id='%s'", id))


    ## --- get full shape file
    shape <- dbGetQuery(.con,
                        sprintf("SELECT CAST(shape_pt_lat AS REAL) AS lat,
                                        CAST(shape_pt_lon AS REAL) AS lon
                                 FROM shapes WHERE shape_id='%s'
                                 ORDER BY shape_pt_sequence", id))
    shape$coord <- apply(shape, 1, function(x) paste(round(x, 4), collapse = ","))

    ## --- shape matching
    segments <- getSegments(.con)
    match <- if (nrow(segments) == 0) {
                 rep(FALSE, length = nrow(shape))
             } else { 
                 shape$coord %in% with(segments,
                                       paste(round(lat, 4), round(lon, 4), sep = ","))
             }

    SEGS <- c(1, cumsum(abs(diff(match))) + 1)
    ni <- tapply(1:length(SEGS), SEGS, length)
    mi <- tapply(match, SEGS, mean)
    ## any "short" non-matches -> matches
    mi[mi == 0 & ni < 10] <- 1
    match <- as.logical(rep(mi, ni))
    SEGS <- c(1, cumsum(abs(diff(match))) + 1)
    shape$match <- match
    shape$seg <- SEGS

    ## --- write NEW segments to database:
    cur.ids <- dbGetQuery(.con, "SELECT DISTINCT segment_id FROM segments")$segment_id
    seg.df <- NULL
    shape$segment_id <- NA

##    try(plotSegments(db ="db/gtfs-static-symonds.db"), TRUE)
    
    if (!all(shape$match)) {
        for (i in unique(shape[!shape$match, "seg"])) {
            ID <- if (length(cur.ids) == 0) { 1 } else { max(cur.ids, na.rm = TRUE) + 1 }
            cur.ids <- c(cur.ids, ID)
            ## need to include the point before and after:
            ssi <- which(shape$seg == i)
            ##if (min(ssi) > 1) ssi <- c(min(ssi) - 1, ssi)
            ##if (max(ssi) < nrow(shape)) ssi <- c(ssi, max(ssi) + 1)
            
            ##try({
            ##    with(shape[ssi, ], addLines(lon, lat, gpar = list(col = "red")))
            ##    grid::grid.locator()
            ##}, TRUE)
            
            
            new.df <- data.frame(segment_id = ID, shape[ssi, 1:2])
            z <- t(new.df[, 2:3])
            new.df$shape_pt_sequence <- 1:nrow(new.df)
            new.df$distance <- c(0, cumsum(distanceFlat(z[, -1], z[, -ncol(z)])))
            seg.df <- rbind(seg.df, new.df)
            shape$segment_id[shape$seg == i] <- ID
        }
        if (!is.null(seg.df)) {
            colnames(seg.df) <- c("segment_id", "shape_pt_lat", "shape_pt_lon",
                                  "shape_pt_sequence", "distance_into_segment")
            if (!dbWriteTable(.con, "segments", seg.df, row.names = FALSE, append = TRUE))
                stop("Unable to write to the database.")
        }
    }

    ## --- deal with matching segments and create shape IDs

    ## gotta split "matches" up into segmental matches: 1-1-1-1-1 -> 1-1-1-2-2
    segments <- getSegments(.con)
    ## plotSegments(db ="db/gtfs-static-symonds.db")
    ## grid::grid.locator()
    for (i in unique(shape$seg)) {
        ri <- shape$seg == i
        tapply(paste(round(segments$lat, 4), round(segments$lon, 4), sep=","),
               segments$segment_id, function(c) {
                   x <- which(shape[ri, "coord"] %in% c)
                   if (length(x) >= 1) return(x) else return(NULL)
               }) -> x
        x <- x[!sapply(x, is.null)]
        shape$seg[ri] <- shape$seg[ri] + 
            cumsum(1:nrow(shape[ri, ]) %in% sapply(x, min)) / 1000
    }
    
    shapeIDs <- numeric()
    shapeDIR <- numeric()
    for (i in unique(shape$seg)) {
        if (!i %in% shape$seg[match]) {
            shapeIDs <- c(shapeIDs, shape[shape$seg == i, ]$segment_id[1])
            shapeDIR <- c(shapeDIR, 0)
            next
        }
        
        segments <- getSegments(.con)
        ri <- shape$seg == i
        
        ## we know it matches - which segment(s) does it match??
        seg.match <-
            tapply(with(segments, paste(round(lat, 4), round(lon, 4), sep = ",")),
                   segments$segment_id,
                   function(x) sum(x %in% shape[ri, "coord"]) >= min(10, sum(ri)))
        seg.match <- as.numeric(names(seg.match)[seg.match])

        ## with(shape[ri, ], addLines(lon, lat, gpar = list(col = "blue")))
        
        for (si in seq_along(seg.match)) {
            s <- seg.match[si]
            ss <- segments[segments$segment_id == s, ]
            SEGcoord <- with(ss, paste(round(lat, 4), round(lon, 4), sep = ","))
            match1 <- shape[ri, "coord"] %in% SEGcoord
            SEGS <- c(1, cumsum(abs(diff(match1))) + 1)
            ni <- tapply(1:length(SEGS), SEGS, length)
            mi <- tapply(match1, SEGS, mean)
            mi[mi == 0 & ni < 10] <- 1
            match2 <- as.logical(rep(mi, ni))      ## NEW in EXISTING
            
            match3 <- SEGcoord %in% shape$coord[ri][match2]
            SEGS <- c(1, cumsum(abs(diff(match3))) + 1)
            ni <- tapply(1:length(SEGS), SEGS, length)
            mi <- tapply(match3, SEGS, mean)
            ## any "short" non-matches -> matches
            mi[mi == 0 & ni < 10] <- 1
            match4 <- as.logical(rep(mi, ni))      ## EXISTING in NEW

            if (all(match4)) {
                ## Yay! everything matches, so set that as the ID
                shapeIDs <- c(shapeIDs, s)

                ## match direction:
                X <- shape[ri, c("lat", "lon")][match2, ][1, ]
                z <- which.min(apply(ss[c(1, nrow(ss)), c("lat", "lon")], 1,
                                     function(y) distanceFlat(y, as.numeric(X))))
                shapeDIR <- c(shapeDIR, z == 2)
            } else {
                cur.ids <- unique(segments$segment_id)
                ## it's a partial match - need to split the segment in the database!!
                SEGS <- c(1, cumsum(abs(diff(match4))) + 1)
                ## create NEW shapes
                seg.df <- NULL
                new.ids <- numeric()
                for (segj in unique(SEGS)) {
                    j <- which(SEGS == segj)
                    tmp <- ss[j, ]
                    tmp$shape_pt_sequence <- order(tmp$shape_pt_sequence)
                    tmp$distance <-
                        tmp$distance - min(tmp$distance)

                    ## with(tmp, addLines(lon, lat, gpar = list(col = "red")))
                    
                    newID <- if (length(cur.ids) == 0) 1 else max(cur.ids, na.rm = TRUE) + 1
                    cur.ids <- c(cur.ids, newID)
                    new.ids <- c(new.ids, newID)
                    tmp$segment_id <- newID
                    seg.df <- rbind(seg.df, tmp)
                    if (all(match4[j])) {
                        shapeIDs <- c(shapeIDs, newID)
                        X <- shape[ri, c("lat", "lon")][match2, ][1, ]
                        z <- which.min(apply(tmp[c(1, nrow(tmp)), c("lat", "lon")], 1,
                                             function(y) distanceFlat(y, as.numeric(X))))
                        shapeDIR <- c(shapeDIR, z == 2)
                    }
                }
                if (!is.null(seg.df)) {
                    colnames(seg.df) <- c("segment_id", "shape_pt_lat", "shape_pt_lon",
                                          "shape_pt_sequence", "distance_into_segment")
                    dbWriteTable(.con, "segments", seg.df, row.names = FALSE, append = TRUE)
                }
                
                ## fix up previous shapes:
                ## s -> new.ids
                sAffected <-
                    dbGetQuery(.con,
                               sprintf("SELECT shape_id FROM shapes_seg WHERE segment_id='%s'",
                                       s))$shape_id
                oldShapes <-
                    dbGetQuery(.con, sprintf("SELECT * FROM shapes_seg WHERE shape_id IN ('%s')",
                                             paste0(sAffected, collapse = "','")))
                if (nrow(oldShapes) > 0) {
                    tapply(1:nrow(oldShapes), oldShapes$shape_id,
                           function(k) {
                               oSk <- oldShapes[k, ]
                               WS <- which(oSk$segment_id == s)
                               nk <- nrow(oSk)
                               lapply(WS, function(wk) {
                                   newseq <- 1:length(new.ids)
                                   newIDS <- if (oSk$direction[wk] == 1) rev(new.ids) else new.ids
                                   alt <- if (wk == 1) {
                                              if (nk == 1) {
                                                  data.frame(
                                                      shape_id = oSk$shape_id[wk],
                                                      segment_id = newIDS,
                                                      segment_sequence =
                                                          as.numeric(oSk$segment_sequence[wk]) +
                                                          (1:length(new.ids))/10,
                                                      direction = oSk$direction[wk])
                                              } else {
                                                  rbind(data.frame(
                                                      shape_id = oSk$shape_id[wk],
                                                      segment_id = newIDS,
                                                      segment_sequence =
                                                          as.numeric(oSk$segment_sequence[wk]) +
                                                          (1:length(new.ids))/10,
                                                      direction = oSk$direction[wk]),
                                                      oSk[(wk+1):nk, ])
                                              }
                                          } else if (wk == nk) {
                                              rbind(oSk[1:(wk-1), ],
                                                    data.frame(
                                                        shape_id = oSk$shape_id[wk],
                                                        segment_id = newIDS,
                                                        segment_sequence =
                                                            as.numeric(oSk$segment_sequence[wk]) +
                                                            (1:length(new.ids))/10,
                                                      direction = oSk$direction[wk]))
                                          } else {
                                              rbind(oSk[1:(wk-1), ],
                                                    data.frame(
                                                        shape_id = oSk$shape_id[wk],
                                                        segment_id = newIDS,
                                                        segment_sequence =
                                                            as.numeric(oSk$segment_sequence[wk]) +
                                                            (1:length(new.ids))/10,
                                                      direction = oSk$direction[wk]),
                                                    oSk[(wk+1):nrow(oSk), ])
                                          }
                                   
                                   alt$segment_sequence <- order(as.numeric(alt$segment_sequence))
                                   alt
                               }) -> p1
                               do.call(rbind, p1)
                           }) -> newShapes
                    newShapes <- do.call(rbind, newShapes)
                    rownames(newShapes) <- NULL                    
                    dbGetQuery(.con, sprintf("DELETE FROM shapes_seg WHERE shape_id IN ('%s')",
                                             paste0(sAffected, collapse = "','")))
                    dbWriteTable(.con, "shapes_seg", newShapes, row.names = FALSE, append = TRUE)
                }
                ## and delete old segments
                dbGetQuery(.con, sprintf("DELETE FROM segments WHERE segment_id = '%s'", s))
            }
        }
    }
    
    ## write shape ids to file
    newShape <- data.frame(shape_id = id, segment_id = shapeIDs,
                           segment_sequence = 1:length(shapeIDs),
                           direction = shapeDIR)
    dbWriteTable(.con, "shapes_seg", newShape,
                 row.names = FALSE, append = TRUE)

    dbDisconnect(.con)
    
    if (verbose) cat("Done adding shape ", id, " to segment database.\n")

    if (plot) plotSegments(db = db)
    
    invisible(NULL)
}


createSegmentTable <- function(db = "db/gtfs-static2.db", yes = FALSE,
                               .con = dbConnect(SQLite(), db)) {
    add <- TRUE
    if ("shapes_seg" %in% dbListTables(.con)) {
        if (!yes)
            yes <- readline("Table `shapes_seg` exists ... overwrite? (y/n) ") == "y"
        if (yes) dbGetQuery(.con, "DROP TABLE shapes_seg")
        else add <- FALSE
    }
    if (add) {
        dbGetQuery(.con, "CREATE TABLE shapes_seg (shape_id VARCHAR(255), segment_id INTEGER,
                                                   segment_sequence INTEGER, direction INTEGER)")
        cat("New table `shapes_seg` created.\n")
    }
    
    add <- TRUE
    if ("segments" %in% dbListTables(.con)) {
        if (!yes)
            yes <- readline("Table `segments` exists ... overwrite? (y/n) ") == "y"
        if (yes) dbGetQuery(.con, "DROP TABLE segments")
        else add <- FALSE
    }
    if (add) {
        dbGetQuery(.con, "CREATE TABLE segments (segment_id INTEGER, shape_pt_lat TEXT,
                                                 shape_pt_lon TEXT, shape_pt_sequence INTEGER,
                                                 distance_into_segment TEXT)")
        cat("New table `segments` created.\n")
    }

    dbDisconnect(.con)
    invisible(NULL)
}



getSegments <- function(.con) {
    dbGetQuery(.con,
               "SELECT segment_id, CAST(shape_pt_lat AS REAL) AS lat,
                                   CAST(shape_pt_lon AS REAL) AS lon,
                       shape_pt_sequence, CAST(distance_into_segment AS REAL) AS distance
                  FROM segments ORDER BY segment_id, shape_pt_sequence")
}



plotSegments <- function(id, db = "db/gtfs-static2.db",
                         col = NULL, alpha = ifelse(shapes, 0.8, 1), lwd = 2,
                         ..., .con = dbConnect(SQLite(), db)) {
    if (missing(id)) {
        shapes <- FALSE
        segments <- dbGetQuery(.con,
                               "SELECT segment_id, shape_pt_sequence,
                                       CAST(shape_pt_lat AS REAL) AS lat,
                                       CAST(shape_pt_lon AS REAL) AS lon,
                                       CAST(distance_into_segment AS REAL) AS distance
                                  FROM segments
                              ORDER BY segment_id, shape_pt_sequence")
    } else {
        shapes <- TRUE
        segments <-
            dbGetQuery(.con,
                       sql <- sprintf("SELECT sh.shape_id, sh.segment_id, sh.segment_sequence,
                                       sh.direction, sg.shape_pt_sequence,
                                       CAST(sg.shape_pt_lat AS REAL) AS lat,
                                       CAST(sg.shape_pt_lon AS REAL) AS lon,
                                       CAST(sg.distance_into_segment AS REAL) AS distance
                                  FROM shapes_seg AS sh, segments AS sg
                                 WHERE sh.segment_id=sg.segment_id
                                   AND sh.shape_id IN ('%s')
                              ORDER BY sh.shape_id, sh.segment_sequence, sg.shape_pt_sequence",
                              paste(id, collapse = "','")))

        ## fix up directions
        A <- tapply(1:nrow(segments),
                    paste(segments$shape_id, segments$segment_sequence),
                    function(i)
                        if (all(segments$direction[i] == 1)) rev(i) else i)
        segments <-
            segments[as.numeric(
                do.call(c, A[unique(paste(segments$shape_id, segments$segment_sequence))])
            ), ]
    }

    mobj <- iNZightMap(~lat, ~lon, data = segments, name = "Bus Route Segments")
    do.call(plot, c(list(x = mobj, pch = NA)))

    grid::grid.rect(gp = grid::gpar(fill = "white", alpha = 0.6))

    if (shapes) {
        if (is.function(col))
            cols <- col(length(unique(segments$shape_id)))
        else
            cols <- rainbow_hcl(length(unique(segments$shape_id)), alpha = alpha)

        addLines(segments$lon, segments$lat, id = as.numeric(as.factor(segments$shape_id)),
                 gp = list(col = cols, lwd = lwd))
    } else {
        addLines(segments$lon, segments$lat, id = segments$segment_id,
                 gp = list(col = col, lwd = lwd, alpha = alpha))
        ## points at boundaries
        with(segments[do.call(c, tapply(1:nrow(segments), segments$segment_id, range)), ],
             addPoints(lon, lat, pch = 19, gp = list(cex = 0.4)))
    }
}
