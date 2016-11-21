library(grid)

plotHistory <- function(hist) {
    xlim <- range(hist$t)
    ylim <- c(0, 16)
    grid.newpage()
    grid.rect(gp = gpar(fill = "#333333"))
    pushViewport(viewport(width = 0.9, height = 0.9, layout = grid.layout(nrow = nrow(hist$mean)-1)))
    xx <- rep(hist$t, each = 4)
    xx <- xx[-(1:2)]
    xx <- xx[1:(length(xx) - 2)]
    yy <- rep(c(0, 1, 1, 0), length.out = length(xx))
    for (i in 2:M-1) {
        pushViewport(viewport(layout.pos.row = i, xscale = xlim, yscale = ylim, clip = TRUE))
        if (i %% 2 == 0) grid.rect(gp = gpar(fill = "#444444", lwd = 0))
        spd <- round(hist$mean[i, ] / 16 * 11)
        cols <- RColorBrewer::brewer.pal(11, "RdYlGn")[spd]
        grid.polygon(unit(xx, units = "native"),
                     unit(yy, units = "npc"),
                     id.lengths = rep(4, length(xx) / 4),
                     gp = gpar(lwd = NA, col = cols, fill = cols))
        ##grid.polygon(c(hist$t, rev(hist$t)),
        ##             c(hist$mean[i, ] + hist$var[i, ], rev(hist$mean[i, ] - hist$var[i, ])),
        ##             default.units = "native", gp = gpar(lwd = 0, fill = "#999999"))
        y2 <- rbind(hist$mean[i, ] - hist$var[i, ], hist$mean[i, ] + hist$var[i, ])
        grid.polyline(rep(hist$t, each = 2), c(y2), default.units = "native", id = rep(1:ncol(y2), each = 2),
                      gp = gpar(col = "#33333380"))
        grid.lines(hist$t + 30 * 5, hist$mean[i, ], default.units = "native",
                   gp = gpar(lwd = 2))
        popViewport()
    }

    pushViewport(viewport(xscale = xlim, yscale = c(M-1, 0)))
    tt <- as.POSIXct(hist$t, origin = "1970-01-01")
    tta <- pretty(tt, min.n = 6)
    grid.xaxis(at = tta, label = gsub("^0", "", format(tta, "%I%P")), gp = gpar(col = "#cccccc"))
    grid.yaxis(at = 1:(M-1) - 0.5, label = 1:(M-1), gp = gpar(lwd = 0, col = "#cccccc"))
    grid.text("Segment", x = unit(-3, "line"), y = 0.5, rot = 90, gp = gpar(col = "#cccccc"))
}


updateSpeeds <- function(con, q = 1, A = diag(M), H = diag(M), t2 = as.integer(Sys.time())) {
    res <- dbGetQuery(con, "SELECT segment_id, speed_mean, speed_var, timestamp FROM segment_speeds WHERE current")

    M <- nrow(res)
    t <- res$timestamp[1]
    obs <- dbGetQuery(
        con,
        sprintf("SELECT MAX(segment_shapes.segment_id) AS segment_id, AVG(velocity) AS mean, VAR_SAMP(velocity) AS sd FROM particles, trips, segment_shapes WHERE particles.trip_id=trips.id AND trips.shape_id=segment_shapes.id AND segment_shapes.leg=segment_index AND timestamp BETWEEN %s AND %s GROUP BY segment_index ORDER BY segment_index",
                t, t2))
    Obs <- data.frame(segment_id = res$segment_id, mean = res$speed_mean, sd = rep(1e6, M))
    rownames(Obs) <- Obs$segment_id
    Obs[as.character(obs$segment_id), ] <- obs
    Obs[is.na(Obs$sd), "sd"] <- 10

    B <- cbind(res$speed_mean)
    P <- res$speed_var * diag(M)
    Q <- q * diag(M)

    ## Predict
    Bk <- A %*% B
    Pk <- A %*% P %*% t(A) + Q

    ## Update
    yk <- Obs$mean - H %*% Bk
    Rk <- diag(Obs$sd)
    Sk <- H %*% Pk %*% t(H) + Rk
    Kk <- Pk %*% t(H) %*% solve(Sk)

    B <- Bk + Kk %*% yk
    P <- (diag(M) - Kk %*% H) %*% Pk

    ## do updates:
    q1 <- dbGetQuery(con, "UPDATE segment_speeds SET current = FALSE")
    q2 <- dbGetQuery(con, sprintf("INSERT INTO segment_speeds (segment_id, speed_mean, speed_var, timestamp, current) VALUES ('%s')",
                                  paste0(res$segment_id, "','", B, "','", pmax(0.000001, diag(P)), "','",
                                         t2, "','TRUE",
                                         collapse="'),('")))

    return(invisible(NULL))
}
plotSpeeds <- function(res, shape = NULL) {
    B <- res$B
    fr <- round(pmin(11, B[,1]/15 * 11))
    if (is.null(shape)) {
        plot(ds, rep(0, M), type = "n", yaxt = "n", ylab = "n", bty = "n",
             main = format(as.POSIXct(res$t, origin = "1970-01-01")))
        abline(v = ds, lty = 3)
        arrows(ds[-M], 0, ds[-1], code = 0, col = RColorBrewer::brewer.pal(11, "RdYlGn")[fr], lwd = 10, lend = 1)
        text(0.5 * (ds[-1] - ds[-M]) + ds[-M], 0.2, labels = round(B[-M]))
    } else {
        library(iNZightMaps)
        mobj <- iNZightMap(~lat, ~lon, data = shape, name = "Route 274: Britomart to Three Kings")
        plot(mobj, pch = NA, join = TRUE, lwd = 6, col.line = "black",
             varnames = list(colby = "Speed (m/s)"),
             subtitle = format(as.POSIXct(res$t, origin = "1970-01-01")),
             col.fun = colorRampPalette(c("green4", "yellow", "red")))
        spd <- round(res$B / 16 * 11)
        cols <- RColorBrewer::brewer.pal(11, "RdYlGn")[spd]
        addLines(shape$lat, shape$lon, id = shape$segment,
                 gpar = list(col = cols, lwd = 4))
    }
}


drawSegments <- function(shape, schedule, speeds, times, true, MAX.speed = 100 * 1000 / 60^2) {
    var <- NULL
    if (class(speeds) == "list") {
        segids <- unique(shape$segment_id)
        speeds <- BHist$mean[as.character(segids), ]
        times <- (BHist$t - min(BHist$t)) / 60
        var <- BHist$var[as.character(segids), ]
    } else {
        speeds <- cbind(speeds)
        if (missing(times)) times <- seq(0, nrow(speeds), by = 1)
    }
    ## Sd <- schedule$pivot.shape_dist_traveled
    Rd <- c(0, tapply(shape$dist_traveled, shape$leg, max))
    o <- par(mfrow = c(1, 1), bg = "#333333", fg = "#cccccc", col.axis = "#cccccc",
             col.lab = "#cccccc", col.main = "#cccccc")
    plot(NA, type = "n", xlab = "Time (minutes)", ylab = "Segment",
         xlim = range(times), xaxs = "i",
         ylim = c(max(Rd), 0), yaxt = "n", yaxs = "i")
    axis(2, at = Rd[-1] - diff(Rd) / 2, labels = 1:(length(Rd) - 1), las = 1, tick = FALSE, cex.axis = 0.8)
    spd <- round(speeds / MAX.speed * 10) + 1
    cols <- apply(spd, 1, function(x) RColorBrewer::brewer.pal(11, "RdYlGn")[x])
    cols <- if (is.null(dim(cols))) cbind(cols) else t(cols)
    for (i in 1:nrow(speeds)) {
        rect(times[-length(times)], rep(Rd[i], length(times) - 1),
             times[-1], rep(Rd[i + 1], length(times) - 1),
             border = cols[i, ], col = cols[i, ])
        if (!is.null(var)) {
            polygon(c(times, rev(times)),
                    c(Rd[i] + (Rd[i + 1] - Rd[i]) * (1 - pmin((speeds[i, ] + sqrt(var[i, ])) / MAX.speed, 1)),
                      rev(Rd[i] + (Rd[i + 1] - Rd[i]) * (1 - pmax(0, (speeds[i, ] - sqrt(var[i, ])) / MAX.speed)))),
                    border = NULL, col = "#33333320")
            lines(times, Rd[i] + (Rd[i + 1] - Rd[i]) * (1 - speeds[i, ] / MAX.speed), lwd = 1, col = "#333333")
        }
        if (!missing(true)) {
            lines(times, Sd[i] + (Sd[i + 1] - Sd[i]) * (1 - true[i, ] / MAX.speed), col = "#222222", lty = 2, type = "s")
        }
    }
    abline(h = Rd[2:(length(Rd) - 1)], col = "#33333330")
    par(o)
}
