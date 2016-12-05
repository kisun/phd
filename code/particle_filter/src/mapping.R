addPoints <- function(lat, lon, gpar, ...) {
    ## --- Add points to an iNZightMap plot

    ## Set up the viewport:
    setVP()

    ## Transform the points:
    xy <- iNZightMaps::latlon.xy(data.frame(lat = lat, lon = lon),
                                 map = global.objects$maps$map)

    ## Draw the points:
    if (missing(gpar)) {
        gpar <- grid::gpar()
    } else {
        gpar <- grid:::validGP(gpar)
        class(gpar) <- "gpar"
    }
    
        
    grid::grid.points(xy$newX, xy$newY, gp = gpar, ...)

    invisible(NULL)
}

addLines <- function(lat, lon, gpar, ...) {
    ## --- Add lines to an iNZightMap plot

    ## Set up the viewport:
    setVP()

    ## Transform the points:
    xy <- iNZightMaps::latlon.xy(data.frame(lat = lat, lon = lon),
                                 map = global.objects$maps$map)

    ## Draw the lines:
    if (missing(gpar)) {
        gpar <- grid::gpar()
    } else {
        gpar <- grid:::validGP(gpar)
        class(gpar) <- "gpar"
    }
    
        
    grid::grid.polyline(xy$newX, xy$newY, default.units = "native", gp = gpar, ...)

    invisible(NULL)
}

setVP <- function() {
    lim <- iNZightMaps:::map.xylim(grid::current.viewport()$yscale,
                                   grid::current.viewport()$xscale,
                                   SCALE = 2)$window.lim
    grid::pushViewport(grid::viewport(xscale = lim[1:2], yscale = lim[3:4], clip = TRUE))

    invisible(NULL)
}
