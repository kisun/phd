#' Measurement function
#'
#' Convert a distance-into-trip value to a latitude/longitude position.
#'
#' @param x numeric, distance into trip to be converted
#' @param shape a matrix with three columns, latitude, longitude, and distance-into-trip, respectively
#'
#' @return latitude and longitude values (or a matrix if x is a vector)
h <- function(x, shape) {
  sapply(x, function(z) {
    # if (is.na(z[1])) print(z)
    if (z[1] <= 0) return(as.numeric(shape[1, 1:2]))
    if (z[1] >= max(shape[, 3]))
        return(as.numeric(shape[nrow(shape), 1:2]))

    j <- which.min(z[1] > shape[, 3]) - 1
    sj <- shape[j, ]

    d <- z[1] - sj[3]

    ## only do the calculations if we need to!
    if (d < 1)
      return(as.numeric(sj[1:2]))
    else {
      ## simply pythagorean distance?!
      p <- as.matrix(shape[j:(j+1), 1:2])
      mode(p) <- "numeric"
      lam0 <- p[1, 2]
      phi1 <- p[1, 1]
      xx <- (p[, 2] - lam0) * cos(phi1)
      yy <- p[, 1] - phi1

      prop <- as.numeric(d / diff(shape[j:(j+1), 3]))
      return(c(prop * yy[2] + phi1, prop * xx[2] / cos(phi1) + lam0))
    }
  })
}
