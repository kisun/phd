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
    if (is.na(z[1])) print(z)
    if (z[1] <= 0) return(as.numeric(shape[1, 1:2]))
    if (z[1] >= max(shape[, 3]))
        return(as.numeric(shape[nrow(shape), 1:2]))

    j <- which.min(z[1] > shape[, 3]) - 1
    sj <- shape[j, ]
    #Psi <- sj$bearing
    d <- z[1] - sj[3]

    ## only do the calculations if we need to!
    if (d < 2)
      return(as.numeric(sj[1:2]))
    else
      return(Inf)
  })
}
