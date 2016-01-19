## a suite of time-based functions

ts2dt <- function(ts, origin = "1970-01-01") {
  as.POSIXct(ts, origin = origin)
}

tsTime <- function(ts) {
    format(ts2dt(ts), format = "%H:%M:%S")
}

tsDate <- function(ts) {
    format(ts2dt(ts), format = "%Y-%m-%d")
}
