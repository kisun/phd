## Functions for working with dates and times
require(lubridate)

convertTimestamp <- function(timestamp, to = "datetime", tz = "EST5EDT") {
    obj <- as.POSIXct(timestamp, tz = tz, origin = "1970-01-01")

    switch(to,
           "datetime" = obj,
           "time" = format(obj, "%H:%M:%S"),
           "date" = as.Date(obj))
           
}

time2seconds <- function(time) {
    time <- hms(time)
    (hour(time) * 60 + minute(time)) * 60 + second(time)
}
