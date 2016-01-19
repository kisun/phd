## functions for reading GTFS database

getPositions <- function(con, route.id, ...) {
    ## SQL preparation ...
    where <- character()
    sql <- "SELECT * FROM vehicle_positions"

    if (!missing(route.id))
        where <- c(where, sprintf("route_id LIKE '%s%s'", route.id, "%"))

    if (length(where))
        sql <- paste0(sql, " WHERE ", paste0(where, collapse = ","))

    print(sql)
    ## SQL call 
    pos <- dbGetQuery(con, sql)

    ## Value fixes
    pos$trip_start_date <- ifelse(nchar(pos$trip_start_date) == 0,
                                  tsDate(pos$timestamp),
                                  pos$trip_start_date)
    strip.cols <- c("trip_id", "route_id")
    pos[, strip.cols] <- lapply(pos[, strip.cols], function(x) gsub("-.+", "", x))

    pos
}
