## A standalone script to set up the GTFS databases ...

.libPaths("../../.Rlibrary")

require("RSQLite")
con = dbConnect(SQLite(), "db/gtfs-static.db")

require("jsonlite")

## Creates the GTFS database:
##files = gsub(".txt", "", list.files("_data/gtfs"))
files =
    list(agency = "agency",
         calendar = "calendar",
         calendar_dates = "calendarDate",
         routes = "routes",
         stops = "stops",
         trips = "trips")

api <- readLines("apikey.txt")

for (i in seq_along(files)) {
    table = names(files)[i]
    url = sprintf("https://api.at.govt.nz/v1/gtfs/%s?api_key=%s", files[[i]], api)
    cat("\nWriting table", table, "...")
    f = fromJSON(url)$response

    ## decimals as strings
    dec.cols = !sapply(f, is.integer) & sapply(f, is.numeric)
    if (any(dec.cols)) {
        f[, dec.cols] = lapply(f[, dec.cols, drop = FALSE], as.character)
    }

    ## remove versioning from IDs
    ## idcols = grepl("_id", colnames(f))
    ## if (any(idcols)) {
    ##     y = lapply(f[, idcols], function(x) {
    ##                    if (!all(is.na(x)) & any(grepl("_v", x)))
    ##                        gsub(".+-.+_v", "", as.character(x))
    ##                    else NULL
    ##                })

    ##     y = y[!sapply(y, is.null)]
    ##     if (length(y))
    ##         f$id_version = apply(do.call(cbind, y), 1, max, na.rm = TRUE)

    ##     f[, idcols] = lapply(f[, idcols, drop = FALSE], function(x) {
    ##                              if (!all(is.na(x)))
    ##                                  gsub("-.+", "", as.character(x))
    ##                              else x
    ##                          })
    ## }

    if (table == "trips")
        .trips = f

    if (dbWriteTable(con, table, f, append = FALSE, overwrite = TRUE))
        cat(" complete")
    else
        cat(" failed")
}


table = "shapes"
cat("\nCreating table shapes ...")
shape.ids = unique(.trips$shape_id)
url = sapply(shape.ids, function(id)
    sprintf("http://api.at.govt.nz/v1/gtfs/shapes/shapeId/%s?api_key=%s", id, api))

f = fromJSON(url[1])$response
if (dbWriteTable(con, table, f, append = FALSE, overwrite = TRUE)) {
    cat(" complete")
} else {
    cat(" failed")
}

table = "stop_times"
cat("\nCreating table stop_times ...")
trip.ids = unique(.trips$trip_id)
url = sapply(trip.ids, function(id)
    sprintf("http://api.at.govt.nz/v1/gtfs/stopTimes/tripId/%s?api_key=%s", id, api))

## only use the first one to create the DB
f =  fromJSON(url[1])$response
if (dbWriteTable(con, table, f, append = FALSE, overwrite = TRUE)) {
    cat(" complete")
} else {
    cat(" failed")
}

cat("\n\n")
