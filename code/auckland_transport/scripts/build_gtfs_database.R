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
         shapes = "shapes",
         stops = "stops",
         stop_times = "stopTimes",
         trips = "trips")

api <- readLines("apikey.txt")

for (i in seq_along(files)) {
    table = names(files)[i]
    url = sprintf("https://api.at.govt.nz/v1/gtfs/%s?api_key=%s", files[[i]], api)
    print(url)
    cat("\nWriting table", table, "...")
    ##f = read.csv(file, header = TRUE, stringsAsFactors = FALSE)
    f = fromJSON(url)$response

    ## decimals as strings
    dec.cols = !sapply(f, is.integer) & sapply(f, is.numeric)
    if (any(dec.cols)) {
        f[, dec.cols] = lapply(f[, dec.cols, drop = FALSE], as.character)
    }

    ## remove versioning from IDs
    idcols = grepl("_id", colnames(f))
    if (any(idcols)) {
        y = lapply(f[, idcols], function(x) {
                       if (!all(is.na(x)) & any(grepl("_v", x)))
                           gsub(".+-.+_v", "", as.character(x))
                       else NULL
                   })

        y = y[!sapply(y, is.null)]
        if (length(y))
            f$id_version = apply(do.call(cbind, y), 1, max, na.rm = TRUE)

        f[, idcols] = lapply(f[, idcols, drop = FALSE], function(x) {
                                 if (!all(is.na(x)))
                                     gsub("-.+", "", as.character(x))
                                 else x
                             })
    }

    if (dbWriteTable(con, table, f, append = FALSE, overwrite = TRUE))
        cat(" complete")
    else
        cat(" failed")
}
