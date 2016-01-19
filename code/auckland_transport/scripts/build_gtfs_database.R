## A standalone script to set up the GTFS databases ...

.libPaths("../../.Rlibrary")

require("RSQLite")
con = dbConnect(SQLite(), "db/gtfs-static.db")

## Creates the GTFS database:
files = list.files("_data/gtfs", full.names = TRUE)

for (file in files) {
    table = gsub("_data/gtfs/|.txt", "", file)
    cat("\nWriting table", table, "...")
    f = read.csv(file, header = TRUE, stringsAsFactors = FALSE)

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
