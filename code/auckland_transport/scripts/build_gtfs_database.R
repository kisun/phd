## A standalone script to set up the GTFS databases ...

.libPaths("../../.Rlibrary")

require("RSQLite")
con = dbConnect(SQLite(), "db/gtfs-static.db")

## Creates the GTFS database:
files = gsub(".txt", "", list.files("_data/gtfs-latest", ".txt"))

for (i in seq_along(files)) {
    table = files[i]
    cat("\nWriting table", table, "...")
    f = read.csv(paste0("_data/gtfs-latest/", table, ".txt"))

    ## decimals as strings
    dec.cols = !sapply(f, is.integer) & sapply(f, is.numeric)
    if (any(dec.cols)) {
        f[, dec.cols] = lapply(f[, dec.cols, drop = FALSE], as.character)
    }

    if (dbWriteTable(con, table, f, append = FALSE, overwrite = TRUE))
        cat(" complete")
    else
        cat(" failed")
}

cat("\n\n")
