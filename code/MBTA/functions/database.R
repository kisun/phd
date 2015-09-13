### Functions for interacting the an SQLite database
require(RSQLite)

query <- function(con, sql, ..., debug = FALSE) {
    dots <- list(...)
    dots <- lapply(dots, function(x) {
        if (length(x) > 1)
            paste0("('", paste(x, collapse = "', '"), "')")
        else
            paste0("'", x, "'")
    })

    XX <- do.call(sprintf, c(sql, dots))

    if (debug) print(XX)

    q <- try(dbGetQuery(con, XX),
             silent = TRUE)

    false <- FALSE
    if (inherits(q, "try-error")) {
        ## Just incase the database is being written to (SQLite problem):
        Sys.sleep(2)
        q <- try(dbGetQuery(con, XX),
                 silent = TRUE)
        if (inherits(q, "try-error")) {
            print(XX)
            stop("The above query failed:\n", q)
        }
    }
    
    q
}
