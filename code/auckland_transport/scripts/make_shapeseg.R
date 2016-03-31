setwd("~/Documents/uni/phd/code/auckland_transport")
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))
loadall()

createSegmentTable(yes = TRUE)
allIDs <- dbGetQuery(dbConnect(SQLite(), "db/gtfs-static2.db"),
                     "SELECT DISTINCT shape_id FROM shapes")$shape_id
i <- 0

i <- i + 1; shape2seg(id = allIDs[i])

pb <- txtProgressBar(0, length(allIDs), style = 3)
for (i in 26:nrow(allIDs)) {
    shape2seg(id = allIDs[i])
    setTxtProgressBar(pb, i)
    grid::grid.locator()
}; close(pb)
