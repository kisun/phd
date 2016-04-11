setwd("~/Documents/uni/phd/code/auckland_transport")
loadall <- function()
    invisible(sapply(list.files("src/R", pattern = "R$",
                                all.files = TRUE, full.names = TRUE),
                     source))
loadall()

## createSegmentTable(yes = TRUE)
allIDs <- dbGetQuery(dbConnect(SQLite(), "db/gtfs-static2.db"),
                     "SELECT DISTINCT shape_id FROM trips")$shape_id

## i <- 0
## i <- i + 1; shape2seg(id = allIDs[i])

pb <- txtProgressBar(0, length(allIDs), style = 3)
for (i in 1:length(allIDs)) {
    shape2seg(id = allIDs[i], plot = i %% 10 == 0)
    setTxtProgressBar(pb, i)
    ## grid::grid.locator()
}; close(pb)


plotSegments(id = "1176-20160316100058_v39.6")
plotSegments(id = "1177-20160316100058_v39.6")
plotSegments(id = "1178-20160316100058_v39.6")

allIDs

#X[as.numeric(do.call(c, tapply(1:nrow(X), paste(X$shape_id, X$segment_sequence),
#                             function(i) if (all(X$direction[i] == 1)) rev(i) else  i))), ]


for (i in unique(X$segment_sequence)) {
    with(X[X$segment_sequence == i, ], addLines(lon, lat))
    with(X[X$segment_sequence == i, ], addPoints(lon[1], lat[1]))
    grid::grid.locator()
}
