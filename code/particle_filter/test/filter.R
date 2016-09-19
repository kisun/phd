library(RPostgreSQL)
library(jsonlite)

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")


vs <- dbGetQuery(con, "SELECT vehicle_id FROM vehicle_positions")$vehicle_id

source("pf.R")
pf(con, vs[1], 10)
