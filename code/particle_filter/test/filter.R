setwd("../")
.libPaths("../../../.Rlibrary")

library(RPostgreSQL)
library(jsonlite)
source(pipe("ssh -p 2222 vagrant@localhost 'cat /home/vagrant/particle_filter/src/pf.R'"))

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")

vs <- dbGetQuery(con, "SELECT vehicle_id FROM vehicle_positions")$vehicle_id


p <- pf(con, vs[1], 10)
p
