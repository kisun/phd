setwd("../")
.libPaths("../../.Rlibrary")

library(RPostgreSQL)
library(jsonlite)
library(iNZightPlots)
library(iNZightMaps)
library(mvtnorm)
source("src/pf.R")
source("src/mapping.R")
source("src/h.R")

drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "homestead", host = "localhost",
                user = "homestead", port = "54320", password = "secret")
con2 = dbConnect(drv, dbname = "historical", host = "localhost",
                 user = "homestead", port = "54320", password = "secret")

hist <- dbGetQuery(con2, "SELECT vehicle_id, count(vehicle_id) as n FROM vehicle_positions group by vehicle_id")
vid <- "3A9A"
vps <- dbGetQuery(
    con2,
    sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s' AND trip_id LIKE '%s' ORDER BY timestamp",
            vid, '%v46.5'))

for (i in 1:nrow(vps))
    pf(con, vid, 500, sig.gps = 5, draw = TRUE, vp = vps[i, ])

i <- 1
pf(con, vid, 500, sig.gps = 5, draw = TRUE, vp = vps[i, ]); i <- i + 1




###
print(pf(con, vid, sig.gps = 50, draw = TRUE,
         vp = ))

ret <- 0
while(ret <= 0) {
    cat(".")
    ret <- pf(con, vid, 1000, sig.gps = 5)
    Sys.sleep(0.5)
}


##
devtools::load_all("~/iNZight/iNZightPlots")
devtools::load_all("~/iNZight/iNZightMaps")

##
vp <- dbGetQuery(con, sprintf("SELECT * FROM vehicle_positions WHERE vehicle_id='%s'", vs[5]))



mu <- with(vp, c(position_longitude, position_latitude))
sigma <- diag(2) * 5
z <- outer(x <- seq(mu[1] - 1, mu[1] + 1, length.out = 500),
           y <- seq(mu[2] - 1, mu[2] + 1, length.out = 500),
           function(x, y) dmvnorm(cbind(x, y), mean = mu, sigma = sigma))
filled.contour(x, y, z, color.palette=viridis::magma)
