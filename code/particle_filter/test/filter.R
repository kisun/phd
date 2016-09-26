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

vs <- dbGetQuery(con, "SELECT vehicle_id FROM vehicle_positions")$vehicle_id
vid <- vs[240]
print(pf(con, vid, 500, sig.gps = 5, draw=TRUE))



###
vid <- "3AA1"
print(pf(con, vid, sig.gps = 50))

ret <- 0
while(ret <= 0) {
    cat(".")
    ret <- pf(con, vid, sig.gps = 5, draw=TRUE)
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
