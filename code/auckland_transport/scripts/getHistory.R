#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) stop("Specify date.")

##### 

setwd("~/Documents/uni/phd/code/auckland_transport")
invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))

lapply(args, function(date) try(collectHistory(day = date)))




