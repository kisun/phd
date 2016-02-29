#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) stop("Specify date.")

if (length(args) > 1) warning("Multiple arguments, using the first.")

date <- args[1]

##### 

setwd("~/Documents/uni/phd/code/auckland_transport")
invisible(sapply(list.files("src/R", pattern = "R$", all.files = TRUE, full.names = TRUE), source))

collectHistory(day = date)
