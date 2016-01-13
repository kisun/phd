files <- list.files("gtfs", full.names = TRUE)

for (file in files) {
  f <- read.csv(file, header = TRUE)
  tostr <- !sapply(f, is.integer)
  for (i in which(tostr)) f[, i] <- as.character(f[, i])
  write.csv(f, file, row.names = FALSE, quote = TRUE, na = "")
}
