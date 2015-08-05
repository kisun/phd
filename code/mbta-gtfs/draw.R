f1 <- read.csv("feed.csv", header = TRUE)
with(f1, plot(lon, lat))

system("python example.py")
f2 <- read.csv("feed.csv", header = TRUE)
with(f2, plot(lon, lat))
