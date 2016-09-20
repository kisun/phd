## R library location:
lib = ".Rlibrary"

## Packages to install from CRAN:

packages = c(
    "DBI", "RSQLite",
    "BH", "devtools",
    "abind",
    "knitr", "xtable",
    "msm",
    "colorspace", "dichromat", "viridis",
    "RPostgreSQL", "jsonlite",
    "iNZightRegression", "iNZightMR", "iNZightPlots", "iNZightMaps"
)

packages = packages[! packages %in% installed.packages(lib = lib)[, 'Package']]

options(repos = c("http://r.docker.stat.auckland.ac.nz", "https://cran.stat.auckland.ac.nz"))

install.packages(packages,
                 lib = lib,
                 dependencies = c("Depends", "Imports"))


## Packages to install from GitHub:
# .libPaths(lib)
# devtools::install_github("iNZightVIT/iNZightRegression")
# devtools::install_github("iNZightVIT/iNZightMR")
# devtools::install_github("iNZightVIT/iNZightPlots")
# devtools::install_github("iNZightVIT/iNZightMaps")
