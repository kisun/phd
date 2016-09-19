## R library location:
lib = ".Rlibrary"

## Packages to install from CRAN:

packages = c(
    "DBI", "RSQLite",
    "BH", "devtools",
    "abind",
    "knitr", "xtable",
    "msm",
    "colorspace", "dichromat",
    "RPostgreSQL", "jsonlite"
)

packages = packages[! packages %in% installed.packages(lib = lib)[, 'Package']]

options(repos = "http://cran.stat.auckland.ac.nz")

install.packages(packages,
                 lib = lib,
                 dependencies = c("Depends", "Imports"))


## Packages to install from GitHub:
.libPaths(lib)
devtools::install_github("iNZightVIT/iNZightRegression")
devtools::install_github("iNZightVIT/iNZightMR")
devtools::install_github("iNZightVIT/iNZightPlots")
devtools::install_github("iNZightVIT/iNZightMaps")
