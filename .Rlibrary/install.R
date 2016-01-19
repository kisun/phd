## R library location:
lib = ".Rlibrary"

## Packages to install from CRAN:

packages = c(
  "DBI", "RSQLite"
)

install.packages(packages,
                 lib = lib,
                 repos = "http://cran.stat.auckland.ac.nz",
                 dependencies = c("Depends", "Imports"))
