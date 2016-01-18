## R library location:
lib = ".Rlibrary"

## Packages to install from CRAN:

packages = c(
  "RSQLite"
)

install.packages(packages,
                 lib = lib,
                 repos = "http://cran.stat.auckland.ac.nz")
