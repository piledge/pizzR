packages <- c("caret", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "memuse", "randomForest", "ranger",
              "raster", "remotes", "rgdal", "RMariaDB", "s2", "sf", "terra", "tidyverse")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
  cat("\n\n\nPackages missing:", paste0(packages[to_install], sep=""), "\n")
  cat("use 'pizzR::mypackage.install(pizzR::packages)' to add\n\n\n\n")
}
