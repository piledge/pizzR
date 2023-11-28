packages <- c("caret", "doParallel", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "magrittr", "memuse", "parallelly", "randomForest", "ranger",
              "raster", "Rcpp", "remotes", "rvest", "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse", "xml2")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
  cat("\n\n\nPackages missing:", paste0(packages[to_install], sep=""), "\n")
  cat("use 'pizzR::package.install(pizzR::packages)' to add\n\n\n\n")
}
