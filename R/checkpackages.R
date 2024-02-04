dependencies <- c("caret", "crayon", "doParallel", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "magrittr", "memuse", "parallelly", "randomForest", "ranger",
              "raster", "Rcpp", "remotes", "rvest", "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse", "xml2")

to_install <- !dependencies %in% installed.packages()
if (any(to_install)){
  cat("\n\n\nDependencies missing:", paste0(dependencies[to_install], sep=""), "\n")
  cat("use 'pizzR::package.install(pizzR::dependencies)' to add\n\n\n\n")
}
