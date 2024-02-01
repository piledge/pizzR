packages <- c("caret", "crayon", "doParallel", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "magrittr", "memuse", "parallelly", "randomForest", "ranger",
              "raster", "Rcpp", "remotes", "rvest", "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse", "xml2")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
  cat("\n\n\nDependencies missing. Installing:", paste0(packages[to_install], sep=""), "\n")
  install.packages(packages[to_install], dependencies = T)
  cat("\n\n\nDependencies installed:", paste0(packages[to_install], sep=""), "\n")
}
library(magrittr)
