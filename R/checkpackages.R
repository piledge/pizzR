dependencies <- c("caret", "crayon", "dbscan", "doParallel", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "magrittr", "memuse", "parallelly", "randomForest", "ranger",
                  "raster", "Rcpp", "remotes", "rvest", "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse", "xml2")

to_install <- !dependencies %in% rownames(installed.packages())
if (any(to_install)){
  missing_dependencies <- as.character(dependencies[to_install])
  cat("\n\nMissing dependencies:", paste(dependencies[to_install], sep = ',', collapse= ", "), "\n")
  cat("use 'pizzR::package.install(pizzR::dependencies)' to add\n\n")
}
