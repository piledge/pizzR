dependencies <- c("caret", "crayon", "dbscan", "doParallel", "e1071",
                  "feather", "foreach", "gdata", "ggplot2", "lidR",
                  "magrittr", "memuse", "parallelly", "randomForest",
                  "ranger", "raster", "Rcpp", "remotes", "rvest",
                  "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse",
                  "xml2", "piledge/pizzR", "MrFlywheel/kaiserschmRn")


to_install <- !dependencies %in% rownames(installed.packages())

if (any(to_install)) {
  missing_dependencies <- as.character(dependencies[to_install])
  cat("\n\nMissing dependencies:", paste(missing_dependencies, collapse = ", "), "\n")
  cat("Use 'pizzR::package.install(pizzR::dependencies)' to add missing packages.\n\n")
} else {
  cat("All dependencies are installed.\n")
}
