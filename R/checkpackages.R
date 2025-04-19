dependencies <- c("caret", "crayon", "dbscan", "doParallel", "e1071",
                  "feather", "foreach", "gdata", "ggplot2", "lidR",
                  "magrittr", "memuse", "parallelly", "randomForest",
                  "ranger", "raster", "Rcpp", "remotes", "rvest",
                  "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse",
                  "xml2", "MrFlywheel/kaiserschmRn")

installed_pkgs <- installed.packages()[, "Package"]
missing_deps  <- setdiff(dependencies, installed_pkgs)

if (length(missing_deps) > 0) {
  cat("\nMissing dependencies:", paste(missing_deps, collapse = ", "), "\n")
  cat("Use 'pizzR::package.install(pizzR::dependencies)' to add missing packages.\n\n")
} else {
  cat("All dependencies are installed.\n")
}
