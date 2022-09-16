packages <- c("feather", "raster", "terra", "caret", "tidyverse", "ranger", "randomForest", "foreach", "ggplot2",
              "rgdal", "e1071", "s2", "sf", "lidR", "RMariaDB", "remotes", "memuse", "gdata")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
  cat("\n\n\nPackages missing:",paste0(packages[to_install],sep=""),"\n")
  cat("use 'mypackageinstall()' to add\n\n\n")
}
