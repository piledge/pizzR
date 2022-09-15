#' automatic installation of all my favourite packages###################################

packages <- c("feather", "raster","terra","caret","tidyverse","ranger","randomForest","foreach","parallel","ggplot2",
              "rgdal","e1071","s2","sf","lidR","RMariaDB","remotes","memuse", "gdata")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
  cat(paste0("\n", Sys.time(), ": install missing packages '", paste(packages, collapse=", "), "'\n"))
  install.packages(packages[to_install], dependencies = T)
  cat(paste0("\n", Sys.time(), ": missing packages '", paste(packages, collapse=", "), "' installed\n\n"))
} else{
  cat(paste0("\n", Sys.time(), ": all packages installed\n\n"))
}
