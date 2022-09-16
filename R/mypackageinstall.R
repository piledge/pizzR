packages <- c("feather", "raster", "terra", "caret", "tidyverse", "ranger", "randomForest", "foreach", "ggplot2",
              "rgdal", "e1071", "s2", "sf", "lidR", "RMariaDB", "remotes", "memuse", "gdata")

to_install <- !packages %in% installed.packages()
if (any(to_install)){
print("\n\n\npakete fehlen du Binf\n\n\n")
}
