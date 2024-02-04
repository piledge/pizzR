dependencies <- c("caret", "crayon", "doParallel", "e1071", "feather", "foreach", "gdata", "ggplot2", "lidR", "magrittr", "parallelly", "randomForest", "ranger",
                  "raster", "Rcpp", "remotes", "rvest", "RMariaDB", "s2", "sf", "snow", "terra", "tidyverse", "xml2")

to_install <- !dependencies %in% rownames(installed.packages())

if (any(to_install)){
  missing_dependencies <- dependencies[to_install]
  n.missing <- length(missing_dependencies)
  nchar.missing <- nchar(n.missing)
  
  cat("\n\nDependencies missing:", paste0(missing_dependencies, sep=","), "\n")
  cat("\nInstalling ...")
  for (i in seq(n.missing)){
    cat(sprintf(paste0("\n%0", nchar.missing, ".f"), i), 'of', n.missing, ':', missing_dependencies[i])
    install.packages(missing_dependencies[i], dependencies = F, quiet = T)
  }
  
  success <- (all(dependencies %in% rownames(installed.packages())))
  if (success) cat("\n\nDone.\n")
  if (!success){
    to_install <- !dependencies %in% rownames(installed.packages())
    cat("\n\nDependencies still missing:", dependencies[to_install])
    cat("Try to install manually.\n")
  }
}else{
  cat("\nDependencies meet the requirements\n")
}
