package.install <- function(packages) {
  to_install <- !packages %in% installed.packages()
  if (any(to_install)){
    cat(paste0("\n", Sys.time(), ": install missing packages '", paste(packages[to_install], collapse=", "), "'\n"))
    install.packages(packages[to_install], dependencies = T)
    cat(paste0("\n", Sys.time(), ": missing packages '", paste(packages[to_install], collapse=", "), "' installed\n\n"))
  }
  else{
    cat(paste0("\n", Sys.time(), ": all packages installed\n\n"))
  }
}
