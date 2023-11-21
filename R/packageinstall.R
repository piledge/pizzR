package.install <- function(x=pizzR::packages, verbose = T) {
  to_install <- !x %in% installed.packages()
  if (any(to_install)){
    cat(paste0("\n", Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
    install.packages(x[to_install], dependencies = T)
    cat(paste0("\n", Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
  }
  else{
    if (verbose) cat(paste0("\n", Sys.time(), ": all packages installed\n\n"))
  }
}
