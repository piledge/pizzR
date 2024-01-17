package.install <- function(x=pizzR::packages, verbose = T) {
  to_install <- !x %in% installed.packages()
  if (any(to_install)){
    cat(paste0("\n", pizzR::Systime(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
    install.packages(x[to_install], dependencies = T)
    cat(paste0("\n", pizzR::Systime(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
  }
  else{
    if (verbose) cat(paste0("\n", pizzR::Systime(), ": all packages installed\n\n"))
  }
}
