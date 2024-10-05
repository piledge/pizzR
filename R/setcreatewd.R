setcreate.wd <- function(path, verbose = TRUE) {
  stopifnot(is.logical(verbose))

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    if (verbose) {
      cat(paste0("\n", pizzR::Systime(), ": '", path, "' created and set as working directory"))
    }
  } 

  if (getwd() != path) {
    setwd(path)
    if (verbose) {
      cat(paste0("\n", pizzR::Systime(), ": '", path, "' set as working directory"))
    }
  }
}
