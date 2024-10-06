setcreate.wd <- function(path, verbose = TRUE) {
  stopifnot(is.logical(verbose))

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    if (verbose) {
      cat(sprintf("\n%s: '%s' created and set as working directory", pizzR::Systime(), path))
    }
  }

  if (getwd() != path) {
    setwd(path)
    if (verbose) {
      cat(sprintf("\n%s: '%s' set as working directory", pizzR::Systime(), path))
    }
  }
}
