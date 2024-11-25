setcreate.wd <- function(path, verbose=T){
  stopifnot(is.logical(verbose))

  if (!dir.exists(path)){
    dir.create(path, recursive = T)
    if (verbose)  cat(sprintf("\n%s: '%s' created and set as working directory.", pizzR::Systime(), path))
    setwd(path)
  } else{
    if (getwd() != path){
      if (verbose)  cat(sprintf("\n%s: '%s' set as working directory.", pizzR::Systime(), path))
      setwd(path)
    }
  }
  invisible(NULL)
}
