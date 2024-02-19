setcreate.wd <- function(path, verbose=T){
  if (!is.logical(verbose)) return(warning("'verbose' has to be of type logical!"))

  if (!dir.exists(path)){
    dir.create(path, recursive = T)
    if (verbose)  cat(paste0("\n", pizzR::Systime(),": '", path, "' created and set as working directory"))
    setwd(path)
  } else{
    if (getwd() != path){
      if (verbose)  cat(paste0("\n", pizzR::Systime(),": '", path, "' set as working directory"))
      setwd(path)
    }
  }
}
