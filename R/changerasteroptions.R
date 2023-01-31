change.terraOptions <- function(changetempdir=F, tempdir="", OSRAM.remaining=3, progress=0, verbose=F, ...){
  
  if (!is.logical(changetempdir)) return(warning("'changetempdir' has to be of class logical!\n"))
  if (!is.logical(verbose)) return(warning("'verbose' has to be of class logical!\n"))
  
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse=", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "Rcpp", "terra"))

  require(terra)
  
  if (changetempdir && dir.exists(tempdir) == F) {
    base::dir.create(tempdir, recursive = T)
    base::cat(paste("\n", Sys.time(), tempdir, "created"))
  }
  
  memmax <- memuse::Sys.meminfo()$totalram@size
  if (((memmax-OSRAM.remaining) / memmax) > 0.9){
    memfrac  <-  0.9
  }else{
    memfrac <- ((memmax - OSRAM.remaining) / memmax)
  }
  
  fparameters                  <- list(...)
  if (changetempdir) fparameters$tempdir          <- tempdir
  fparameters$memfrac          <- memfrac
  fparameters$memmax           <- memmax
  fparameters$progress         <- progress
  
  do.call(terra::terraOptions, fparameters)
  if (verbose) terra::terraOptions()
}
