change.terraOptions <- function(changetempdir=FALSE, tempdir="", OSRAM.remaining=3, progress=0, verbose=FALSE, ...){

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

  if (changetempdir == TRUE && dir.exists(tempdir) == FALSE) {
    base::dir.create(tempdir, recursive = TRUE)
    base::cat(paste("\n", Sys.time(), tempdir, "created"))
  }

  memmax <- memuse::Sys.meminfo()$totalram@size
  if (((memmax-OSRAM.remaining) / memmax) > 0.9){
    memfrac  <-  0.9
  }else{
    memfrac <- ((memmax - OSRAM.remaining) / memmax)
  }

                          fparameters                  <- list(...)
  if (changetempdir == T) fparameters$tempdir          <- tempdir
                          fparameters$memfrac          <- memfrac
                          fparameters$memmax           <- memmax
                          fparameters$progress         <- progress

  do.call(terra::terraOptions, fparameters)
  if (verbose == TRUE) terra::terraOptions()
}
