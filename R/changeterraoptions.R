change.terraOptions <- function(changetempdir=FALSE, tempdir="", OSRAM.remaining=3, progress=0, verbose=FALSE, ...){

  package.install <- function(packages) {
    to_install <- !packages %in% installed.packages()
    if (any(to_install)){
      cat(paste0(Sys.time(), ": install missing packages '", paste(packages[to_install], collapse=", "), "'\n"))
      install.packages(packages[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(packages[to_install], collapse=", "), "' installed\n\n"))
    }
  }
  package.install(c("memuse", "raster", "terra"))


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
