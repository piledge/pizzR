change.terraOptions <- function (tempdir = NA,
                                 progress = 0, verbose = F, ...) 
{
  if (!is.logical(verbose))         return(warning("'verbose' has to be of class logical!\n"))
  
  if (!is.na(tempdir)){
    if (dir.exists(tempdir) == F) {
                                    base::dir.create(tempdir, recursive = T)
                                    base::cat(paste("\n", Sys.time(), tempdir, "created"))
    }
  }else{
    if (Sys.info()[1] == 'Windows') tempdir <- 'C:/temp/Rtmp'
    if (Sys.info()[1] == 'Linux')   tempdir <- '/tmp/Rtmp'
    if (dir.exists(tempdir) == F) {
                                    base::dir.create(tempdir, recursive = T)
                                    base::cat(paste("\n", Sys.time(), tempdir, "created"))
    }
  }

  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)) {
      cat(paste0(Sys.time(), ": install missing packages '", paste(x[to_install], collapse = ", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(Sys.time(), ": missing packages '", paste(x[to_install], collapse = ", "), "' installed\n\n"))
    }
  }
    package.install(c("memuse", "raster", "Rcpp", "terra"))
    require(terra)
    
    memmax <- memuse::Sys.meminfo()$totalram@size
    if (((memmax - 3)/memmax) > 0.9) {
      memfrac <- 0.9
    }
    else {
      memfrac <- ((memmax - 3)/memmax)
    }
    
    fparameters <- list(...)
    if (!is.na(tempdir)){
      fparameters$tempdir <- tempdir
      Sys.setenv(TMP = tempdir, TEMP = tempdir)
    }
    fparameters$memfrac <- memfrac
    fparameters$progress <- progress
    
    do.call(terra::terraOptions, fparameters)
    if (verbose) terra::terraOptions()
}
