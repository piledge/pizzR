LAStoLAZ <- function(x,y=NULL,verbose=T){
  package.install <- function(x) {
    to_install <- !x %in% installed.packages()
    if (any(to_install)) {
      cat(paste0(pizzR::Systime(), ": install missing packages '",
                 paste(x[to_install], collapse = ", "), "'\n"))
      install.packages(x[to_install], dependencies = T)
      cat(paste0(pizzR::Systime(), ": missing packages '", paste(x[to_install],
                                                           collapse = ", "), "' installed\n\n"))
    }
  }
  package.install('lidR')

  if(is.null(y)) y <- file.path(dirname(x), 'output_LAStoLAZ')

  if(!is.logical(verbose))    return(warning('Verbose has to be logical!'))
  if(!dir.exists(x))          return(warning('Input-folder does not exist!'))
  if(!dir.exists(y))          dir.create(y, recursive = T)

  lasfiles                    <- list.files(x, pattern = '.las$', full.names = T)
  nfiles                      <- length(lasfiles)
  if (nfiles == 0)            return(warning('Input-folder empty!'))
  if (verbose)                cat(paste0('\nWriting LAZ-files to "', y, '"\n\n'))

  for (i in seq(lasfiles)){
    if (verbose)              cat('compressing: ', i, '/', nfiles,  '   \r')

    las                       <- lidR::readLAS(lasfiles[i])
    lidR::writeLAS(las, file.path(x, basename(sub('las', 'laz', basename(lasfiles[i])))))
  }
}
