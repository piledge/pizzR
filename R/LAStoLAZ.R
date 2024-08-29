LAStoLAZ <- function(x,y=NULL,verbose=T){

  pizzR::package.install('lidR', verbose = 1)

  if(is.null(y))              y <- paste0(dirname(x), 'output_LAStoLAZ')

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
