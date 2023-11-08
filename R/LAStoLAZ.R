LAStoLAZ <- function(i.path,o.path=NULL,verbose=T){
  pizzR::package.install('lidR')
  
  if(is.null(o.path)) o.path <- file.path(dirname(i.path), 'output_LAStoLAZ')
  
  if(!is.logical(verbose))    return(warning('Verbose has to be logical!'))
  if(!dir.exists(i.path))     return(warning('Input-folder does not exist!'))
  if(!dir.exists(o.path))     dir.create(o.path, recursive = T)
  
  lasfiles                    <- list.files(i.path, pattern = '.las$', full.names = T)
  nfiles                      <- length(lasfiles)
  if (nfiles == 0)            return(warning('Input-folder empty!'))
  if (verbose)                cat(paste0('Writing LAZ-files to "', o.path, '"\n\n'))
  
  for (i in seq(lasfiles)){
    if (verbose)              cat('compressing: ', i, '/', nfiles,  '   \r')
  
    las                       <- lidR::readLAS(lasfiles[i])
    lidR::writeLAS(las, file.path(o.path, basename(sub('las', 'laz', basename(lasfiles[i])))))
  }
}