LAStoLAZ <- function(x, y = NULL, verbose = TRUE) {
  pizzR::package.install('lidR', verbose = 1)

  if (is.null(y)) y <- file.path(dirname(x), 'output_LAStoLAZ')
  if (!dir.exists(y)) pizzR::setcreate.wd(y)

  stopifnot(is.logical(verbose))
  stopifnot(dir.exists(x), msg = 'Input-folder does not exist!')
  
  lasfiles <- list.files(x, pattern = '\\.las$', full.names = TRUE)
  nfiles <- length(lasfiles)

  stopifnot(nfiles > 0, msg = 'Input-folder empty!')

  if (verbose) cat(paste0('\nWriting LAZ-files to "', y, '"\n\n'))

  for (i in seq_along(lasfiles)) {
    if (verbose) cat('Compressing: ', i, '/', nfiles, '   \r')

    las <- lidR::readLAS(lasfiles[i])
    output_file <- file.path(y, sub('\\.las$', '.laz', basename(lasfiles[i])))
    lidR::writeLAS(las, output_file)
  }
  
  if (verbose) cat('\nCompression complete!\n')
}
