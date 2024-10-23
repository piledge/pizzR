LAStoLAZ <- function(x, y = NULL, verbose = TRUE){

  pizzR::package.install('lidR', verbose = 1)

  if (is.null(y)) y <- file.path(dirname(x), 'output_LAStoLAZ')
  if (!dir.exists(y)) pizzR::setcreate.wd(y)
  stopifnot(is.logical(verbose), dir.exists(x))
  
  lasfiles <- list.files(x, pattern = '\\.las$', full.names = TRUE)
  nfiles <- length(lasfiles)
  stopifnot(nfiles > 0, msg = 'Input-folder empty!')

  if (verbose) cat(sprintf('\nWriting LAZ-files to "%s"\n\n', y))
  for (i in seq_along(lasfiles)) {
    if (verbose) cat(sprintf("Compressing: %d / %d   \r", i, nfiles))

    las <- lidR::readLAS(lasfiles[i])
    output_file <- file.path(y, sub('\\.las$', '.laz', basename(lasfiles[i])))
    lidR::writeLAS(las, output_file)
  }
  if (verbose) cat('\nCompression complete!\n')
}
