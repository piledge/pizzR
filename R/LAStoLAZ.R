LAStoLAZ <- function(in_path, out_path = NULL, verbose = TRUE)
{
  pizzR::package.install("lidR", verbose = 1)

  if (is.null(out_path)) out_path <- file.path(in_path, "output_LAStoLAZ")
  if (!dir.exists(out_path)) pizzR::setcreate.wd(out_path, verbose = F)
  stopifnot(is.logical(verbose), dir.exists(in_path))

  lasfiles <- list.files(in_path, pattern = '\\.las$', full.names = TRUE)
  nfiles <- length(lasfiles)
  stopifnot(nfiles > 0)

  if (verbose) cat(sprintf("\nWriting LAZ-files to '%s'\n\n", out_path))
  for (i in seq_along(lasfiles)) {
    if (verbose) cat(sprintf('Compressing: %d / %d   \r', i, nfiles))

    las <- lidR::readLAS(lasfiles[i])
    output_file <- file.path(out_path, sub('\\.las$', '.laz', basename(lasfiles[i])))
    lidR::writeLAS(las, output_file)
  }
  if (verbose) cat('\nCompression complete!\n')
}
