raster.compressor <- function (x, tmpdir = NULL, recursive = T, dryrun = T)
{
  stopifnot(is.character(x))
  if (is.null(tmpdir)) tmpdir <- file.path(tempdir(), "tmp_compression")
  stopifnot(is.character(tmpdir))
  stopifnot(is.logical(recursive))
  stopifnot(is.logical(dryrun))

  cat(sprintf("\n%s: Searching for '.tif'- or '.tiff'-files.\n", pizzR::Systime()))
  files <- list.files(x, pattern = ".tif$", recursive = recursive, full.names = T)
  nfiles <- length(files)
  if (nfiles == 0) return(warning("No files have been found!"))
  cat(sprintf("\n%s: %s files found.\n", pizzR::Systime(), nfiles))

  nchar.nfiles <- nchar(nfiles)
  indices <- sprintf("%0*d", 3, seq(nfiles))
  names.tmpfiles <- sprintf('%s_%s', indices, basename(files))
  file.list <- data.frame(id = indices, old.files = files,
                          tmp.files = file.path(tmpdir, names.tmpfiles), compressed = F,
                          filesize.old = file.size(files), filesize.new = NA,
                          filesize.diff = NA, percent.new = NA, change.file = NA)
  pizzR::setcreate.wd(tmpdir)

  for (i in seq(files)) {
    cat(sprintf('\n\n%s: %s of %s', pizzR::Systime(), i, nfiles))
    cat(sprintf("\n%s: File '%s'", pizzR::Systime(), file.list$old.files[i]))
    if (!file.exists(file.list$old.files[i])) {
      cat(crayon::red(sprintf("\n\n%s: File initially found but currently not available!", pizzR::Systime())))
      file.list$filesize.new[i] <- file.list$filesize.diff[i] <- file.list$percent.new[i] <- NA
      next
    }

    rst <- terra::rast(file.list$old.files[i])

    orig.filesize <- file.list$filesize.old[i]
    cat(sprintf('\n%s: Original filesize is %s ', pizzR::Systime(), pizzR::format_file_size(orig.filesize)))
    pizzR::writeslimRaster(rst, file.list$tmp.files[i], compression = T)

    file.list$filesize.new[i] <- file.size(file.list$tmp.files[i])
    file.list$filesize.diff[i] <- file.list$filesize.old[i] - file.list$filesize.new[i]

    file.list$percent.new[i] <- file.list$filesize.new[i]/file.list$filesize.old[i] *  100
    file.list$compressed[i] <- T

    if (file.list$percent.new[i] < 100) change.file <- T else change.file <- F
    file.list$change.file[i] <- change.file

    if (!dryrun && change.file) {
      file.remove(file.list$old.files[i])
      suppressWarnings(file.rename(file.list$tmp.files[i], file.list$old.files[i]))
      if (file.exists(file.list$tmp.files[i])) {
        try(file.copy(file.list$tmp.files[i], file.list$old.files[i]), F)
        try(file.remove(file.list$tmp.files[i]), F)
      }
    }
    filesize.diff <- abs(file.list$filesize.diff[i])

    if (file.list$percent.new[i] < 100) cat(sprintf('%s: New file is %s (%s %%) smaller', pizzR::Systime(), pizzR::format_file_size(filesize.diff), round(100 - file.list$percent.new[i], 2)))
    if (file.list$percent.new[i] > 100) cat(sprintf('%s: New file is %s (%s %%) bigger', pizzR::Systime(), pizzR::format_file_size(filesize.diff), round(file.list$percent.new[i] - 100, 2)))
    if (file.list$percent.new[i] == 100) cat(sprintf("%s: No change in filesize. File will not be changed!", pizzR::Systime()))
  }

  if (nfiles > 1) {
    file.list.changed <- file.list[file.list$change.file == T, ]
    changed.diff <- sum(file.list.changed$filesize.old[file.list.changed$compressed]) - sum(file.list.changed$filesize.new[file.list.changed$compressed])
    changed.percent <- sum(file.list.changed$filesize.new[file.list.changed$compressed])/sum(file.list.changed$filesize.old[file.list.changed$compressed]) * 100

    if (is.nan(changed.percent)) {
      cat(crayon::red(sprintf("\n\n%s: No changes in filesize. No File will be changed!\n", pizzR::Systime())))
    }
    else {
      if (changed.percent < 100) cat(sprintf("\n\n%s: %s files changed. New files are %s (%s%%) smaller.\n\n", pizzR::Systime(), nrow(file.list.changed), pizzR::format_file_size(changed.diff), round(100 - changed.percent, 2)))
      if (changed.percent > 100) cat(sprintf("\n\n%s: %s files changed. New files are %s (%s%%) bigger\n\n", pizzR::Systime(), nrow(file.list.changed), pizzR::format_file_size(changed.diff), abs(round(changed.percent - 100, 2))))
      if (changed.percent == 100) cat(sprintf("%s: No change in filesizes.", pizzR::Systime()))
    }
  }
  if (dryrun) {
    file.remove(list.files(file.path(tempdir(), "tmp_compression"), recursive = T, full.names = T))
    cat(crayon::red(sprintf("\n%s: Dryrun! No files have been changed!\n", pizzR::Systime())))
  }
  invisible(file.list)
}
