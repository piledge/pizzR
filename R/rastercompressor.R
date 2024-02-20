raster.compressor <- function(x, tmpdir=NULL, recursive=T, dryrun=T){

  if (!is.character(x))       return(warning("'x' has to be of type character!"))
  if (is.null(tmpdir))        tmpdir <- file.path(tempdir(), 'tmp_compression')
  if (!is.character(tmpdir))  return(warning("'tmpdir' has to be of type character!"))
  if (!is.logical(recursive)) return(warning("'recursive' has to be of type logical!"))
  if (!is.logical(dryrun))    return(warning("'dryrun' has to be of type logical!"))

  cat(paste0("\n\n", pizzR::Systime(), ": Searching for '.tif'- or '.tiff'-files"))
  files <- list.files(x, pattern = '.tif$', recursive = recursive, full.names = T)
  nfiles <- length(files)
  if (nfiles == 0)    return(warning("No files have been found!"))
  cat(paste0(', ', nfiles, ' found\n'))

  nchar.nfiles <- nchar(nfiles)
  indices <- sprintf(paste0("%0", nchar.nfiles, ".f"), seq(nfiles))

  names.tmpfiles <- paste(indices, basename(files), sep = '_')

  file.list <- data.frame(id=indices, old.files=files, tmp.files=file.path(tmpdir, names.tmpfiles), compressed=F,
                          filesize.old.MiB=file.size(files)/1048576, filesize.new.MiB=NA,
                          filesize.diff=NA, percent.new=NA)

  pizzR::setcreate.wd(tmpdir)
  for (i in seq(files)){
    if (!file.exists(file.list$old.files[i])){
      cat(paste0("\n\n", pizzR::Systime(), ": ",  i, " of ", nfiles))
      cat(paste0("\n", pizzR::Systime(), ": File '",  file.list$old.files[i], "'"))
      cat(crayon::red(paste0("\n", pizzR::Systime(), ": File initially found but currently not available!\n")))
      file.list$filesize.new.MiB[i] <- file.list$filesize.diff[i] <- file.list$percent.new[i] <- NA
      next
    }
    rst <- terra::rast(file.list$old.files[i])
    cat(paste0("\n\n", pizzR::Systime(), ": ",  i, " of ", nfiles))
    cat(paste0("\n", pizzR::Systime(), ": File '",  file.list$old.files[i], "'"))
    cat(paste0("\n", pizzR::Systime(), ": Original filesize ",  round(file.list$filesize.old.MiB[i], 2), " MiB"))
    pizzR::writeslimRaster(rst, file.list$tmp.files[i], compression = T)
    file.list$filesize.new.MiB[i] <- file.size(file.list$tmp.files[i])/1048576
    file.list$filesize.diff[i] <- file.list$filesize.old.MiB[i] - file.list$filesize.new.MiB[i]
    file.list$percent.new[i] <- file.list$filesize.new.MiB[i] / file.list$filesize.old.MiB[i] * 100
    file.list$compressed[i] <- T

    if (!dryrun){
      file.remove(file.list$old.files[i])
      suppressWarnings(file.rename(file.list$tmp.files[i], file.list$old.files[i]))
      if (file.exists(file.list$tmp.files[i])){
        try(file.copy(file.list$tmp.files[i], file.list$old.files[i]), F)
        try(file.remove(file.list$tmp.files[i]), F)
      }
    }

    if (file.list$percent.new[i] < 100)   cat(paste0(pizzR::Systime(), ": New file is ",  round(file.list$filesize.diff[i], 2) , ' MiB (', round(100 - file.list$percent.new[i], 2), ' %) smaller.\n'))
    if (file.list$percent.new[i] > 100)   cat(paste0(pizzR::Systime(), ": New file is ",  abs(round(file.list$filesize.diff[i], 2)) , ' MiB (', abs(round(file.list$percent.new[i] - 100, 2)), ' %) bigger.\n'))
    if (file.list$percent.new[i] == 100)  cat(paste0(pizzR::Systime(), ": No change in filesize\n"))
  }

  if (nfiles > 1){
    all.diff <- sum(file.list$filesize.old.MiB[file.list$compressed]) - sum(file.list$filesize.new.MiB[file.list$compressed])
    all.percent <- sum(file.list$filesize.new.MiB[file.list$compressed]) / sum(file.list$filesize.old.MiB[file.list$compressed]) * 100
    if (all.percent < 100)   cat(paste0("\n\n", pizzR::Systime(), ": New files are ",  round(all.diff, 2) , ' MiB (', round(100 - all.percent, 2), ' %) smaller.\n\n'))
    if (all.percent > 100)   cat(paste0("\n\n", pizzR::Systime(), ": New files are ",  abs(round(all.diff, 2)) , ' MiB (', abs(round(all.percent - 100, 2)), ' %) bigger.\n\n'))
    if (all.percent == 100)  cat(paste0("\n\n", pizzR::Systime(), ": No change in filesize\n\n"))
  }

  if (dryrun){
    file.remove(list.files(file.path(tempdir(), 'tmp_compression'), recursive = T, full.names = T))
    cat(crayon::red(paste0("\n", pizzR::Systime(), ": Dryrun! No files have been changed!\n")))
  }

  invisible(file.list)
}
