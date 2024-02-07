raster.compressor <- function(x, tmpdir=NA, dryrun = T){

  if (!is.character(x))       return(warning("'x' has to be of type character!"))
  if (is.na(tmpdir))          tmpdir <- file.path(x, 'tmp_compression')
  if (!is.character(tmpdir))  return(warning("'tmpdir' has to be of type character!"))
  if (!is.logical(dryrun))    return(warning("'dryrun' has to be of type logical!"))

  cat(paste0("\n\n", pizzR::Systime(), ": Searching for '.tif'- or '.tiff'-files "))

  files <- list.files(x, pattern = '.tif$', recursive = T, full.names = T)
  nfiles <- length(files)
  if (nfiles == 0)    return(warning("No files have been found!"))
  cat(paste0(', ', nfiles, ' found\n'))

  nchar.nfiles <- nchar(nfiles)
  indices <- sprintf(paste0("%0", nchar.nfiles, ".f"), seq(nfiles))

  names.tmpfiles <- paste(indices, basename(files), sep = '_')

  file.list <- data.frame(id=indices, old.files=files, tmp.files=file.path(tmpdir, names.tmpfiles),
                          filesize.old.MiB=file.info(files)$size/(1048576), filesize.new.MiB=NA,
                          filesize.diff=NA, percent.new=NA)

  pizzR::setcreate.wd(tmpdir)
  for (i in seq(files)){
    if (!file.exists(file.list$old.files[i])){
      cat(crayon::red(paste0("\n", pizzR::Systime(), ": File initially found but not available!\n")))
      file.list$filesize.new.MiB[i] <- file.list$diff[i] <- file.list$percent.new[i] <- NA
      next
    }
    rst <- terra::rast(file.list$old.files[i])
    cat(paste0("\n\n", pizzR::Systime(), ": File '",  file.list$old.files[i], "'"))
    cat(paste0("\n", pizzR::Systime(), ": Original filesize ",  file.list$filesize.old.MiB[i], " MiB"))
    pizzR::writeslimRaster(rst, file.list$tmp.files[i], compression = T)
    file.list$filesize.new.MiB[i] <- file.info(file.list$tmp.files[i])$size/(1048576)
    file.list$diff[i] <- file.list$filesize.old.MiB[i] - file.list$filesize.new.MiB[i]
    file.list$percent.new[i] <- file.list$filesize.new.MiB[i] / file.list$filesize.old.MiB[i] * 100

    if (!dryrun){
      file.remove(file.list$old.files)
      file.rename(file.list$tmp.files, file.list$old.files)
    }

    if (file.list$percent.new[i] < 100)   cat(paste0(pizzR::Systime(), ": New file is ",  round(file.list$diff[i], 2) , ' MiB (', round(100 - file.list$percent.new[i], 2), ' %) smaller.\n'))
    if (file.list$percent.new[i] > 100)   cat(paste0(pizzR::Systime(), ": New file is ",  abs(round(file.list$diff[i], 2)) , ' MiB (', abs(round(file.list$percent.new[i] - 100, 2)), ' %) bigger.\n'))
    if (file.list$percent.new[i] == 100)  cat(paste0(pizzR::Systime(), ": No change in filesize\n"))
  }

  if (!dryrun) file.remove(tmpdir)

  if (nfiles > 1){
    all.diff <- sum(file.list$filesize.old.MiB) - sum(file.list$filesize.new.MiB)
    all.percent <- sum(file.list$filesize.new.MiB) / sum(file.list$filesize.old.MiB) * 100
    if (all.percent < 100)   cat(paste0("\n", pizzR::Systime(), ": New files are ",  round(all.diff, 2) , ' MiB (', round(100 - all.percent, 2), ' %) smaller.\n'))
    if (all.percent > 100)   cat(paste0("\n", pizzR::Systime(), ": New files are ",  abs(round(all.diff, 2)) , ' MiB (', abs(round(all.percent - 100, 2)), ' %) bigger.\n'))
    if (all.percent == 100)  cat(paste0("\n", pizzR::Systime(), ": No change in filesize\n"))
  }
  if (dryrun) cat(crayon::red(paste0("\n", pizzR::Systime(), ": Dryrun! No files have been changed!\n")))
}
