remove.oldtmps <- function(recursive = T, force = F, expand = T){
  tmp.dir.current <- tempdir()
  tmp.folder <- dirname(tmp.dir.current)

  tmp.folders <- dir(tmp.folder, pattern = 'Rtmp', full.names = T)
  tmp.folders.old <- tmp.folders[-grep(basename(tmp.dir.current), tmp.folders)]
  tmp.files <- list.files(tmp.folders.old, recursive = T, full.names = T)
  filesum.mib <- round(sum(file.size(tmp.files))/1048576, 2)

  n.folders.old <- length(tmp.folders.old)

  if (n.folders.old == 0){
    cat(paste0("\n", pizzR::Systime(), ": no old folders have been removed!\n"))
  }else{
    unlink(tmp.folders.old, recursive = recursive, force = force, expand = expand)

    if (filesum.mib < 1024) unit <- ' MiB' else unit <- ' GiB'
    cat(paste0("\n", pizzR::Systime(), ": ", n.folders.old, " old folders have been removed!\n"))
    cat(paste0("\n", pizzR::Systime(), ": ", filesum.mib, unit, " of storage have been freed!\n"))
  }
}
