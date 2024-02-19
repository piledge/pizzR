remove.oldtmps <- function(recursive = T, force = F){
  tmp.dir.current <- tempdir()
  tmp.folder <- dirname(tmp.dir.current)


  tmp.folders <- dir(tmp.folder, pattern = 'Rtmp', full.names = T)
  tmp.folders.old <- tmp.folders[-grep(basename(tmp.dir.current), tmp.folders)]

  n.folders.old <- length(tmp.folders.old)

  if (n.folders.old == 0){
    cat(paste0("\n", pizzR::Systime(), ": no old folders have been removed\n"))

  }else{
    unlink(tmp.folders.old, recursive = recursive, force = force)
    cat(paste0("\n", pizzR::Systime(), ": ",n.folders.old, " old folders have been removed\n"))
  }
}
