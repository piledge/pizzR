remove.oldtmps <- function(recursive=T, force=F, expand=T){
  stopifnot(inherits(recursive, 'logical'),
            inherits(force, 'logical'),
            inherits(expand, 'logical'))
  
  tmp.dir.current <- tempdir()
  tmp.folder <- dirname(tmp.dir.current)
  
  tmp.folders <- dir(tmp.folder, pattern = 'Rtmp', full.names = T)
  tmp.folders.old <- tmp.folders[-grep(basename(tmp.dir.current), tmp.folders)]
  tmp.files <- list.files(tmp.folders.old, recursive = T, full.names = T)
  filesum <- pizzR::format_file_size(sum(file.size(tmp.files)))
  
  n.folders.old <- length(tmp.folders.old)
  
  if (n.folders.old == 0){
    cat(sprintf("\n%s: no old folders have been removed!\n", pizzR::Systime()))
  }else{
    unlink(tmp.folders.old, recursive = recursive, force = force, expand = expand)
    
    cat(sprintf("\n%s: %d old folders have been removed!", pizzR::Systime(), n.folders.old))
    cat(sprintf("\n%s: %s of storage have been freed!\n", pizzR::Systime(), filesum))
  }
}
