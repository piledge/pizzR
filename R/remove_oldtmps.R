remove.oldtmps <- function(recursive=T, force=F, expand=T){
  stopifnot(inherits(recursive, 'logical'),
            inherits(force, 'logical'),
            inherits(expand, 'logical'))
  
  tmp_dir_current <- tempdir()
  tmp_folder <- dirname(tmp_dir_current)
  
  tmp_folders <- dir(tmp_folder, pattern = 'Rtmp', full.names = T)
  tmp_folders_old <- tmp_folders[-grep(basename(tmp_dir_current), tmp_folders)]
  tmp_files <- list.files(tmp_folders_old, recursive = T, full.names = T)
  filesum <- pizzR::format_file_size(sum(file.size(tmp_files)))
  
  n_folders_old <- length(tmp_folders_old)
  
  if (n_folders_old == 0){
    cat(sprintf("\n%s: no old folders have been removed!\n", pizzR::Systime()))
  }else{
    unlink(tmp_folders_old, recursive = recursive, force = force, expand = expand)
    
    cat(sprintf("\n%s: %d old folders have been removed!", pizzR::Systime(), n_folders_old))
    cat(sprintf("\n%s: %s of storage have been freed!\n", pizzR::Systime(), filesum))
  }
}
