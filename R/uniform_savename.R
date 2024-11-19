uniform_savename <- function(file_name, file_extension, iterator=NULL, width=5){
  if (is.null(iterator)) return(sprintf('%s%s', file_name, file_extension))
  return(sprintf('%s_%0*d%s', file_name, width, iterator, file_extension))
}
