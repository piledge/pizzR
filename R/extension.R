extension <- function (filename, value = NULL, maxchar = 10)
{
  if (!is.null(value)) {
    extension(filename) <- value
    return(filename)
  }
  lfn <- nchar(filename)
  ext <- list()
  for (f in 1:length(filename)) {
    extstart <- -1
    for (i in lfn[f]:2) {
      if (substr(filename[f], i, i) == ".") {
        extstart <- i
        break
      }
    }
    if (extstart > 0) {
      ext[f] <- substr(filename[f], extstart, lfn[f])
    }
    else {
      ext[f] <- ""
    }
  }
  ext <- unlist(ext)
  ext[nchar(ext) > maxchar] <- ""
  return(ext)
}
