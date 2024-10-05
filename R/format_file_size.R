format_file_size <- function(size_in_bytes) {
  if (size_in_bytes >= 2^30) {
    size <- size_in_bytes / (2^30)
    return(sprintf("%.2f GiB", size))
  } else if (size_in_bytes >= 2^20) {
    size <- size_in_bytes / (2^20)
    return(sprintf("%.2f MiB", size))
  } else if (size_in_bytes >= 2^10) {
    size <- size_in_bytes / (2^10)
    return(sprintf("%.2f KiB", size))
  } else {
    return(paste(size_in_bytes, "Bytes"))
  }
}
