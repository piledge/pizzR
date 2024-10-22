loop_progress <- function(i, total_digits){
  cat(sprintf("%s: Loop %0*d\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), total_digits, i))
}
