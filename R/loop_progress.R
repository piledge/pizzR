loop_progress <- function(i, total_digits = 5, text = 'Loop'){
  cat(sprintf("%s: %s %0*d\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text, total_digits, i))
}
