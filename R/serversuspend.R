server.suspend <- function(x=60, y='sh /media/geodata1/suspend.sh'){

  for (i in seq_len(x)){
    cat(paste0("\r", pizzR::Systime(), ": node '", Sys.info()[4],"' will suspend in ", x - i + 1, " seconds!   "))
    Sys.sleep(1)
  }
  cat(paste0("\r", pizzR::Systime(), ": planned suspension after processing   "))
  system(y)
}
