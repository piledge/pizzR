server.suspend <- function(x=60, y='sh /media/geodata1/suspend.sh'){
  
  y <-tolower(y)
  
  for (i in 1:x){
    cat(paste0("\r",Sys.time(), ": node '",Sys.info()[4],"' will suspend in ", x - i + 1, " seconds!   "))
    Sys.sleep(1)
  }
  cat(paste0("\r",Sys.time(), ": planned suspension after processing   "))
  system(y)
}
