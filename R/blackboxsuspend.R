blackbox.suspend <- function(gracetime = 60){
  if(Sys.info()[4] != "blackbox") return(warning("Only valid for node 'Blackbox'!"))
  
  for (i in 1:gracetime){
    cat(paste0("\r",Sys.time(), ": Blackbox will suspend in ", gracetime - i + 1, " seconds!   "))
    Sys.sleep(1)
  }
  cat(paste0("\r",Sys.time(), ": planned suspension after processing   "))
  system('sh /media/geodata1/suspend.sh')
}