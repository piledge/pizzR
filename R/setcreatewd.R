setcreate.wd <- function(x){
  options(digits.secs = 0)
  if (!dir.exists(x)){
    base::dir.create(x, recursive = T)
    base::cat(paste0("\n", Sys.time(),": '", x, "' created and set as working directory"))
    base::setwd(x)
  } else{
    if (base::getwd() != x){
      base::cat(paste0("\n", Sys.time(),": '", x, "' set as working directory"))
      base::setwd(x)
    }
  }
}
