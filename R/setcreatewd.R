setcreate.wd <- function(x){
  if (!dir.exists(x)){
    base::dir.create(x, recursive = T)
    base::cat(paste0("\n", pizzR::Systime(),": '", x, "' created and set as working directory"))
    base::setwd(x)
  } else{
    if (base::getwd() != x){
      base::cat(paste0("\n", pizzR::Systime(),": '", x, "' set as working directory"))
      base::setwd(x)
    }
  }
}
