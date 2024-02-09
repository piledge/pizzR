setcreate.wd <- function(path){
  if (!dir.exists(path)){
    base::dir.create(path, recursive = T)
    base::cat(paste0("\n", pizzR::Systime(),": '", path, "' created and set as working directory"))
    base::setwd(path)
  } else{
    if (base::getwd() != path){
      base::cat(paste0("\n", pizzR::Systime(),": '", path, "' set as working directory"))
      base::setwd(path)
    }
  }
}
