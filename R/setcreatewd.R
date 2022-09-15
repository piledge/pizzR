#' setcreate.wd() ##########################################################################
#' combines 'setwd()' and 'getwd()'-functions; creates working directory if not exist
#' @param      setcreate.wd.folder directory to create/set
#' @example    setcreate.wd("D:/working directory")


setcreate.wd <- function(setcreate.wd.folder){
  if (dir.exists(setcreate.wd.folder) == FALSE){
    base::dir.create(setcreate.wd.folder, recursive = TRUE)
    base::cat(paste0("\n", Sys.time(),": '", setcreate.wd.folder, "' created and set as working directory"))
    base::setwd(setcreate.wd.folder)
  } else{
    if (base::getwd() != setcreate.wd.folder){
      base::cat(paste0("\n", Sys.time(),": '", setcreate.wd.folder, "' set as working directory"))
      base::setwd(setcreate.wd.folder)
    }
  }
}
