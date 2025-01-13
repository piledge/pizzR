OTB_init <- function (path = NULL)
{
  if (is.null(path)) {
    if (Sys.info()["sysname"] == "Windows")
      files <- list.files("C:/", full.names = T)
    if (Sys.info()["sysname"] == "Linux")
      files <- list.files("~/", full.names = T)
    otb.instances <- files[grep("OTB", files)]

    if (length(otb.instances) == 0)
      stop(paste0("\n", pizzR::Systime(), ": Unable to access ", path, "'!"))

    path <- otb.instances[length(otb.instances)]
  }

  if (Sys.info()["sysname"] == "Windows" && length(grep('otbenv.bat', list.files(path))) == 0)   stop(paste0("\n", pizzR::Systime(), " '", path, "' not containing OTB-files!"))
  #if (Sys.info()["sysname"] == "Linux" && length(grep('monteverdi.sh', list.files(path))) == 0)  stop(paste0("\n", pizzR::Systime(), " '", path, "' not containing OTB-files!"))
  options(OTB_PATH = path)
  cat(paste0("\n", pizzR::Systime(), ": 'OTB_PATH' set to ", path))
}


OTB_run   <- function(cmd, Ncore = NULL, DefaultRAM = NULL, ...){

  pizzR::package.install(c("parallel"), verbose = 1)

  if (is.null(getOption("OTB_PATH"))){
    stop("OTB_PATH not found! Use 'OTB_init()' to set it...")
  }

  if (is.null(Ncore)) Ncore <- parallel::detectCores()-1

  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = Ncore)
  if (!is.null(DefaultRAM)) Sys.setenv(OTB_MAX_RAM_HINT = DefaultRAM)

  if (Sys.info()["sysname"] == "Windows"){

    OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.bat")
    stopifnot(file.access(OTB_PREFIX) == 0)
    OTB_PREFIX <- sprintf('"%s"', OTB_PREFIX)
  } else if (Sys.info()["sysname"] == "Linux"){

    OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.profile")
    stopifnot(file.access(OTB_PREFIX) == 0)
    OTB_PREFIX <- sprintf('. %s', OTB_PREFIX)

  } else {
    OTB_PREFIX <- file.path(getOption("OTB_PATH"), "otbenv.profile")
    OTB_PREFIX <- system(sprintf('echo %s', OTB_PREFIX), intern = TRUE)
    stopifnot(file.access(OTB_PREFIX) == 0)
    OTB_PREFIX <- sprintf('. "%s"', OTB_PREFIX)
  }

  try({
    cmd <- sprintf('%s && %s', OTB_PREFIX, cmd)
    cat("\nRunning:\n", cmd, "\n\n")
    system(cmd, ...)
  })
}


otb_setpath <- function(path){
  if (Sys.info()["sysname"] == "Windows"){
    if (is.null(path)) path <- 'C:/'

    files <- list.files(path, full.names = T)
    otb.instances <- files[grep("OTB", files)]

    if (length(otb.instances) == 0) stop(sprintf("\n%s: Unable to access '%s'", pizzR::Systime(), path))
    path <- otb.instances[length(otb.instances)]
    options(OTB_PATH = path)
  }
}




