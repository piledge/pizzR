# uc_OTB_haralick <- function(IMGpath=NULL,savedir=NULL,OTBpath=NULL,
#                             band=NULL,texture=c("simple","advanced","higher"),
#                             xrad=3,yrad=3,nbbin=8,
#                             Ncore=parallel::detectCores()-1,ram=NULL, ...){
#   
#   pizzR::package.install(c("memuse", "terra", "tools"), verbose = 1)
#   
#   if (!is.null(IMGpath)){
#     if (!is.character(IMGpath)) return(warning("'IMGpath' has to be of type character!"))
#     IMGpath   <- paste(c(' -in', IMGpath), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(band[i])){
#     if (!is.numeric(band[i])) return(warning("'band' has to be of type integer!"))
#     band.i    <- paste(c(' -channel', band[i]), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(xrad)){
#     if (!is.numeric(xrad)) return(warning("'xrad' has to be of type integer!"))
#     xrad      <- paste(c(' -parameters.xrad', xrad), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(yrad)){
#     if (!is.numeric(yrad)) return(warning("'yrad' has to be of type integer!"))
#     yrad      <- paste(c(' -parameters.yrad', yrad), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(nbbin)){
#     if (!is.numeric(nbbin)) return(warning("'nbbin' has to be of type integer!"))
#     nbbin     <- paste(c(' -parameters.nbbin', nbbin), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(ram)){
#     if (!is.numeric(ram)) return(warning("'ram' has to be of type integer!"))
#     ram       <- paste(c(' -ram', ram), sep = ',', collapse= " ")
#   }
#   if (!is.null(ram)){
#     avail.ram <- memuse::Sys.meminfo()$totalram@size*1024
#     ram <- avail.ram - 3072
#     if (ram < 4096) ram <- 4096
#   }
#   
#   if (!is.null(filename)){
#     if (!is.character(filename)) return(warning("'filename' has to be of type character!"))
#     if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath))) return(warning('IMGpath has to be a .tif-file.'))
#     filename  <- paste(c(' -out', filename), sep = ',', collapse= " ")
#   }
#   
#   if (!is.null(texture)){
#     if (!texture %in% c("simple","advanced","higher")) return(warning("'texture' can either be one of 'simple', 'advanced' or '' higher!"))
#     texture  <- paste(c(' -texture', texture), sep = ',', collapse= " ")
#   }
#   
#   basenam <- gsub(pattern = "[.][[:print:]]*$", replacement = "", IMGpath)
#   if (is.null(savedir)) savedir <- paste0(basenam, "_LSMS")
#   if (is.null(band)) band <- seq_len(terra::nlyr(terra::rast(IMGpath)))
#   
#   pizzR::setcreate.wd(savedir)
#   pizzR::OTB_init(path = OTBpath)
#   
#   for (i in seq_along(band)){
#     filename <- file.path(savedir, basename(IMGpath))
#     filename <- gsub(pattern = pizzR::extension(filename),
#                      replacement = sprintf("_b%s_%s_xr%s_yr%s_nbbin%s.tif",
#                                            band[i], texture, xrad, yrad, nbbin),
#                      x = filename)
#     
#     cmd1 <- paste0('otbcli_HaralickTextureExtraction', IMGpath, band.i, xrad, yrad, nbbin, texture, ram, filename)
#     cat('\n', cmd1, '\n')
#     if (!file.exists(filename)) pizzR::OTB_run(cmd1, ...)
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# files <- list.files('~/R/Github-Repos/WAMO/', pattern = 'metrics', full.names = T)
# 
# data <- as.data.frame(feather::read_feather(files))
# data <- data[1:50, 1:50]
# 
# classes <- names(data)[10]
# #test <- pizzR::ranFeatsel(data, names(data)[length(data)])
# #ranger::ranger(x=x, y=y)
# 
# classes_col <- which(colnames(data) == classes)
# 
# type = 'r'
# dots <- list()
# dots$x <- data[, -classes_col]
# if (type == 'c') dots$y <- as.factor(data[, classes_col]) else dots$y <- data[, classes_col]
# 
# 
# ranger_submod <- do.call(ranger::ranger, dots)
# ranger_submod
# 
# 
# 
# rf_submod <- do.call(randomForest::randomForest, dots)
# rf_submod
# 
