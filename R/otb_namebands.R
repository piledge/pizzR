OTB_namebands <- function(IMGpath,savedir=NULL,otbfunction='Haralick', verbose=T){

  if (!otbfunction %in% c("Haralick"))  return(warning("Export can currently only be 'Haralick'"))
  if (is.null(savedir))                 savedir <- file.path(dirname(IMGpath[1]), "OTBexport_namebands")
  if (!is.logical(verbose))             return(warning("'verbose' has to be of type logical"))
  pizzR::setcreate.wd(savedir)

  for (i in seq(IMGpath)){
    if (!any(c('tif', 'tiff') %in% tools::file_ext(IMGpath[i]))) return(warning('IMGpath has to be a .tif-file.'))
    rst <- terra::rast(IMGpath[i])

    if (otbfunction == 'Haralick'){
      nbands <- terra::nlyr(rst)
      if (!(nbands %in% c(8, 10, 11))) return(warning('Not a valid OTB-Haralick-export'))
      if (nbands == 8) bandnames <- c("Energy", "Entropy", "Correlation", "InverseDifferenceMoment", "Inertia", "ClusterShade", "ClusterProminence", "HaralickCorrelation")
      if (nbands == 10) bandnames <- c("Mean", "Variance", "Dissimilarity", "SumAverage", "SumVariance", "SumEntropy", "DifferenceofEntropies", "DifferenceofVariances", "IC1", "IC2")
      if (nbands == 11) bandnames <- c("ShortRunEmphasis", "LongRunEmphasis", "GreyLevelNonuniformity", "RunLengthNonuniformity", "RunPercentage", "LowGreyLevelRunEmphasis", "HighGreyLevelRunEmphasis", "ShortRunLowGreyLevelEmphasis", "ShortRunHighGreyLevelEmphasis", "LongRunLowGreyLevelEmphasis", "LongRunHighGreyLevelEmphasis")
    }

    names(rst) <- paste(basename(pizzR::file_path_sans_ext(IMGpath)), bandnames, sep='_')
    if (verbose) cat("               \r", paste0(pizzR::Systime(), ": remaining loops: ", length(IMGpath) - i, ))
    pizzR::writeslimRaster(rst, file.path(savedir, basename(IMGpath[i])))
  }
}
