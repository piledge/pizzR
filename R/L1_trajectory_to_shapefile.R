L1_trajectory_to_shapefile <- function(x=NULL,y=NULL,reduce=T,crs.origin=4326,crs.project=NULL,wfeather=F,wcsv=F, ...){
  if (is.null(x))                                 return(warning("Input file missing!"))
  if (!is.character(x))                           return(warning("'x' has to be of type character!"))
  if (is.null(y))                                 y <- file.path(dirname(x), 'Trajectory')
  if (!is.character(y))                           return(warning("'y' has to be of type character!"))
  if (!pizzR::extension(x) %in% c('.txt'))        return(warning("Input has to be a DJI L1 output trajectory '.txt'-file "))
  if (!is.logical(reduce) & !is.numeric(reduce))  return(warning("'reduce' has to be of type character or logical!"))
  if (!is.numeric(crs.origin))                    return(warning("'crs.origin' has to be of type numeric!"))
  if (!is.numeric(crs.project))                   return(warning("'crs.project' has to be of type numeric!"))

  pizzR::package.install(c('feather', 'terra'), verbose = 1)

  cat(paste0('\n', pizzR::Systime(), ": Loading data ..."))
  traject_file   <- file(x)
  header_names   <- scan(traject_file, what = '', nlines = 1, sep='')
  data           <- suppressWarnings(read.table(x, header = F, sep='', skip=2))
  colnames(data) <- header_names[-1]
  data.points    <- nrow(data)

  if (is.logical(reduce) | is.numeric(reduce))  cat(paste0('\n', pizzR::Systime(), ': Subsetting ...'))
  if (reduce){
    reduce <- trunc(sqrt(data.points))
    data <- data[sample(data.points, reduce),]
  }
  if (is.numeric(reduce)) data <- data[sample(data.points, reduce),]

  fact <- 180 / pi
  
  fname <- file.path(y, sub('Zenmuse-L1-mission_sbet.txt', 'trajectory', basename(x)))

  pizzR::setcreate.wd(y)
  cat(paste0('\n', pizzR::Systime(), ": Write data to disk '", y, "'"))
  if (wfeather) feather::write_feather(data, paste0(fname, '.feather'))
  if (wcsv)     write.csv2(data, paste0(fname, '.csv'))

  fparameters             <- list(...)
  fparameters$northing    <- data$Longitude * fact
  fparameters$easting     <- data$Latitude  * fact
  fparameters$filename    <- paste0(fname, '.shp')
  fparameters$crs.origin  <- crs.origin
  fparameters$crs.project <- crs.project

  do.call(pizzR::tableToSpatialpoints, fparameters)

  cat(paste0('\n', pizzR::Systime(), ': Done ...\n'))
  invisible(terra::vect(file.path(y, paste0(fname, '.shp'))))
}
