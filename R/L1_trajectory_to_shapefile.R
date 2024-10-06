L1_trajectory_to_shapefile <- function(x=NULL,y=NULL,reduce=T,crs.origin=4326,crs.project=NULL,feather=F,csv=F, ...){
  stopifnot(
    !is.null(x),
    is.character(x),
    is.null(y) || is.character(y),
    pizzR::extension(x) %in% c('.txt'),
    is.logical(reduce) || is.numeric(reduce),
    is.logical(feather),
    is.logical(csv)
  )
  if (is.null(y)) y <- file.path(dirname(x), 'Trajectory')

  pizzR::package.install(c('feather', 'terra'), verbose = 1)

  cat(sprintf('\n%s: Loading data ...', pizzR::Systime()))
  traject_file   <- file(x)
  header_names   <- scan(traject_file, what = '', nlines = 1, sep='')
  data           <- suppressWarnings(read.table(x, header = F, sep='', skip=2))
  colnames(data) <- header_names[-1]
  data.points    <- nrow(data)

  if (is.logical(reduce) | is.numeric(reduce)) cat(sprintf('\n%s: Subsetting ...', pizzR::Systime()))
  if (reduce){
    reduce <- trunc(sqrt(data.points))
    data <- data[sample(data.points, reduce),]
  }
  if (is.numeric(reduce)) data <- data[sample(data.points, reduce),]

  fact <- 180 / pi

  fname <- file.path(y, sub('Zenmuse-L1-mission_sbet.txt', 'trajectory', basename(x)))

  pizzR::setcreate.wd(y)
  cat(sprintf("\n%s: Write data to disk '%s'", pizzR::Systime(), y))
  if (feather)  feather::write_feather(data, sprintf('%s.feather', fname))
  if (csv)      write.csv2(data, sprintf('%s.csv', fname))


  fparameters             <- list(...)
  fparameters$northing    <- data$Longitude * fact
  fparameters$easting     <- data$Latitude  * fact
  fparameters$filename    <- sprintf('%s.shp', fname)
  fparameters$crs.origin  <- crs.origin
  fparameters$crs.project <- crs.project

  do.call(pizzR::tableToSpatialpoints, fparameters)

  cat(sprintf("\n%s: Done ...\n", pizzR::Systime()))
  invisible(terra::vect(file.path(y, sprintf('%s.shp', fname))))
}
