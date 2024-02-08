L1_trajectory_to_shapefile <- function(x=NULL,y=NULL,reduce=T,crs.origin=4326,crs.project=NULL,wfeather=F,wcsv=F){
  if (is.null(x))                                 return(warning("Input file missing!"))
  if (!is.character(x))                           return(warning("'x' has to be of type character!"))
  if (is.null(y))                                 y <- file.path(dirname(x), 'Trajectory')
  if (!is.character(y))                           return(warning("'y' has to be of type character!"))
  if (!pizzR::extension(x) %in% c('.txt'))        return(warning("Input has to be a DJI L1 output trajectory '.txt'-file "))
  if (!is.logical(reduce) & !is.numeric(reduce))  return(warning("'reduce' has to be of type character or logical!"))
  if (!is.numeric(crs.origin))                    return(warning("'crs.origin' has to be of type numeric!"))
  if (!is.numeric(crs.project))                   return(warning("'crs.project' has to be of type numeric!"))

  pizzR::package.install(c('feather'), verbose = 1)

  cat(paste0('\n', pizzR::Systime(), ": Loading data ..."))
  traject_file  <- file(x)
  header_names  <- scan(traject_file, what = '', nlines = 1, sep='')
  roh           <- suppressWarnings(read.table(x, header = F, sep='', skip=2))
  colnames(roh) <- header_names[-1]
  roh$Latitude  <- roh$Latitude * (180/pi)
  roh$Longitude <- roh$Longitude * (180/pi)

  if (is.logical(reduce) | is.numeric(reduce))  cat(paste0('\n', pizzR::Systime(), ': Subsetting ...'))
  if (reduce){
    reduce <- sqrt(nrow(roh))
    roh <- roh[sample(nrow(roh), reduce),]
  }
  if (is.numeric(reduce)) roh <- roh[sample(nrow(roh), reduce),]

  fname <- basename(x)
  fname <- sub('Zenmuse-L1-mission_sbet.txt', 'trajectory', fname)

  pizzR::setcreate.wd(y)
  cat(paste0('\n', pizzR::Systime(), ": Write data to disk '", y, "'"))
  if (wfeather) feather::write_feather(roh, file.path(y, paste0(fname, '.feather')))
  if (wcsv)     write.csv2(roh, file.path(y, paste0(fname, '.csv')))
  pizzR::tableToSpatialpoints(northing = roh$Longitude, easting = roh$Latitude,
                              filename = file.path(y, paste0(fname, '.shp')),
                              crs.origin = crs.origin, crs.project = crs.project)
  cat(paste0('\n', pizzR::Systime(), ': Done ...\n'))
}
