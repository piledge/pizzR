L1_trajectory_to_shapefile <- function(x, reduce=T, crs.origin=4326, crs.project=NULL, outpath=NULL, wfeather=F, wcsv=F){
  if (!is.character(x))                           return(warning("'x' has to be of type character!"))
  if (!pizzR::extension(x) %in% c('.txt'))        return(warning("Input has to be a DJI L1 output trajectory '.txt'-file "))
  if (!is.logical(reduce) & !is.numeric(reduce))  return(warning("'reduce' has to be of type character or logical!"))
  if (!is.character(crs.origin))                  return(warning("'crs.origin' has to be of type numeric!"))
  if (!is.character(crs.project))                 return(warning("'crs.project' has to be of type numeric!"))
  if (is.null(outpath))                           outpath <- file.path(dirname(x), 'Trajectory')
  if (!is.character(outpath))                     return(warning("'outpath' has to be of type character!"))

  pizzR::setcreate.wd(outpath)

  traject_file <- file(x)
  header_names <- scan(traject_file, what = "", nlines = 1, sep="", quote = "\"",)
  roh <- read.table(x, header = F, sep='', skip=2)
  colnames(roh) <- header_names[-1]
  roh$Latitude  <- (roh$Latitude * (180/pi))
  roh$Longitude <- (roh$Longitude * (180/pi))

  if (reduce){
    reduce <- sqrt(nrow(roh))
    roh <- roh[sample(nrow(roh), reduce),]
  }
  if (is.numeric(reduce)) roh <- roh[sample(nrow(roh), reduce),]

  fname <- basename(x)
  fname <- sub('Zenmuse-L1-mission_sbet.txt', 'trajectory', fname)

  if (wfeather) feather::write_feather(roh, file.path(outpath, paste0(fname, '.feather')))
  if (wcsv)     write_csv2(roh, file.path(outpath, paste0(fname, '.csv')))
  pizzR::tableToSpatialpoints(northing = roh$Longitude, easting = roh$Latitude,
                              filename = file.path(outpath, paste0(fname, '.shp')),
                              crs.origin = crs.origin, crs.project = crs.project)
}
