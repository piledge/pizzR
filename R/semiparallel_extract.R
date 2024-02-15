semiparallel.extract <- function(SpatRaster=NULL, SpatVector=NULL, core=1, ncores=2, opath=NULL){

  pizzR::package.install(c("feather", "raster", "terra"), verbose = 1)

  if (class(SpatRaster)[1] != "SpatRaster") return(warning("Not a suitable rasterfile!\n"))
  if (class(SpatVector)[1] != "SpatVector") return(warning("Not a suitable vectorfile!\n"))

  chunks.id <- pizzR::split.vector(seq(shp), groups = ncores)
  chunks <- vector('list', ncores)
  for (i in seq(ncores)) chunks[[i]] <- shp[chunks.id == i]

  extr <- terra::extract(rst, chunks[[core]])

  nobjects <- as.numeric(table(chunks.id))
  nobjects <- c(0, nobjects[-length(nobjects)])
  newid <- cumsum(nobjects)

  extr$ID <- extr$ID + newid[core]

  filename <- paste0(sprintf(paste0('extract_%0', nchar(ncores), '.f'), core), '.feather')
  if (is.null(opath)) pizzR::setcreate.wd(file.path(getwd(), 'parallel_extract')) else pizzR::setcreate.wd(opath)

  feather::write_feather(extr, filename)
  cat(paste0("\n", pizzR::Systime(), ": '",filename, "' written to '", getwd(), "'/n"))
}
