\name{aws_S2_process_tile_region_scl}
\alias{aws_S2_process_tile_region_scl}
\title{Funcion to process full S2-tiles from AWS.}
\usage{
aws_S2_process_tile_region_scl(path, tile, shp_path, crop_ext_buffer=NULL, export_path=NULL, prefix = 's2', mask=F, preview=F, check_values=T, bad_values=c(0, 3, 8, 9, 10))
}
\arguments{
\item{path}{character. Path to find raw files.}
\item{tile}{character. Tile name of requested area.}
\item{shp_path}{character. Path to find shapefile to crop/mask.}
\item{crop_ext_buffer}{numeric. Buffer size to overcrop.}
\item{export_path}{character. Path to write processed files.}
\item{prefix}{character. To customize export filenames.}
\item{mask}{logical. Pixel-filtering by mask. Default is filtering by extent.}
\item{preview}{logical. Plot preview of written file.}
\item{check_values}{logical. Filter scenes for bad pixels.}
\item{bad_values}{numeric. Values to filter scenes containing unwanted pixel classes. See Sentinel-2 L2A Scene Classification Map for details. Default is \code{c(0, 3, 8, 9, 10)}.}
}
\description{
Simple function to process Sentinel-2 L2A-products downloaded from AWS. Best to use in combination with \code{aws_S2_lookup()} and \code{aws_S2_lookup()}.
Specify a shapefile to crop requested area. \code{if (mask)}, the filtering will be done only on pixels within polygons. However, export will be done with extent.
}
\examples{
library(pizzR)
library(terra)

# download a single scene
aws_S2_download_tile('33UWP', 2022, 6, 1)

# specify shapefile
shp_path <- system.file('extdata/crop_shp/crop_shp.shp', package="pizzR")

# process tile
aws_S2_process_tile_region_scl(path = tempdir(), shp_path = shp_path, tile = '33UWP', preview = TRUE)

# show output
rfile <- list.files(file.path(tempdir(), 'export'), full.names = T, pattern = '.tif')
rst <- rast(rfile)
plot(rst)
}
