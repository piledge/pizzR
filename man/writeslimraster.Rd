\name{writeslimRaster}
\alias{writeslimRaster}
\title{Wrapper-function for optimized export of spatialraster.}
\usage{
writeslimRaster(rasterobject, filename, compression=TRUE, overwrite=TRUE, ...)
}
\arguments{
\item{rasterobject}{SpatRaster, RasterLayer, RasterBrick, RasterStack}
\item{filename}{character. Output filename. Can be a single filename, or as many filenames as \code{nlyr(x)} to write a file for each layer.}
\item{compression}{logical. If \code{TRUE}, turns loss-free compression to maximum. Calculated by a random-sample of pixels.}
\item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten.}
\item{datatype}{Could be either ESTIMATE do appreciate the optimal datatype for a given \code{x} or one of the following options: LOG1S/INT1S/INT1U/INT2S/INT2U/INT4S/INT4U/FLT4S/FLT8S!}
\item{...}{additional arguments for writing files. See writeRaster() of raster- or terra-package.}
}
\description{
Writes a raster object of both, terra- and raster-package, to a file. \code{datatype} and \code{NAflag} will be estimated by using the \code{pizzR:opt.datatype()}-function.
By default, compression-level is set to maximum loss-free compression.
}
\examples{
library(pizzR)
change.terraOptions()

rasterobject <- rast(nrows=5, ncols=5, vals=1:25)

# create a temporary filename for the example
filename <- file.path(tempdir(), "example.tif")

# export a compressed raster
writeslimRaster(rasterobject, filename)

# export a uncompressed raster
writeslimRaster(rasterobject, filename, compression=FALSE)
}
