\name{writeslimRaster}
\alias{writeslimRaster}
\title{Wrapper-function for optimized export of spatialraster}
\usage{
writeslimRaster(x, filename, compression=TRUE, overwrite=TRUE, ...)
}
\arguments{
\item{x}{SpatRaster}
\item{filename}{character. Output filename. Can be a single filename, or as many filenames as \code{nlyr(x)} to write a file for each layer}
\item{compression}{logical. If \code{TRUE}, turns loss-free compression to maximum. Calculated by a random-sample of pixels}
\item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
\item{samplesize}{integer. Number of pixels used of each bands to estimate the optimized datatype for the output raster}
\item{...}{additional arguments for writing files. See writeRaster() of raster- or terra-package}
}
\description{
Writes a raster object of both, terra- and raster-package, to a file.
}
\examples{
library(pizzR)
change.terraOptions()

r <- rast(nrows=5, ncols=5, vals=1:25)

# create a temporary filename for the example
f <- file.path(tempdir(), "example.tif")

# export a compressed raster
writeslimRaster(r, f)

# export a uncompressed raster
writeslimRaster(r, f, compression=FALSE)
}
