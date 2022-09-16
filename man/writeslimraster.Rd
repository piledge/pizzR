\name{writeslimRaster}
\alias{writeslimRaster}
\title{wrapper-function for optimized export of spatialraster}
\usage{
writeslimRaster(x, filename, compression=TRUE, overwrite=TRUE, ...)
}
\arguments{
x
SpatRaster

filename
character. Output filename. Can be a single filename, or as many filenames as nlyr(x) to write a file for each layer

compression
logical. Turns loss-free compression to maximum. Calculated by a random-sample of pixels

overwrite
logical. If TRUE, filename is overwritten

...	
additional arguments for for writing files. See writeRaster() of raster- or terra-package
}
\description{
Write a raster object of both, terra- and raster-package, to a file.
}
\examples{
library(pizzR)
change.terraOptions()

r <- rast(nrows=5, ncols=5, vals=1:25)

# create a temporary filename for the example
f <- file.path(tempdir(), "test.tif")

# export a compressed raster
writeslimRaster(r, f)

# export a uncompressed raster
writeslimRaster(r, f, compression=FALSE)
}
