\name{opt.datatype}
\alias{opt.datatype}
\title{Returns the optimal export-datatype for raster- or terra-spatialraster}
\usage{
opt.datatype(x)
}
\arguments{
\item{x}{SpatRaster, RasterLayer, RasterBrick, RasterStack}
\item{samplesize}{integer. Number of pixels used of each bands to estimate the optimized datatype for the output raster}
}
\description{
Returns the optimal export-datatype for raster- or terra-spatialraster.
}
\examples{
library(pizzR)
change.terraOptions()

r1 <- rast(nrows=5, ncols=5, vals=sample(25, 25))
r2 <- rast(nrows=5, ncols=5, vals=sqrt(sample(25, 25)))

# returns optimal datatype
opt.datatype(r1)
opt.datatype(r2)
}
