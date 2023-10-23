\name{opt.datatype}
\alias{opt.datatype}
\title{Returns a list including the optimal export-datatype and information if a raster-object contains NA-Values}
\usage{
opt.datatype(x)
}
\arguments{
\item{x}{SpatRaster, RasterLayer, RasterBrick, RasterStack}
}
\description{
Returns a list including the optimal export-datatype.
Works for both, raster- and terra-package.
NOTE: raster-package was not fully tested!
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
