\name{plot.rstshp}
\alias{plot.rstshp}
\title{To plot a spatialraster with superimposed spatialvecor in one command.}
\usage{
plot.rstshp()
}
\arguments{
\item{x}{SpatRaster or SpatVector. Defines if raster- or terra-package is used for plot.}
\item{y}{SpatRaster or SpatVector.}
}
\description{
To plot a spatialraster with superimposed spatialvecor in one command.
Can also be used to superimpose two spatialvecors.
}
\examples{
library(pizzR)
change.terraOptions()

r <- rast(nrows=10, ncols=10, vals=sqrt(sample(100, 100)))

v1 <- vect("POLYGON ((-90 50, 40 60, 60 -50, -30 -80))")
v2 <- vect("POLYGON ((-80 40, 30 50, 50 -40, -20 -70))")

# plot raster with superimposed vector
plot.rstshp(r, v1)

# it's also possible to plot vector with superimposed vector
plot.rstshp(v1, v2)
}
