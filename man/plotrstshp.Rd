\name{plot.rstshp}
\alias{plot.rstshp}
\title{To plot a spatialraster with superimposed spatialvecor in one command}
\usage{
plot.rstshp()
}
\arguments{
\item{x}{SpatRaster or SpatVector}
\item{y}{SpatRaster or SpatVector}
}
\description{
To plot a spatialraster with superimposed spatialvecor in one command.
Can also be used to superimpose two spatialvecors.
}
\examples{
library(pizzR)
change.terraOptions()

r <- rast(nrows=10, ncols=10, vals=sqrt(sample(100, 100)))

e <- ext(r)
v1 <- e-10
v2 <- e-20

# plot raster with superimposed vector
plot.rstshp(r, v1)

# plot vector with superimposed vector
plot.rstshp(r, v2)
}
