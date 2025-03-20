\name{aws_S2_lookup}
\alias{aws_S2_lookup}
\title{Funcion to find S2-Scenes in AWS.}
\usage{
aws_S2_lookup(rasterobject, filename, compression=TRUE, overwrite=TRUE, verbose=T, ...)
}
\arguments{
\item{tile}{character. Tile name of requested area.}
\item{years}{numeric. Range of years to lookup.}
\item{months}{numeric. Range of months to lookup.}
\item{days}{numeric. Range of days to lookup.}
\item{verbose}{logical. Console output.}
}
\description{
Simple function to search for Sentinel-2 L2A-products in AWS. Returns a \code{data.frame()} including available Scenes in given time windows.
}
\examples{
library(pizzR)

# specify tile and time range
tile <- '33UWP'
years <- c(2022, 2024)
months <- 6:8
days <- 1:31

# look for available scenes in specified timerange. This may take a while depending on your search-parameters
aws_S2_lookup(tile=tile, years=years, months=months, days=days)
}


