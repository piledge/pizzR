\name{change_dots}
\alias{change_dots}
\title{Simple wrapper-function to replace dots in strings.}
\usage{
change_dots(string_data, replacement)
}
\arguments{
\item{string_data}{character. Tile name of requested area.}
\item{replacement}{numeric. Range of years to lookup.}
}
\description{
Can be used if dots lead to unwanted behavior in some cases.
}
\examples{
library(pizzR)

data <- c('example.tif', 'abc.def.ghi.tif', 'band.1', 'band.2', 'band.3')

change_dots(data)
change_dots(data, replacement = '')
}
