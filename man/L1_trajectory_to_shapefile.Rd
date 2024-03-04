\name{confMatrix}
\alias{confMatrix}
\title{Calculation of Confusion Matrices.}
\usage{
  confMatrix()
}
\arguments{
  \item{pred}{Vector of predicted values.}
  \item{ref}{Vector of reference values.}
}
\description{
  Returning a list containing Confusion Matrix, Producers- , Users-, Overall-Accuracy and Cohens Kappa.
}
\examples{
  library(pizzR)
  library(lidR)

  ### load sample-raster
  i.path <- system.file('extdata/DJI_20230825110812_0001_Zenmuse-L1-mission_sbet.txt', package="pizzR")
  r <- rast(i.path)
  plot(r)
}
