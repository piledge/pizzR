\name{quapply}
\alias{quapply}
\title{Columnwise calculating of a dataframes quantiles}
\usage{
  quapply()
}
\arguments{
  \item{x}{Dataframe.}
  \item{qu}{Quantiles to calculate.}
}
\description{
  Columnwise calculating of a dataframes quantiles.
}
\examples{
  library(pizzR)

  sampledata <- data.frame(red=sample(100, replace = T), green=sample(100, replace = T),
                           blue=sample(100, replace = T), infrared=sample(100, replace = T))

  # Calculate columnwise 0.3 quantile of a dataframe
  quapply(sampledata, 0.3)

  # Calculate columnwise 0.25, 0.50 and 0.75 quantile of a dataframe
  quapply(sampledata, c(0.25,0.50, 0.75))
}
