\name{split.vector}
\alias{split.vector}
\title{Split a vector into parts}
\usage{
  split.vector()
}
\arguments{
  \item{x}{numeric. Vector to split}
  \item{qu}{Number of parts to split.}
}
\description{
  Split a vector into parts to create subsets. Remaining size will be handled at the last part.
}
\examples{
  library(pizzR)

  sampledata <- matrix(sample(100), ncol = 10)
  colums <- seq(ncol(sampledata))

  # split a dataset into 5 parts
  groups <- split.vector(colums, 5)
  sampledata[,groups == 1]
  sampledata[,groups == 2]

  # split a dataset into 3 parts
  groups <- split.vector(colums, 3)
  sampledata[,groups == 2]
  sampledata[,groups == 3]
}

