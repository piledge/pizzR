\name{split.group}
\alias{split.group}
\title{Split groups of a vector into parts}
\usage{
  split.group()
}
\arguments{
  \item{x}{numeric. IDs of the groups}
  \item{qu}{Number of parts to split.}
}
\description{
  Split groups of a vector into parts. Remaining size will be handled at the last part.
  Can be used to split a raster-extract of the \code{raster}- or \code{terra}-package into subsets.
}
\examples{
  library(pizzR)

  dataset <- c(rep(1,3), rep(2, 3), rep(3,1), rep(4,3), rep(5, 2), rep(6,4), rep(7,5))

  # split dataset into 5 groups
  (data.frame(dataset=dataset, groups=split.groups(dataset, 3)))

  # split dataset into 4 groups
  (data.frame(dataset=dataset, groups=split.groups(dataset, 4)))
}

