\name{slice.matrix}
\alias{slice.matrix}
\title{slice matrix into rows or colums}
\usage{
slice.matrix()
}
\arguments{
\item{x}{matrix.}
\item{nslices}{integer. Number of slices}
\item{windowsize}{integer. Define overlapping rows or columns for neighbouring slices}
\item{direction}{character. Slice either horizontal ('h') or vertical ('v')}
}
\description{
Returns a list including rows of colums of a sliced matrix. Can be used to split a Matrix into several pieces prior to parallel computing.
}
\examples{
library(pizzR)

# define a matrix
mtx <- matrix(c(1:250), nrow=5)


# slice the matrix in 5 vertical slices
slice.cols <- slice.matrix(mtx, nslices=5, windowsize = 3, direction='v')


# take a look on the colnumbers
for (i in 1:length(slice.cols)) print(slice.cols[[i]])


# select the colums of the slices
for (i in 1:length(slice.cols)) print(mtx[, slice.cols[[i]]])