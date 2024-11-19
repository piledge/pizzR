\name{uniform_savename}
\alias{uniform_savename}
\title{Easy creation of standardized filenames for output.}
\usage{
uniform_savename(file_name, file_extension, iterator=NULL, width=5)
}
\arguments{
\item{file_name}{character. Filename. Can also be a concatenated string.}
\item{file_extension}{character. Extension of output-file.}
\item{Iterator}{numeric. Iterator if used in loops.}
\item{width}{numeric. Number of digits for the Iterator.}
}
\description{
Easy creation of standardized filenames for output.
}
\examples{
library(pizzR)

# Without loop iterator
uniform_savename('testfile', '.csv')

# With loop iterator
uniform_savename('testfile', '.csv', iterator = 3)
}
