\name{change.terraOptions}
\alias{change.terraOptions}
\title{Simple modification of \code{terra}-package parameters.}
\usage{
change.terraOptions()
}
\arguments{
\item{changetmpdir}{logical. If \code{TRUE}, path of \code{tmpdir} is required.}
\item{tmpdir}{character. Path to write temporary files if RAM is exeeded.}
\item{OSRAM.remaining}{integer. Memory remaining for operating system. To small values may lead to system instability.}
\item{progress}{non-negative integer. A progress bar is shown if the number of chunks in which the data is processed is larger than this number. No progress bar is shown if the value is zero.}
\item{verbose}{logical. Console output.}
\item{...}{additional arguments for writing files. See terraOptions() of terra-package.}
}
\description{
Simple modification of 'terra'-package parameters.
}
\examples{
library(pizzR)
library(terra)

# default options of the terra-package
terraOptions()


# default options by using change.terraOptions() for maximum RAM usage
change.terraOptions(verbose=TRUE)

# further options passed to change.terraOptions()
change.terraOptions(changetmpdir=TRUE,tmpdir=tempdir(),OSRAM.remaining=3,verbose=TRUE)
}
