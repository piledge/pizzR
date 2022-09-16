\name{setcreate.wd}
\alias{setcreate.wd}
\title{a useful combination of setwd() and dir.create()}
\usage{
setcreate.wd(x)
}
\arguments{
  \item{x}{path}
}
\description{
Can be used instead of setwd(). Creates recursive path prior to change of working directory.
}
\examples{
library(pizzR)

currend.wd <- getwd()

# create new folder if not exist and change working directory
setcreate.wd(paste0(getwd(),"/samplefolder"))

# change working directory if exist
setcreate.wd(currend.wd)
}
