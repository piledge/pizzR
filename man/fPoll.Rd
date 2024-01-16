\name{fPoll}
\alias{fPoll}
\title{Returns the Pollanschütz-value for a given species.}
\usage{
fPoll()
}
\arguments{
\item{species}{integer. Species-ID. Execute \code{fPoll.dictionary} for details.}
\item{dbh_cm}{numeric. Diameter breast height in centimeter.}
\item{height_m}{numeric. Tree height in meter.}

}
\description{
Returns the Pollanschütz-value for a given species.
}
\examples{
library(pizzR)

# see dictionary for species
fPoll.dictionary

# picea abies with 25.4cm dbh and 30m tree height
fPoll(1, 25.4, 30)

# abies alba with 15.9cm dbh and 18m tree height
fPoll(2, 15.9, 18)

# Pollanschütz, J. 1974: Zuwachsschätzung im Rahmen von Stichprobe-inventuren der Forsteinrichtung
}
