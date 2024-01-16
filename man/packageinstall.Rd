\name{package.install}
\alias{package.install}
\title{Check if packages are installed. If not, do so.}
\usage{
package.install(x)
}
\arguments{
\item{x}{Package}
}
\description{
Check if packages are installed. If not, do so.
}
\examples{
library(pizzR)

# check/install one package
package.install("pizzR")

# check/install multiple packages
package.install(c("pizzR","pizzR"))
}
