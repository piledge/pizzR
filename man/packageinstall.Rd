\name{package.install}
\alias{package.install}
\title{Check if packages/Github-Repositories are installed. If not, do so.}
\usage{
package.install(x)
}
\arguments{
\item{x}{Package}
}
\description{
Check if packages/Github-Repositories are installed. If not, do so.
}
\examples{
library(pizzR)

# check/install one package
package.install("ggplot2")

# check/install multiple packages and Github-Repositories
package.install(c("piledge/pizzR","ggplot2"))
}
