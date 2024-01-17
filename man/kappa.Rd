\name{kappa}
\alias{kappa}
\title{Calculation of cohens kappa.}
\usage{
  kappa()
}
\arguments{
  \item{pred}{Vector of predicted values.}
  \item{ref}{Vector of reference values.}
}
\description{
  Easy calculation of cohens kappa.
}
\examples{
  library(pizzR)

  # creating a bad dataset
  pred <- factor(c("A", "A", "B", "C", "B", "B", "B", "B", "A", "B", "B", "B", "B"))
  ref <- factor(c("A", "B", "B", "B", "B", "A", "B", "B", "B", "B", "B", "B", "C"))

  # Calculate cohens kappa
  kappa(pred, ref)
}
