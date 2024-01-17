\name{confMatrix}
\alias{confMatrix}
\title{Calculation of Confusion Matrices.}
\usage{
  confMatrix()
}
\arguments{
  \item{pred}{Vector of predicted values.}
  \item{ref}{Vector of reference values.}
}
\description{
  Returning a list containing Confusion Matrix, Producers- , Users-, Overall-Accuracy and Cohens Kappa.
}
\examples{
  library(pizzR)

  # creating a bad dataset
  pred <- factor(c("A", "A", "B", "C", "B", "B", "B", "B", "A", "B", "B", "B", "B"))
  ref <- factor(c("A", "B", "B", "B", "B", "A", "B", "B", "B", "B", "B", "B", "C"))

  # Calculate confusion matrix
  confMatrix(pred, ref)
}
