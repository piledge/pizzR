\name{classmaker}
\alias{classmaker}
\title{
Group Numeric Values into Classes Based on a Specified Range.
}
\usage{
classmaker(x, class_range = 50, include.lowest = TRUE, right = FALSE)
}
\arguments{
  \numeric{x}{A numeric vector of values to be classified.}
  \numeric{class_range}{A numeric value specifying the size of each class interval. The default value is 50.}
  \boolean{include.lowest}{A logical value indicating whether the lowest (first) interval should be closed on the left (and open on the right) or not. The default is \code{TRUE}.}
  \boolean{right}{A logical value indicating whether intervals should be closed on the right (and open on the left) or not. The default is \code{FALSE}.}
}
\description{
This function categorizes numeric values into integer classes based on a specified class range.
}
\seealso{
  \code{\link{cut}} for similar functionality in R.
}
\examples{
library(pizzR)

# Example 1
values <- c(10, 75, 150, 225, 300)
classmaker(values, class_range = 100)

# Example 2
values <- c(10, 75, 150, 225, 300)
classmaker(values, class_range = 50)
}
