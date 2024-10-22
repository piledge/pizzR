\name{loop_progress}
\alias{loop_progress}
\title{
Sneaky formatted console output of the current loop progress.
}
\usage{
loop_progress(i, total_digits=3, text='Loop')
}
\arguments{
  \item{i}{A numeric to foward the current loop index to the function.}
  \item{total_digits}{A numeric value specifying the number of characters for the counter.}
  \item{text}{A String to customize the console output..
}
\description{
Sneaky formatted console output of the current loop progress. Can be used to viszualize the progress of the loop.
}
\examples{
library(pizzR)

# Example 1
loop_progress(i=42, total_digits=3)

# Example 2
loop_progress(i=4242, total_digits=6, text='Inner Loop')
}
