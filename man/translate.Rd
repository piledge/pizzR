\name{translate}
\alias{translate}
\title{To translate the values of a dataframes colums.}
\usage{
translate()
}
\arguments{
\item{x}{dataframe's column.}
\item{y}{dataframe including column named 'old' and 'new'.}
\item{verbose}{logical. console output.}
}
\description{
Translates the values of a dataframes colum by use of a dictionary-dataframe.
}
\examples{
library(pizzR)

data <- data.frame(id=1:10, species=sample(3,10,replace=T))
dictionary <- data.frame(old=1:3,new=c("oak","beech","pine"))

# can be used to overwrite existing column
data$species_new <- translate(data$species, dictionary)
data

# can be used to overwrite existing column
data$species <- translate(data$species, dictionary)
data
}
