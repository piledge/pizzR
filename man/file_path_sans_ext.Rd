\name{file_path_sans_ext}
\alias{file_path_sans_ext}
\title{Re-implementation of \code{\link{file_path_sans_ext}} in \code{tools}. This
version can handle "." just before the file extenstion, unlike the original
implementation.}
\usage{
file_path_sans_ext(x, compression = FALSE)
}
\arguments{
\item{x}{Character vector giving file paths.}

\item{compression}{Logical: should compression extension '.gz', '.bz2' or
'.xz' be removed first?}
}
\value{
File path without the file extension.
}
\description{
Re-implementation of \code{\link{file_path_sans_ext}} in \code{tools}. This
version can handle "." just before the file extenstion, unlike the original
implementation.
}
\examples{
library(pizzR)

# get the paths without extensions
paths <- file.path(tempdir(), c('examplefile_1.txt', 'examplefile_3.png', 'examplefile_2.gif'))
file_path_sans_ext(paths)

# get the filenames without extensions
files <- basename(paths)
file_path_sans_ext(files)

}
\author{
Joona Lehtomaki \email{joona.lehtomaki@gmail.com}
}
