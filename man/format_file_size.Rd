\name{format_file_size}
\alias{format_file_size}
\title{Filesize-formatter.}
\usage{
  format_file_size()
}
\arguments{
  \item{filesize}{in Bytes.}
}
\description{
  The format_file_size function formats a given file size (in bytes) into a more readable format, represented in GiB (Gibibyte), MiB (Mebibyte), KiB (Kibibyte), or Bytes.
}
\examples{
  library(pizzR)

  # Format to pretty output
  result <- format_file_size(1234567890)
  print(result)  # Output: "1.15 GiB"

}
