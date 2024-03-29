% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{interval_inclusion}
\alias{interval_inclusion}
\title{get values by interval inclusion}
\usage{
interval_inclusion(
  x,
  interval = NULL,
  inclusion = c("left", "both", "right", "neither")
)
}
\arguments{
\item{x}{a vector of values}

\item{interval}{the range of inclusion to be appied}

\item{inclusion}{defining inclusion}
}
\value{
a vector
}
\description{
get values by interval inclusion
}
\examples{
x <- seq(0, 1, length.out = 9)
interval_inclusion(x, interval = c(0.5, 1), inclusion = "left")
interval_inclusion(x, interval = c(0.5, 1), inclusion = "right")
interval_inclusion(x, interval = c(0.5, 1), inclusion = "both")
interval_inclusion(x, interval = c(0.5, 1), inclusion = "neither")

}
\keyword{internal}
