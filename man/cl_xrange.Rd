% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosslink.R
\name{cl_xrange}
\alias{cl_xrange}
\alias{cl_yrange}
\title{Retrieve ranges of X or Y axis}
\usage{
cl_xrange(object, layout = NULL, all = F, crosses = NULL, include.flank = T)

cl_yrange(object, layout = NULL, all = F, crosses = NULL, include.flank = T)
}
\arguments{
\item{object}{a CrossLink object}

\item{layout}{name of the layout to be retrived or set}

\item{all}{also return ranges for each cross. The range does not include header.}

\item{crosses}{subset crosses. NULL for all crosses.}
}
\value{
range of X or Y axis for the given layout
}
\description{
Retrieve ranges of X or Y axis
}
