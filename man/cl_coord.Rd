% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosslink.R
\name{cl_coord}
\alias{cl_coord}
\title{Retrieve coordinates of nodes}
\usage{
cl_coord(object, nodes, relative = F, rel.by.cross = F, layout = NULL)
}
\arguments{
\item{object}{a CrossLink object}

\item{nodes}{keys of nodes to be retrieved}

\item{relative}{return relative values in the canvas or each cross (rel.by.cross = TRUE). values are in range of 0 to 1.}

\item{rel.by.cross}{return relative values to each cross.}
}
\value{
a vector of values
}
\description{
Retrieve coordinates of nodes
}
