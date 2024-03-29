% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosslink.R
\name{crosslink}
\alias{crosslink}
\title{Generator of class CrossLink}
\usage{
crosslink(
  nodes,
  edges,
  cross.by,
  gaps = NULL,
  spaces = c("equal", "partition", "flank"),
  xrange = NULL,
  yrange = NULL,
  key.by = 1,
  src.by = 1,
  tar.by = 2,
  odd.rm = FALSE
)
}
\arguments{
\item{nodes}{a data frame with unique keys in the 1st (settable by \code{key.by}) column and metadata (used for aesthetic of nodes) in the other columns.}

\item{edges}{a data frame with source and target nodes in the first two (settable by \code{src.by} and \code{tar.by}) columns and other metadata.}

\item{cross.by}{the name of a column in \code{nodes}, by which nodes will be grouped into different crosses.
To control the order of crosses and the order of nodes, set factor levels for the corresponding column in \code{nodes}. see \code{\link{factor}} or \code{\link[forcats:fct_relevel]{forcats::fct_relevel()}}}

\item{gaps}{a numeric vector of n-1 length, n is the number of crosses (that is \code{length(object@cross)}).
It's a vector of gaps between adjacent crosses.
set NA or NULL to use equally divided gaps.}

\item{spaces}{set \code{"equal"} for equidistant globally in all crosses,
set \code{"partition"} for equidistant separately in each cross,
or set \code{"flank"}, which is similar to \code{"partition"} but with top/bottom flank anchors included.
Default: \code{equal}.}

\item{xrange, yrange}{range of x and y. set NULL or NA to ignore this.}

\item{key.by}{name or index of the column in \code{nodes} to be used as key of nodes}

\item{src.by}{name or index of the column in \code{edges} to be used as source of links (edges)}

\item{tar.by}{name or index of the column in \code{edges} to be used as target of links (edges)}

\item{odd.rm}{remove odd nodes that are not included in edges (without links).}
}
\value{
a CrossLink object
}
\description{
Achieve a initialized CrossLink object by providing nodes, edges and cross.by.
}
\details{
IMPORTANT! The colnames of 'node', 'cross', 'node.type', 'x', 'y', 'degree' MUST NOT BE included in nodes and edges!
}
