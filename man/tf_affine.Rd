% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transfromation.R
\name{tf_affine}
\alias{tf_affine}
\alias{tf_rotate}
\alias{tf_flip}
\alias{tf_shift}
\alias{tf_shear}
\alias{tf_scale}
\title{Affine Transformation}
\usage{
tf_affine(
  object,
  type = c("none", "translate", "scale", "rotate", "shear", "reflect"),
  by.each.cross = F,
  x = 0,
  y = 0,
  angle = NA,
  scale.x = NA,
  scale.y = NA,
  counterclockwise = FALSE,
  relative = T,
  crosses = NULL,
  layout = "transforming"
)

tf_rotate(object, x = 0, y = 0, angle = 90, ...)

tf_flip(object, axis = c("x", "y"), ...)

tf_shift(object, x = 0, y = 0, ...)

tf_shear(object, axis = c("x", "y"), angle = 15, ...)

tf_scale(object, x = 0, y = 0, scale.x = 1, scale.y = 1, ...)
}
\arguments{
\item{object}{a CrossLink object}

\item{type}{affine type}

\item{by.each.cross}{transformation to be performed by each cross}

\item{x, y}{coordiantes of transformation center for \code{tf_rotate};
offset in x,y axies for \code{tf_shift};
scale about this position for \code{tf_scale};
scale \code{x} and \code{y} along x, y axies, respectively, for \code{tf_scale_xy}}

\item{angle}{transformation angle in degree}

\item{scale.x, scale.y}{scale size for type \code{scale}}

\item{counterclockwise}{angle in counterclockwise for transformation}

\item{relative}{x,y is relative coordinates in range c(0,1)}

\item{crosses}{only these crosses will be transformed}

\item{layout}{layout name to save}

\item{...}{other parameters passed to tf_affine}

\item{axis}{along which axis to flip or shear}
}
\value{
an updated CrossLink object
}
\description{
Wrapper functions for transformations,
rotate: \code{tf_rotate}, flip: \code{tf_flip}, shift: \code{tf_shift} shear: \code{tf_shear}, scale: \code{tf_scale} and \code{tf_scale_xy}
}
\details{
\subsection{learn more about 2D affine and perspective tranformation matrix:}{
\itemize{
\item https://en.wikipedia.org/wiki/Transformation_matrix
\item https://www.cnblogs.com/bnuvincent/p/6691189.html
\item \code{Rconic::`Affine planar transformations matrix`}
}
}
}
