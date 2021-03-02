#' convert a vector into a list
#'
#' convert x (a vector) into a list according to the group information (another vector) of elements in x.
#'
#' @param x a vector
#' @param group a vector with equal length of x, providing group information of elements in x
#' @param drop drop levels of x
#'
#' @return a list
#' @export
#'
#' @examples
#' tolist_by_group(x = 1:10, group = c(rep("A", 5), rep("B", 5)))
#'
tolist_by_group <- function(x, group, drop = F) {
  xg <- split(x, group, drop = drop)
  if(drop) xg <- lapply(xg, droplevels)
  return(xg)
}


#' convert a list into a data.frame
#'
#' convert list (a list with equal length of each element) into a data.frame
#'
#' @param list a list with equal length of its each element
#'
#' @return data.frame
#' @export
#'
#' @examples
#' todataframe_by_list(list(num = c(1:10), cat = c(rep("A", 5), rep("B", 5))))
#'
todataframe_by_list <- function(list){
  names <- names(list)

  # without names
  if(is.null(names)) names <- 1:length(list)

  # partially without names
  ind_null <- which(names == "")
  names[ind_null] <- ind_null

  do.call(rbind, lapply(1:length(list), function(x) {
    data.frame(name = rep(names[x], length(list[[x]])), value = list[[x]])
  }))
}

#' a helper function to set default value when inputting NULL or NA
#'
#' @param x input argument
#' @param default returned default value if x is NULL or NA
#'
#' @return x or default asis
#' @export
#'
#' @examples
#' nullna_default(NULL, c(1,2))
#'
nullna_default <- function(x, default) {
  if(is.na(x) || is.null(x)) return(default) else return(x)
}

#' cut a segment defined by a range in 1D coordinate into n pieces
#'
#' @param range a vector of 2 numeric values defining a segment
#' @param n cut the segment into \code{n} pieces
#' @param scale a vector of \code{n} numeric values to control segment sizes relatively. Set NA or NULL for equally dividing.
#' @param return.cutter only return the coordinate values of internal cutters
#'
#' @return a vector of n+1 or n-1 if \code{return.cutter}
#' @export
#'
#' @examples
#' segment_range(c(0,10), 3, scale = c(1, 2, 7), return.cutter = F)
#'
segment_range <- function(range = c(0, 1), n, scale = NA, return.cutter = T){
  scale <- nullna_default(scale, rep(1, n))
  if(length(scale) != n) stop("scale length must be equal to n!")
  x <- scales::rescale(cumsum(c(0, scale)), range(range))
  if(return.cutter){
    return(x[2:n]) # x length is n+1
  }else{
    return(x)
  }
}

#' convert a vector of characters to a vector of numeric coordinates
#'
#' @param x a vector of characters, which will be coerced into factor.
#' @param scale a vector of \code{length(x) + 1} numeric values to control adjacent spacing and flanks (the first and the last values). Set NA or NULL for equally dividing.
#' @param range a vector of 2 numeric values defining a interval, where \code{x} will be mapped.
#'
#' @return a vector of length(x)
#' @export
#'
#' @examples
#' vector_to_coord(1:4, scale = c(1,2,2,2,1), range(1, 9))
vector_to_coord <- function(x, scale = NA, range = c(0, 1)){ # scale = x+1  in length
  x <- factor(x)
  segment_range(
    range = range, n = length(levels(x)) + 1,
    scale = scale, return.cutter = T)[x]
}

#' a helper function to get names by integer indexes from data.frame or list
#'
#' @param x a data.frame or a list
#' @param ind a vector of integer indexes, whose element must be smaller than length of \code{x}
#'
#' @return a vector of characters
#'
#' @examples
#' index_to_name(data.frame(A = 1, B = 2), 1)
#'
index_to_name <- function(x, ind){
  if(is.numeric(ind)){
    if(max(ind) <= length(x))
      return(names(x)[ind])
    else
      stop("index is larger than length of x")
  }

  if(all(ind %in% names(x)))
    return(ind)
  else
    stop("index is not in names of x")
}



#' generate a 3*3 matrix for affine transformation
#'
#' @param type a affine transformation type
#' @param x transform value along x axis, used for
#' @param y transform value along y axis, used for
#' @param theta transform value of angle, usually in 0 to 2pi
#' @param counterclockwise control directions of theta or x,y if type is "shear"
#'
#' @return a 3*3 matrix
#' @description
#' See the following urls to learn more about 2D affine and perspective tranformation matrix:
#' https://en.wikipedia.org/wiki/Transformation_matrix
#' https://www.cnblogs.com/bnuvincent/p/6691189.html
#' \code{Rconic::`Affine planar transformations matrix`}
#'
#' Used arguments for different types:
#' none: ignore everything
#' translate: x, y
#' scale: x, y
#' rotate: x, y, theta (angle of the rotation)
#' shear: x, y
#' reflect: x, y, theta (angle from x axis)
#'
#'
#' @export
#'
#' @examples
#'
#' # eigen matrix
#' transform_matrix_affine(type = "none")
#'
#' # shift matrix for convert c(0, 0) to c(1, 2)
#' mat <- transform_matrix_affine(type = "translate", x = 1, y = 2)
#' mat %*% c(0, 0, 1)
#'
#' # rotation matrix for convert c(0, 0) to c(0, 2)
#' mat <- transform_matrix_affine(type = "rotate", x = 1, y = 1, theta = pi/2)
#' mat %*% c(0, 0, 1)
#'
#' # shear for convert c(1, 1) to c(2, 1) if along x or c(1, 2) if along y
#' mat <- transform_matrix_affine(type = "shear", x = pi*45/180, y = 0)
#' mat <- transform_matrix_affine(type = "shear", x = 0, y = pi*45/180, counterclockwise = T)
#'
#' # reflect for convert c(0, 0) to c(2, 2)
#' mat <- transform_matrix_affine(type = "reflect", x = 1, y = 1, theta = pi*45/180)
#'
transform_matrix_affine <- function(
  type = c("none", "translate", "scale", "rotate", "shear", "reflect"),
  x = 0, y = 0, theta = NA, counterclockwise = FALSE){
  # none: ignore everything
  # translate: x, y
  # scale: x, y
  # rotate: x, y, theta (angle of the rotation)
  # shear: x, y
  # reflect: x, y, theta (angle with x axis)
  if(counterclockwise) theta <- -theta
  switch(
    match.arg(arg = type, several.ok = FALSE),
    none = transform_matrix(),
    translate = transform_matrix(c = x, f = y),
    scale = transform_matrix(a = x, e = y),
    rotate = {
      if(!is.numeric(theta)) stop("parameter theta must be numeric for ROTATE!")
      transform_matrix(c = x, f = y) %*%
        transform_matrix(a = cos(theta), b = sin(theta), d = -sin(theta), e = cos(theta)) %*%
        transform_matrix(c = -x, f = -y)
    },
    shear = transform_matrix(b = tan(x * ifelse(counterclockwise, -1, 1)),
                             d = tan(-y * ifelse(counterclockwise, -1, 1))),
    reflect = {
      if(is.na(theta) || is.null(theta)){
        transform_matrix(c = x, f = y) %*%
          transform_matrix(a = -1, e = -1) %*%
          transform_matrix(c = -x, f = -y)
      }else{
        transform_matrix(c = x, f = y) %*%
          transform_matrix(a = cos(theta), b = sin(theta), d = -sin(theta), e = cos(theta)) %*%
          transform_matrix(a = 1, e = -1) %*%
          transform_matrix(a = cos(-theta), b = sin(-theta), d = -sin(-theta), e = cos(-theta)) %*%
          transform_matrix(c = -x, f = -y)
      }
    }
  )
}

#' a helper function for 3*3 matrix
#'
#' @param a 1st value of 3*3 matrix
#' @param b 2nd value of 3*3 matrix
#' @param c 3rd value of 3*3 matrix
#' @param d 4th value of 3*3 matrix
#' @param e 5th value of 3*3 matrix
#' @param f 6th value of 3*3 matrix
#' @param g 7th value of 3*3 matrix
#' @param h 8th value of 3*3 matrix
#' @param i 9th value of 3*3 matrix
#'
#' @return a 3*3 matrix as follows:
#' matrix(
#' a, b, c
#' d, e, f
#' g, h, i
#' )
#'
#' @export
#'
#' @examples
#' transform_matrix(a = 2, e = 2, i = 2)
#'
transform_matrix <- function(
  a = 1, b = 0, c = 0,
  d = 0, e = 1, f = 0,
  g = 0, h = 0, i = 1){
  matrix(c(a, b, c, d, e, f, g, h, i), nrow = 3, byrow = TRUE)
}


#' Transfrom 2D coordinates according to a affine transformation matrix
#'
#' @param x a vector of coordinates at x axis or a data.frame if y is NULL
#' @param y a vector of coordinates at y axis or NULL
#' @param matrix a 3*3 affine transformation matrix
#'
#' @return a matrix of transformed coordinates
#' @export
#'
#' @examples
#' # coordinate for a unit square
#' us <- expand.grid(c(0,1), c(0,1))[c(1,3,4,2),]
#' us_tf <- transform_by_matrix(x = us, matrix = transform_matrix_affine("rotate", x = 0, y =0, theta = pi/4))
#' library(ggplot2)
#' library(magrittr)
#' ggplot() +
#'   geom_polygon(mapping = aes(x = us[,1], y = us[,2]), fill = "blue", alpha = 0.3) +
#'   geom_point(mapping = aes(x = us[,1], y = us[,2]), color = "blue", size = 3) +
#'   geom_polygon(mapping = aes(x = us_tf[,1], y = us_tf[,2]), fill = "red", alpha = 0.3) +
#'   geom_point(mapping = aes(x = us_tf[,1], y = us_tf[,2]), color = "red", size = 3) +
#'   geom_segment(mapping = aes(x = us[,1], y = us[,2], xend = us_tf[,1], yend = us_tf[,2]),
#'                color = "black", linetype = "dotted", size = 1, arrow = arrow(type = "closed", length = unit(0.03, "native"))) +
#'   coord_fixed()
#'
transform_by_matrix <- function(x, y = NULL, matrix = transform_matrix_affine("none")){
  if(is.null(y)) {
    df <- as.data.frame(x)
    x <- df[[1]]
    y <- df[[2]]
  }
  t(matrix %*% t(as.matrix(data.frame(x = x, y = y, 1))))[,c(1,2)]
}

#' Title
#'
#' @param x
#' @param y
#' @param fun
#' @param along
#' @param xrange.to
#' @param xrange.from
#' @param yrange.to
#' @param yrange.from
#'
#' @return
#' @export
#'
#' @examples
transform_by_fun <- function(
  x = NULL, y = NULL, fun, along = c("x", "y", "xy"),
  xrange.to = NULL, xrange.from = NULL,
  yrange.to = NULL, yrange.from = NULL){
  along <- match.arg(arg = along)

  xrange.to <- nullna_default(xrange.to, range(x, na.rm =TRUE, finite = TRUE))
  yrange.to <- nullna_default(yrange.to, range(y, na.rm =TRUE, finite = TRUE))
  xrange.from <- nullna_default(xrange.from, range(x, na.rm =TRUE, finite = TRUE))
  yrange.from <- nullna_default(yrange.from, range(y, na.rm =TRUE, finite = TRUE))

  x <- scales::rescale(x, xrange.to, xrange.from)
  y <- scales::rescale(y, yrange.to, yrange.from)

  switch(
    along,
    x = data.frame(x = x, y = y + fun(x)),
    y = data.frame(x = x + fun(y), y = y),
    xy = fun(x, y) # return data.frame of x,y
    )
}

#' Title
#'
#' @param x
#' @param n
#' @param unlist
#'
#' @return
#' @export
#'
#' @examples
coerce_x_len <- function(x, n, unlist = T){
  if(unlist) x <- unlist(x)
  if (length(x) < n) x <- rep(x, ceiling(n/length(x))) # extend if x length less than n
  return(x[1:n])
}



#' Title
#'
#' @param names
#' @param default
#' @param init
#'
#' @return
#' @export
#'
#' @examples
list_along <- function(names, default = NA, init = NA){
  x <- rep(list(init), length(names))
  names(x) <- names
  inter_names <- intersect(names(default), names)
  x[inter_names] <- default[inter_names]
  return(x)
}
