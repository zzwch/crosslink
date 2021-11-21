#' Affine Transformation
#'
#' Wrapper functions for transformations,
#' rotate: \code{tf_rotate}, flip: \code{tf_flip}, shift: \code{tf_shift} shear: \code{tf_shear}, scale: \code{tf_scale} and \code{tf_scale_xy}
#'
#' @param object a CrossLink object
#' @param type affine type
#' @param by.each.cross transformation to be performed by each cross
#' @param x,y coordiantes of transformation center for `tf_rotate`;
#' offset in x,y axies for `tf_shift`;
#' scale about this position for `tf_scale`;
#' scale `x` and `y` along x, y axies, respectively, for `tf_scale_xy`
#' @param angle transformation angle in degree
#' @param scale.x,scale.y scale size for type \code{scale}
#' @param counterclockwise angle in counterclockwise for transformation
#' @param relative x,y is relative coordinates in range c(0,1)
#' @param crosses only these crosses will be transformed
#' @param layout layout name to save
#' @param axis along which axis to flip or shear
#' @param ... other parameters passed to tf_affine
#'
#' @details
#' ## learn more about 2D affine and perspective tranformation matrix:
#' - https://en.wikipedia.org/wiki/Transformation_matrix
#' - https://www.cnblogs.com/bnuvincent/p/6691189.html
#' - \code{Rconic::`Affine planar transformations matrix`}
#'
#' @md
#' @return an updated CrossLink object
#' @export
#' @name tf_affine
#'
#' @examples
#'
tf_affine <- function(
  object,
  type = c("none", "translate", "scale", "rotate", "shear", "reflect"),
  by.each.cross = F,
  x = 0, y = 0, angle = NA, scale.x = NA, scale.y = NA,
  counterclockwise = FALSE,
  relative = T,
  crosses = NULL, # tf these crosses (names)
  layout = "transforming"
) {
  active <- object@active
  coord <- object@layout[[active]]

  type <- match.arg(arg = type, several.ok = FALSE)
  theta <- pi*angle/180

  crosses <- nullna_default(crosses, names(object@cross))

  if(by.each.cross){
    n_cross <- length(crosses)
    x <- coerce_x_len(x, n_cross)
    y <- coerce_x_len(y, n_cross)
    scale.x <- coerce_x_len(scale.x, n_cross)
    scale.y <- coerce_x_len(scale.y, n_cross)

    if(relative){
      xy <- cl_rel2abs(object, x, y, crosses, by.canvas = F)
      x <- xy[,1]
      y <- xy[,2]
    }

    theta <- coerce_x_len(theta, n_cross)
    counterclockwise <- coerce_x_len(counterclockwise, n_cross)

    for (i in seq_len(n_cross)) {
      ind <- which(coord$cross == crosses[[i]])
      ind_woh <- which(coord$cross == crosses[[i]] & coord$node.type != "header")
      coord_i <- coord[ind, c("x", "y"), drop = F]
      #coord_woh <- coord[ind_woh, c("x", "y"), drop = F]
      # cross_coord
      coord[ind, c("x", "y")] <- transform_by_matrix(
        coord_i,
        matrix = transform_matrix_affine(
          type = type,
          x = x[[i]], y = y[[i]], theta = theta[[i]],
          scale.x = scale.x[[i]], scale.y = scale.y[[i]],
          counterclockwise = counterclockwise[[i]]))
    }
  }else{

    if(relative){
      xy <- cl_rel2abs(object, x, y, crosses, by.canvas = T)
      x <- xy[,1]
      y <- xy[,2]
    }

    ind <- which(coord$cross %in% crosses)

    coord[ind, c("x", "y")] <- transform_by_matrix(
      coord[ind, c("x", "y")],
      matrix = transform_matrix_affine(
        type = type,
        x = x[[1]], y = y[[1]], theta = theta[[1]],
        scale.x = scale.x[[1]], scale.y = scale.y[[1]],
        counterclockwise = counterclockwise[[1]]))

  }

  object@layout[[layout]] <- coord
  cl_active(object) <- layout

  return(object)
}

#' @export
#' @rdname tf_affine
#'
tf_rotate <- function(
  object, x = 0, y = 0, angle = 90,
  ...) {

  tf_affine(
    object, type = c("rotate"), x = x, y = y, angle = angle,
    ...)
}

#'
#' @export
#' @rdname tf_affine
#'

tf_flip <- function(
  object, axis = c("x", "y"),
  ...) {

  axis <- match.arg(axis)
  x <- 0
  y <- 0
  angle <- switch(axis, x = 0, y = 90)
  tf_affine(
    object, type = c("reflect"), x = x, y = y, angle = angle,
    counterclockwise = T,
    ...)
}

#'
#' @export
#' @rdname tf_affine
#'
tf_shift <- function(
  object, x = 0, y = 0,
  ...) {

  tf_affine(
    object, type = c("translate"), x = x, y = y, angle = NA,
    ...)
}

#' @export
#' @rdname tf_affine
#'
#' @examples
#'
tf_shear <- function(
  object, axis = c("x", "y"), angle = 15, # no more than 90

  ...) {

  axis <- match.arg(axis)
  x <- pi*angle/180
  y <- pi*angle/180
  if(axis == "x") y <- 0 else x <- 0

  tf_affine(
    object, type = c("shear"),x = x, y = y, angle = NA,
    ...)
}

#' @export
#' @rdname tf_affine
#'
tf_scale <- function(
  object, x = 0, y = 0, scale.x = 1, scale.y = 1,
  ...) {

  tf_affine(
    object, type = c("scale"), x = x, y = y, scale.x = scale.x, scale.y = scale.y,
    ...)

}


#' Transformation by a given function
#'
#' @param object a CrossLink object
#' @param fun function for coordinate transformation; return data.frame if along is 'xy', otherwise return vector.
#' @param along another axis will changed along this given axis. Such as, if along = "x", transformation function will change y values along x axis and keep x values asis.
#' @param layout layout name to save
#' @param xrange.from,yrange.from the range to be scaled from before transformation by function
#' @param xrange.to,yrange.to the range to be scaled to
#' @param crosses only these crosses will be transformed
#' @param ... other parameters passed to \code{fun}
#'
#' @return an updated CrossLink object
#' @export
#'
#' @examples
#'
tf_fun <- function(
  object, fun, along = c("x", "y", "xy"),
  layout = "transforming",
  xrange.from = NULL,
  yrange.from = NULL,
  xrange.to = NULL,
  yrange.to = NULL,
  crosses = NULL, # tf these crosses (names)
  ...
  ) {

  active <- cl_active(object)
  x <- object@layout[[active]][["x"]]
  y <- object@layout[[active]][["y"]]

  crosses <- nullna_default(crosses, names(object@cross))
  ind_crosses <- which(object@layout[[active]][['cross']] %in% crosses)

  xy <- transform_by_fun(
    x = x[ind_crosses], y = y[ind_crosses], fun = fun, along = along,
    xrange.to = xrange.to, xrange.from = xrange.from,
    yrange.to = yrange.to, yrange.from = yrange.from, ...)
  # header
  # header <- object@canvas[[active]]$header
  # if(!identical(header, NA)){
  #   ind_header <- header$cross %in% crosses
  #   header[ind_header, c("x", "y")] <- transform_by_fun(
  #     header$x[ind_header],header$y[ind_header],
  #     fun = fun, along = along,
  #     xrange.to = c(0,1), xrange.from = c(0,1),
  #     yrange.to = c(0,1), yrange.from = c(0,1))# range(0, 1)
  # }

  object@layout[[layout]] <- object@layout[[active]]
  object@layout[[layout]][ind_crosses,"x"] <- xy[["x"]]
  object@layout[[layout]][ind_crosses,"y"] <- xy[["y"]]

  # canvas
  # object@canvas[[layout]] <- object@canvas[[active]]
  # header
  # object@canvas[[layout]]$header <- header
  # range
  # xyrange <- expand.grid(object@canvas[[active]][["xrange"]],
  #                        object@canvas[[active]][["yrange"]])
  # xyrange <- transform_by_fun(x = xyrange[,1], y = xyrange[,2], fun = fun,  along = along,
  #                             xrange.to = xrange.to, xrange.from = xrange.from,
  #                             yrange.to = yrange.to, yrange.from = yrange.from)
  # cl_xrange(object, layout = layout) <- range(xyrange[,1])
  # cl_yrange(object, layout = layout) <- range(xyrange[,2])

  cl_active(object) <- layout
  return(object)
}


#' generate a 3*3 matrix for affine transformation
#'
#' @param type a affine transformation type
#' @param x transform value along x axis
#' @param y transform value along y axis
#' @param theta transform value of angle in radian, usually in 0 to 2pi
#' @param counterclockwise control directions of theta or x,y if type is "shear"
#' @param scale.x,scale.y scale size, only used for \code{scale} type
#'
#' @return a 3*3 matrix
#' @md
#' @description
#' ## learn more about 2D affine and perspective tranformation matrix:
#' - https://en.wikipedia.org/wiki/Transformation_matrix
#' - https://www.cnblogs.com/bnuvincent/p/6691189.html
#' - \code{Rconic::`Affine planar transformations matrix`}
#'
#' ## Used arguments for different types:
#' - none: ignore everything
#' - translate: x, y, counterclockwise
#' - scale: x, y, scale.x, scale.y
#' - scale_xy: x, y
#' - rotate: x, y, theta (angle of the rotation), counterclockwise
#' - shear: x, y, counterclockwise
#' - reflect: x, y, theta (angle from x axis), counterclockwise
#'
#' @keywords internal
#' @export
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
  x = 0, y = 0, theta = NA, scale.x = 1, scale.y = 1, counterclockwise = FALSE){

  if(counterclockwise && type != "scale") theta <- -theta
  switch(
    match.arg(arg = type, several.ok = FALSE),
    none = transform_matrix(),
    translate = transform_matrix(c = x, f = y),
    scale = transform_matrix(a = scale.x,
                             c = x-x*scale.x,
                             e = scale.y,
                             f = y-y*scale.y),
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
#' @keywords internal
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


#' Transform 2D coordinates by a 3*3 matrix
#'
#' @param x a vector of coordinates at x axis or a data.frame if y is NULL
#' @param y a vector of coordinates at y axis or NULL
#' @param matrix a 3*3 affine transformation matrix
#'
#' @return a matrix of transformed coordinates
#' @keywords internal
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

#' Transform 2D coordinates by a function(x, y)
#'
#' @param x a vector of coordinates at x axis
#' @param y a vector of coordinates at y axis
#' @param fun a function taking \code{x} and/or \code{y} as arguments
#' @param along th independent variables of \code{fun}
#' @param xrange.from range of \code{x}, set NULL to use \code{range(x)}
#' @param yrange.from same with \code{xrange.from}, but for y axis
#' @param xrange.to before transforming, rescale \code{x} from \code{xrange.from} into the range of \code{xrange.to}, set NULL to use \code{range(x)}
#' @param yrange.to same with \code{xrange.to}, but for y axis
#' @param ... other parameters passed to \code{fun}
#'
#' @return a matrix of transformed coordinates
#' @keywords internal
#' @export
#'
#' @examples
#' xy <- data.frame(x = 1,
#'                  y = 1:100)
#' xy_tf <- transform_by_fun(xy$x, xy$y, function(x) 0.1*sin(x), along = "y", yrange.to = c(0, 2*pi))
#' plot(xy)
#' plot(xy_tf, type = "o")
#'
transform_by_fun <- function(
  x = NULL, y = NULL, fun, along = c("x", "y", "xy"),
  xrange.to = NULL, xrange.from = NULL,
  yrange.to = NULL, yrange.from = NULL,
  ...){
  along <- match.arg(arg = along)

  xrange.to <- nullna_default(xrange.to, range(x, na.rm =TRUE, finite = TRUE))
  yrange.to <- nullna_default(yrange.to, range(y, na.rm =TRUE, finite = TRUE))
  xrange.from <- nullna_default(xrange.from, range(x, na.rm =TRUE, finite = TRUE))
  yrange.from <- nullna_default(yrange.from, range(y, na.rm =TRUE, finite = TRUE))

  x <- scales::rescale(x, xrange.to, xrange.from)
  y <- scales::rescale(y, yrange.to, yrange.from)

  switch(
    along,
    x = data.frame(x = x, y = y + fun(x, ...)),
    y = data.frame(x = x + fun(y, ...), y = y),
    xy = fun(x, y, ...) # return data.frame of x,y
  )
}
