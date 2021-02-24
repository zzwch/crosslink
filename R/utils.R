tolist_by_group <- function(x, group, drop = F) {
  xg <- split(x, group, drop = drop)
  if(drop) xg <- lapply(xg, droplevels)
  return(xg)
}
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

nullna_default <- function(x, default) {
  if(is.na(x) || is.null(x)) return(default) else return(x)
}

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

vector_to_coord <- function(x, scale = NA, range = c(0, 1)){ # scale = x+1  in length
  x <- factor(x)
  segment_range(
    range = range, n = length(levels(x)) + 1,
    scale = scale, return.cutter = T)[x]
}

index_to_name <- function(x, ind){
  if(is.numeric(ind)){
    if(max(ind) <= length(nodes))
      return(names(x)[ind])
    else
      stop("index is larger than length of x")
  }

  if(all(ind %in% names(nodes)))
    return(ind)
  else
    stop("index is not in names of x")
}


# 2D affine and perspective tranformation matrix
# https://en.wikipedia.org/wiki/Transformation_matrix
# https://www.cnblogs.com/bnuvincent/p/6691189.html
# Rconic::`Affine planar transformations matrix`
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
    shear = transform_matrix(b = tan(x), d = tan(-y)),
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

transform_matrix <- function(
  a = 1, b = 0, c = 0,
  d = 0, e = 1, f = 0,
  g = 0, h = 0, i = 1){
  matrix(c(a, b, c, d, e, f, g, h, i), nrow = 3, byrow = TRUE)
}


transform_by_matrix <- function(x, y = NULL, matrix = transform_matrix_affine("none")){
  if(is.null(y)) {
    df <- as.data.frame(x)
    x <- df[[1]]
    y <- df[[2]]
  }
  t(matrix %*% t(as.matrix(data.frame(x = x, y = y, 1))))[,c(1,2)]
}

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

coerce_x_len <- function(x, n, unlist = T){
  if(unlist) x <- unlist(x)
  if (length(x) < n) x <- rep(x, ceiling(n/length(x))) # extend if x length less than n
  return(x[1:n])
}



list_along <- function(names, default = NA, init = NA){
  x <- rep(list(init), length(names))
  names(x) <- names
  inter_names <- intersect(names(default), names)
  x[inter_names] <- default[inter_names]
  return(x)
}

# rbind(mapping_along(x = 1, y = 1:100, sin, along = "y", range = c(0, 4*pi)),
#       mapping_along(x = 1, y = 1:100, cos, along = "y", range = c(0+3*pi/4, 4*pi+3*pi/4))) %>% plot

# if(is.null(y)) {
#   df <- as.data.frame(x)
#   x <- df[[1]]
#   y <- df[[2]]
# }
