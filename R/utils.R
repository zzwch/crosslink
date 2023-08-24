#' convert a vector into a list
#'
#' convert x (a vector) into a list according to the group information (another vector) of elements in x.
#'
#' @param x a vector
#' @param group a vector with equal length of x, providing group information of elements in x
#' @param drop drop levels of x
#'
#' @return a list. Order of elements is controled by levels of group.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#' @export
#'
#' @examples
#' nullna_default(NULL, c(1,2))
#'
nullna_default <- function(x, default) {
  if(any(is.na(x)) || any(is.null(x))) return(default) else return(x)
}

#' cut a segment defined by a range in 1D coordinate into n pieces
#'
#' @param range a vector of 2 numeric values defining a segment
#' @param n cut the segment into \code{n} pieces
#' @param scale a vector of \code{n} numeric values to control segment sizes relatively. Set NA or NULL for equally dividing.
#' @param return.cutter only return the coordinate values of internal cutters
#'
#' @return a vector of n+1 or n-1 if \code{return.cutter}
#' @keywords internal
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
#' @param scale a vector of \code{length(x) - 1} numeric values to control adjacent spacing. Set NA or NULL for equally dividing.
#' @param range a vector of 2 numeric values defining a interval, where \code{x} will be mapped.
#'
#' @return a vector of length(x)
#' @keywords internal
#' @export
#'
#' @examples
#' vector_to_coord(1:4, scale = c(2,2,2), range(1, 9))
#'
vector_to_coord <- function(x, scale = NA, range = c(0, 1)){ # scale = x-1  in length
  x <- factor(x)
  segment_range(
    range = range, n = length(levels(x)) - 1,
    scale = scale, return.cutter = F)[x]
}

#' a helper function to get names by integer indexes from data.frame or list
#'
#' @param x a data.frame or a list
#' @param ind a vector of integer indexes or colnames of \code{x}
#'
#' @return a vector of characters
#' @keywords internal
#' @export
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
    stop("index is not all in names of x")
}



#' use a vector repeatedly to generate a new vector of the specified length
#'
#' @param x a vector to be used repeatedly
#' @param n length of the new generated vector
#' @param unlist unlist \code{x} before repeatedly being used
#'
#' @return a vector of \code{n} length
#' @keywords internal
#' @export
#'
#' @examples
#' coerce_x_len(1, 10)
#'
coerce_x_len <- function(x, n, unlist = T){
  if(unlist) x <- unlist(x)
  if (length(x) < n) x <- rep(x, ceiling(n/length(x))) # extend if x length less than n
  return(x[1:n])
}



#' Create a list along given names and default values
#'
#' It can be useful to create an list with default values and some settable values. This is similar to the idea of seq_along(), which creates a vector of the same length as its input.
#'
#' @param names names of the list to be generated
#' @param default default value for each element of the generated list
#' @param some a list with elements whose names should be included in \code{names}, use this to set default values for some elements of the generated list
#'
#' @return a list
#' @export
#'
#' @examples
#' list_along(names = c("A", "B", "C"), default = NA, some = list(B = 1))
#'
list_along <- function(names, default = NA, some = NA){
  x <- rep(list(default), length(names))
  names(x) <- names
  inter_names <- intersect(names(some), names)
  x[inter_names] <- some[inter_names]
  return(x)
}


#' Merge two list
#'
#' @param x one list
#' @param y another list. Elements having same names in \code{x} will be override by those in \code{y}.
#'
#' @return a merged list
#' @keywords internal
#' @export
#'
#' @examples
#' merge_list(list(1, A = 2, B = 3, 4), list(A = 22, 5, 1))
merge_list <- function(x, y){
  x <- c(y, x)
  if (is.null(names(x)))
    return(x)
  else
    return(x[!duplicated(names(x)) | names(x) == ""])
}

#' generate a toy CrossLink object
#'
#' @param n_cross the number of crosses (columns)
#' @param n_link a vector of numbers of links between crosses
#' @param n_node a vector of numbers of nodes in each cross
#' @param seed random seed for random links
#'
#' @return a CrossLink object
#' @export
#'
#' @examples
#'
gen_demo <- function(n_cross = 2, n_link = c(10), n_node = c(3,4), seed = 6){
  nodes <- data.frame(
    key = paste0(rep(LETTERS[seq_len(n_cross)], n_node),
                 unlist(sapply(n_node, seq_len))),
    type = rep(LETTERS[seq_len(n_cross)], n_node))
  crosses <- tolist_by_group(nodes$key, nodes$type)

  edges <- NULL
  set.seed(seed)
  for(i in seq_along(n_link)){
    edges = rbind(edges, data.frame(
      src = sample(crosses[[i]], n_link[i], replace = T),
      tar = sample(crosses[[i + 1]], n_link[i], replace = T)
    ))
  }
  return(crosslink(nodes = nodes, edges = edges, cross.by = "type"))
}


#' Override default aes with new aes
#'
#' @param aes input aes
#' @param default_aes aes with some default values
#'
#' @return a updated aes
#' @import rlang
#' @keywords internal
#' @export
#'
#' @examples
#'
override_aes <- function(mapping = NULL, default_aes){
  x <- merge_list(lapply(default_aes, rlang::quo_get_expr),
                         lapply(mapping, rlang::quo_get_expr))

  new_aesthetic <- function (x, env = globalenv()) {
    if (is_quosure(x)) {
      if (!quo_is_symbolic(x)) {
        x <- quo_get_expr(x)
      }
      return(x)
    }
    if (is_symbolic(x)) {
      x <- new_quosure(x, env = env)
      return(x)
    }
    x
  }
  x <- lapply(x, new_aesthetic, env = parent.frame())
  structure(x, class = "uneval")
}

#' convert degree to radian
#'
#' @param degree angle in degree
#'
#' @return angle in radian
#' @keywords internal
#' @export
#'
#' @examples
#' degree_to_radian(180)
#'
degree_to_radian <- function(degree) {
  pi*degree/180
}

#' convert radian to degree
#'
#' @param radian angle in radian
#'
#' @return angle in degree
#' @keywords internal
#' @export
#'
#' @examples
#' degree_to_radian(pi)
#'
radian_to_degree <- function(radian) {
  180*radian/pi
}

#' get values by interval inclusion
#'
#' @param x a vector of values
#' @param interval the range of inclusion to be appied
#' @param inclusion defining inclusion
#'
#' @return a vector
#' @keywords internal
#' @export
#'
#' @examples
#' x <- seq(0, 1, length.out = 9)
#' interval_inclusion(x, interval = c(0.5, 1), inclusion = "left")
#' interval_inclusion(x, interval = c(0.5, 1), inclusion = "right")
#' interval_inclusion(x, interval = c(0.5, 1), inclusion = "both")
#' interval_inclusion(x, interval = c(0.5, 1), inclusion = "neither")
#'
interval_inclusion <- function(x, interval = NULL, inclusion = c("left", "both", "right","neither")){
  inclusion <- match.arg(inclusion)
  left_inc <- get(">")
  right_inc <- get("<")
  if(inclusion %in% c("both", "left")) left_inc <- get(">=")
  if(inclusion %in% c("both", "right")) right_inc <- get("<=")

  if(is.null(interval)) interval <- range(x)
  x[left_inc(x, interval[1]) & right_inc(x, interval[2])]
}

#' get period variables with same return value
#'
#' @param x a vector of variables in one period
#' @param period the period
#' @param range the range in which variables (usually with the same values with x) will be returned
#' @param range.inclusion left or right inclusion for \code{range}
#' @param simplify return list or not, see \code{\link{sapply}}
#'
#' @return a vector
#' @keywords internal
#' @export
#'
#' @examples
#' period_variable(pi, 2*pi, c(-pi, 5*pi))
#'
period_variable <- function(x, period, range = c(0, period), range.inclusion = c("left", "both","right", "neither"), simplify = F){
  x <- unique(x - period*floor(x/period)) # 0 to period range
  inc <- match.arg(range.inclusion)
  sapply(x, function(y) {
    y <- y + period * floor(range[1]/period):ceiling(range[2]/period)
    interval_inclusion(y, interval = range, inclusion = inc)
  }, simplify = simplify)
}


#' get arc-trigonometric values in periods
#'
#' @param x trigonometric values
#' @param y used for atan2 function. see \code{\link{atan2}}
#' @param arc.fun arc-trigonometric function
#' @param return.range set NULL for a range of 0 to 2*pi.
#'
#' @return angles in radian
#' @keywords internal
#' @export
#'
#' @examples
#'

period_arcTrig <- function(x, y = NULL, arc.fun = c("acos", "asin", "atan", "atan2"), return.range = NULL){
  arc.fun <- match.arg(arc.fun)
  if(is.null(return.range)) return.range <- c(0, 2*pi)

  radian <- switch(
    arc.fun,
    acos = c(acos(x), 2*pi - acos(x)), # 0 to pi
    asin = c(asin(x), pi - asin(x)), # -pi/2 to pi/2
    atan = atan(x), # -pi/2 to pi/2
    atan2 = atan2(y, x)) # 0 to pi
  period <- switch(
    arc.fun,
    acos = 2*pi, # 0 to pi
    asin = 2*pi, # -pi/2 to pi/2
    atan = pi, # -pi/2 to pi/2
    atan2 = pi) # 0 to pi

  period_variable(unique(radian), period = period, range = return.range, range.inclusion = "left", simplify = T)
}

#' Get its center, radius and angle from a circle's string
#'
#' @param x,y,xend,yend start and end points defining a string of a circle
#' @param radian angle of the string.
#' @param radius radius of the circle. Only used if \code{radian} is NULL or 2*pi.
#'
#' @return a list of center, radius, string.start, string.end, angle, angle.start, angle.end
#' @details For a string, given its radian or radius, there will be two circle
#' centers matched. This function will return both centers in a order of left
#' center and right center. For brevity, the radian will always less than pi (0-2*pi).
#'
#' @keywords internal
#' @export
#'
#' @examples
#' string_to_circle(-1, 0, 1, 0, pi)
#' string_to_circle(-1, 0, 1, 0, pi/2)
#' string_to_circle(0, 1, 0, -1, pi/2)
#'
string_to_circle <- function(x, y, xend, yend, radian = NULL, radius = NULL) {
  d <- sqrt((xend - x)^2 + (yend - y)^2)
  if(is.null(radian) || is.na(radian)){
    if(is.null(radius) || is.na(radius)) stop("Either radian or radius should be setted!")
    if(radius < d/2) {
      warning("radius is too small and is coerced to string/2!")
      radius <- d/2
    }
    radian <- nullna_default(radian, acos(1-d^2/radius^2/2)) # 0-pi
  }else{
    if(radian >= 2*pi && is.null(radius)) stop("Please also set radius!")
    if(radian < 2*pi) radius <- d/sqrt(2*(1-cos(radian)))
  }

  l <- sqrt(radius^2 - (d/2)^2)
  xc <- (x+xend)/2
  yc <- (y+yend)/2
  theta <- atan(-(xend-x)/(yend-y))

  # left center
  x1 <- xc-cos(theta)*l
  y1 <- yc-sin(theta)*l

  # right center
  x2 <- xc+cos(theta)*l
  y2 <- yc+sin(theta)*l

  return(
    list(radius = radius,
         string.start = c(x, y),
         string.start = c(xend, yend),
         angle = ifelse(radian > pi, 2*pi-radian, radian),
         left = list(center = c(x1, y1),
                     angle.start = atan2(y-y1, x-x1),
                     angle.end = atan2(yend-y1, xend-x1)),
         right = list(center = c(x2, y2),
                      angle.start = atan2(y-y2, x-x2),
                      angle.end = atan2(yend-y2, xend-x2)))
  )
}


#' project points to a circle
#'
#' @param x,y points to be projected
#' @param x0,y0 circle center
#' @param r radius of circle
#' @param source projection type.
#'
#' @details
#' Here, for sources of x or y, projected points will be at the right or up half circle;
#' for sources of -x or -y, projected points will be at the left or down half circle
#'
#' @return data.frame of coordinates of projected points
#' @keywords internal
#' @export
#'
#' @examples
#'
circle_projector <- function(x, y, x0 = 0, y0 = 0, r = 1, source = c("x", "y", "-x", "-y", "center")){
  source <- match.arg(source)
  if(source == "center"){
    theta <- atan2(y-y0, x-x0)
    return(
      data.frame(x = x0 + r*cos(theta),
               y = y0 + r*sin(theta))
    )
  }
  if(source == "x"){
    return(
      data.frame(
      x = x0+r*cos(asin((y-y0)/r)),
      y = y)
    )
  }
  if(source == "y"){
    return(
      data.frame(
      x = x,
      y = y0+r*sin(acos((x-x0)/r)))
    )
  }
  if(source == "-x"){
    return(
      data.frame(
      x = x0-r*cos(asin((y-y0)/r)),
      y = y)
    )
  }
  if(source == "-y"){
    return(
      data.frame(
      x = x,
      y = y0-r*sin(acos((x-x0)/r)))
    )
  }

}


