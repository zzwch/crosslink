#' Predefined layouts
#'
#' @param object a CrossLink object
#' @param layout_save layout name to save
#' @param angles angles in degree for hive, polygon and arc. set NA or NULL for equally divided.
#'
#' @return a updated CrossLink object
#' @importFrom magrittr %>%
#' @export
#' @details
#' \code{layout_column} is the default \code{initial} layout.
#'
#' \code{layout_row} and \code{layout_column} will always based on \code{initial} layout.
#'
#' @name Predefined_layouts
#'
#' @examples
#'
layout_column <- function(object, layout_save = "column"){
  object %>%
    cl_copy(to = layout_save, from = "initial", override = TRUE)
}

#' @export
#' @rdname Predefined_layouts
#'
layout_row <- function(object, layout_save = "row"){
  object %>%
    `cl_active<-`(value = "initial") %>%
    tf_rotate(x = 0.5, y = 0.5, angle = 90, layout = layout_save)
}

#' @export
#' @rdname Predefined_layouts
#'
layout_hive <- function(object, angles = NULL, layout_based = "initial", layout_save = "hive") {
  n_cross <- length(object@cross)
  if(n_cross < 3) stop("Hive layout is not recommented for crosses of length < 3!")

  crosses <- names(object@cross)
  angles <- cumsum(nullna_default(angles, c(0, coerce_x_len(360/n_cross, n = n_cross-1))))

  if(length(angles) != n_cross) stop("anlges length must be equal with length of crosses")

  cl_active(object) <- layout_based

  object %<>%
    tf_fun(fun = (function(x, y) {return(data.frame(x = 0, y = y))}),
           along = "xy",
           layout = layout_save)
  object %<>%
    tf_rotate(x = 0.5, y = 0, angle = angles, by.each.cross = T, relative = T,
              crosses = crosses, layout = layout_save)
  return(object)
}

#' @export
#' @rdname Predefined_layouts
#'
layout_polygon <- function(object, angles = NULL, layout_based = "initial",layout_save = "polygon"){
  n_cross <- length(object@cross)
  if(n_cross < 3) stop("polygon layout is not recommented for crosses of length < 3!")
  crosses <- names(object@cross)
  angles <- nullna_default(angles, c(0, coerce_x_len(360/n_cross, n = n_cross-1)))
  angles_cs <- cumsum(angles)
  if(length(angles_cs) != n_cross) stop("anlges length must be equal with length of crosses")

  cl_active(object) <- layout_based

  object %<>%
    tf_fun(fun = (function(x, y) {return(data.frame(x = 0, y = y))}),
           along = "xy",
           layout = layout_save)
  object %<>%
    tf_rotate(x = 0.5, y = 0, angle = angles_cs, by.each.cross = T, relative = T,
              crosses = crosses, layout = layout_save)

  coord <- cl_active(object, ret.data = T)
  nodes_top <- as.character(coord$node[coord$node.type == "top"])
  coord_top <- cl_coord(object, nodes_top, relative = F)

  angles[1] <- 360-sum(angles)
  object %<>%
    tf_rotate(x = coord_top$x[match(crosses, coord_top$cross)],
              y = coord_top$y[match(crosses, coord_top$cross)],
              angle = (180-angles)/2, by.each.cross = T, counterclockwise = F, relative = F,
              crosses = crosses, layout = layout_save)

  coord <- cl_active(object, ret.data = T)
  nodes_bottom <- as.character(coord$node[coord$node.type == "bottom"])
  coord_bottom <- cl_coord(object, nodes_bottom, relative = F)

  object %<>%
    tf_scale(x = coord_top$x, y = coord_top$y,
             #angle = angles_slope,
             scale = sqrt(2-2*cos(degree_to_radian(angles))),
             by.each.cross = T,
             relative = F,
             crosses = crosses, layout = layout_save)
  #angles_slope <- radian_to_degree(atan(abs((coord_bottom$y-coord_top$y)/(coord_bottom$x - coord_top$x))))
  return(object)
}

#' @export
#' @rdname Predefined_layouts
#'
layout_arc <- function(object, angles = 30, layout_based = "initial", layout_save = "arc") {
  n_cross <- length(object@cross)
  crosses <- names(object@cross)
  angles <- coerce_x_len(angles, n_cross)
  if(length(angles) != n_cross) stop("anlges length must be equal with length of crosses")

  coord <- object@layout[[layout_based]]
  cl_copy(object, to = layout_save, from = layout_based)

  for(i in seq(n_cross)){
    crs_ind <- (coord$cross == crosses[i]) & (coord$cross != "header")
    top_ind <- crs_ind & coord$node.type == "top"
    bottom_ind <- crs_ind & coord$node.type == "bottom"
    circle <- string_to_circle(
      coord[top_ind,"x"],
      coord[top_ind,"y"],
      coord[bottom_ind,"x"],
      coord[bottom_ind,"y"],
      radian = degree_to_radian(angles[i]))

    object %<>%
      tf_fun(fun = circle_projector,
             along = "xy",
             layout = layout_save,
             crosses = crosses[i],
             x0 = circle$left$center[1],
             y0 = circle$left$center[2],
             r = circle$radius,
             source = "x")
  }

  return(object)
}

#' @export
#' @rdname Predefined_layouts
#'
layout_custom <- function(object, angles = 30, layout_save = "arc") {

}
