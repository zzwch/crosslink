#' @rdname cl_xrange
#' @param value a vector of length 2 defining a range for X or Y axis
#'
#' @return an updated CrossLink object
#' @export
#'
#' @examples
#'
`cl_xrange<-` <- function(object, layout = NULL, value){
  if(length(value) != 2) stop("value must be of length 2.")
  object@canvas[[nullna_default(layout, cl_active(object))]][["xrange"]] <- value
  return(object)
}

#' @rdname cl_xrange
#' @export
`cl_yrange<-` <- function(object, layout = NULL, value){
  if(length(value) != 2) stop("value must be of length 2.")
  object@canvas[[nullna_default(layout, cl_active(object))]][["yrange"]] <- value
  return(object)
}


#' Change gaps between adjacent crosses
#'
#' This function is only valid for the initial layout. Change flanks and gaps between adjacent crosses
#'
#' @inheritParams cl_layouts
#' @param gaps a numeric vector of n+1 length, n is equal to length of crosses (that is \code{length(object@crosses)}).
#'  The gaps vector is c(left flank, gaps between adjacent crosses, right flank).
#'  set NA to use equally divided gaps, or set NULL to do nothing.
#'
#' @return a updated CrossLink object
#' @export
#'
#' @examples
#'
set_gap <- function(object, gaps = NA, layout_save = "initial"){
  if(is.null(gaps)) return(object) # do nothing

  layout <- object@layout[["initial"]]
  cross <- object@cross
  # levels from up to down
  cross_anchored <- lapply(names(cross), function(x){
    x_nodes <- c(paste0(x, "_BOTTOM"),
                 rev(levels(factor(cross[[x]]))),
                 paste0(x, "_TOP"))
    factor(x = x_nodes,
           levels = x_nodes)
  })

  layout$x <- vector_to_coord(
    x = factor(rep(names(cross_anchored),
                   times = sapply(cross_anchored, length)),
               levels = names(cross_anchored)),
    scale = gaps,
    range = cl_xrange(object, layout = "initial"))

  object@layout[[layout_save]] <- layout
  cl_active(object) <- layout_save
  return(object)
}

#' Change spaces between adjacent nodes
#'
#' This function is only valid for the initial layout.
#' Change flanks and spaces between adjacent nodes for each cross,
#' The change is applied for each cross, seperately.
#'
#' @inheritParams cl_layouts
#' @param spaces a named list of numeric vectors. The names must be names of crosses to be changed.
#' And each vector is of n+1 length, n is equal to length of nodes in their belonging cross.
#' The vector is c(top flank, spaces between adjacent nodes, bottom flank).
#' set NA to use equally divided spaces, or set NULL to do nothing.
#' @param layout_save layout name to save
#' @return
#' @export
#'
#' @examples
#'
set_space <- function(object, spaces = NA, layout_save = "initial"){
  if(is.null(spaces)) return(object) # do nothing

  layout <- object@layout[["initial"]]
  cross <- object@cross

  layout$y <- do.call(
    c, mapply(FUN = vector_to_coord,
              x = cross,
              scale = list_along(names(cross),
                                 some = spaces),
              range = list_along(names(cross),
                                 default = cl_yrange(object, layout = "initial")))
  )

  object@layout[[layout_save]] <- layout
  cl_active(object) <- layout_save
  return(object)
}


