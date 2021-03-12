#' The CrossLink class
#'
#' This Class is designed to manipulate coordinates of nodes with groups and to
#' visualize the crosslink network.
#'
#' Nodes and Edges formed a network, and in the so called crosslink network,
#' Nodes in the same type will be placed in one column (named as a cross), and
#' Edges is the links linking between crosses.
#'
#' @slot nodes data.frame for node data
#' @slot edges data.frame for edge data
#' @slot cross a list, saving nodes in each cross, initialized by \code{\link{crosslink}}
#' @slot layout a list, saving different layout of coordinates of nodes in crosses.
#' @slot active character, Name of the active, or default, layout; settable using \code{\link{cl_active}}
#' @slot params list.
#'
#' @details
#' cross: list names are inherited from cross.by in nodes; names' order (to control it using factor) is used as column order.
#'
#' layout: list names are layout names. For each layout, a data.frame is stored
#' with node (key), node.type (node, top, bottom or header), x, y (coordinates), cross (which cross it belongs to).
#'
#' canvas: deprecated
#'
#' active: a layout name which is being used currently
#'
#' params: other params used for internal functions
#'
CrossLink <- setClass(
  # http://adv-r.had.co.nz/OO-essentials.html
  "CrossLink",
  slots = c( # slots should be: logical TOC, layer by layer, minimize layers' depth (<= 3)
    nodes = "data.frame",
    edges = "data.frame",
    cross = "list",
    layout = "list", #
    active = "character",
    params = "list"
  ))

#' Generator of class CrossLink
#'
#' Achieve a initialized CrossLink object by providing nodes, edges and cross.by.
#'
#' @param nodes a data frame with unique keys in the 1st (settable by \code{key.by}) column and metadata (used for aesthetic of nodes) in the other columns
#' @param edges a data frame with source and target nodes in the first two (settable by \code{src.by} and \code{tar.by}) columns and other metadata.
#' @param cross.by name of the column in \code{nodes} to be used to group nodes in different crosses. The order of crosses and nodes could be controlled by setting levels for corresponding columns in \code{nodes} using \code{\link{factor}} or \code{\link{forcats::fct_relevel}}
#' @param gaps a numeric vector of n-1 length, n is equal to length of crosses (that is \code{length(object@crosses)}).
#'  The gaps vector is c(gaps between adjacent crosses).
#'  set NA to use equally divided gaps.
#' @param spaces a named list of numeric vectors. The names must be names of crosses to be changed.
#' And each vector is of n+1 length, n is equal to length of nodes in their belonging cross.
#' The vector is c(top flank, spaces between adjacent nodes, bottom flank).
#' set NA to use equally divided spaces.
#' @param xrange,yrange canvas range
#' @param key.by name or index of the column in \code{nodes} to be used as key of nodes
#' @param src.by name or index of the column in \code{edges} to be used as source of links (edges)
#' @param tar.by name or index of the column in \code{edges} to be used as target of links (edges)
#' @param odd.rm remove odd nodes that are not included in edges (without links).
#'
#' @return a CrossLink object
#' @export
#'
#' @examples
#'
crosslink <- function(
  nodes, edges, cross.by, gaps = NA, spaces = NA, xrange = c(0, 10), yrange = c(0, 10),
  key.by = 1, src.by = 1, tar.by = 2,
  odd.rm = FALSE) {
  # I try to keep one function clean and simple, which should be friendly.
  # I prefere using %>% to pipe functions, and this way should be more intuitive and easy to use.

  # coerce to data.frame
  nodes <- as.data.frame(nodes)
  edges <- as.data.frame(edges)

  # default key index
  key.by <- index_to_name(x = nodes, ind = key.by)
  src.by <- index_to_name(x = edges, ind = src.by)
  tar.by <- index_to_name(x = edges, ind = tar.by)
  cross.by <- index_to_name(x = nodes, ind = cross.by)

  # clean nodes and edges
  nodes <- nodes[!is.na(nodes[[cross.by]]),] # remove nodes not included in crosses
  edges <- edges[((edges[[src.by]] %in% nodes[[key.by]]) & (edges[[tar.by]] %in% nodes[[key.by]])),]

  if(odd.rm) nodes <- nodes[(nodes[[key.by]] %in% edges[[src.by]]) | (nodes[[key.by]] %in% edges[[tar.by]]),]
  # c(fct_x, fct_y) will coerce fct_x, and fct_y to numeric

  node_key <- nodes[[key.by]]
  node_crs <- nodes[[cross.by]]

  # check nodes uniqueness
  if(anyDuplicated(node_key) != 0) stop("keys of nodes must be unique!")

  # cross
  cross <- tolist_by_group(factor(node_key), node_crs, drop = T) # factor(x) will drop unused levels

  # range
  # xrange <- c(0, length(cross)+1)
  # yrange <- c(0, do.call(max, lapply(cross, length))+1)

  # levels from up to down
  cross_anchored <- sapply(names(cross), function(x){
    x_nodes <- c(paste0(x, "_BOTTOM"),
                 rev(levels(factor(cross[[x]]))),
                 paste0(x, "_TOP"))
    factor(x = x_nodes,
           levels = x_nodes)
    }, simplify = F, USE.NAMES = T)

  # layout
  layout <- list(
    initial = data.frame(
        node = unlist(cross_anchored),
        node.type = unlist(lapply(cross_anchored, function(x) {c("bottom", rep("node", length(x)-2), "top")})),
        x = vector_to_coord(x = factor(rep(names(cross_anchored),
                                           times = sapply(cross_anchored, length)),
                                       levels = levels(factor(node_crs))),
                            scale = gaps,
                            range = xrange),
        y = do.call(c, mapply(FUN = vector_to_coord,
                              x = cross_anchored,
                              scale = list_along(names(cross_anchored), default = NA, some = spaces),
                              range = list_along(names(cross_anchored), default = yrange),
                              SIMPLIFY = F)),
        cross = rep(names(cross_anchored),
                    times = sapply(cross_anchored, length)))
    )

  # params
  params <- list(
    key.by = key.by,
    src.by = src.by,
    tar.by = tar.by,
    cross.by = cross.by
  )

  return(
    new("CrossLink",
        nodes = nodes,
        edges = edges,
        cross = cross,
        layout = layout,
        active = "initial",
        params = params)
    )
}




#' Print the CrossLink object
#'
#' @param object a CrossLink object
#'
#' @return No value is returned.
#' @keywords internal
#'
#' @examples
#'
setMethod(
  f = "show",
  signature = "CrossLink",
  definition = function(object) {
    cat("An object of class CroosLink\n")
    n_node <- nrow(object@nodes)
    n_edge <- nrow(object@edges)
    n_cross <- length(object@cross)
    cat(n_edge, "edges between", n_node, "nodes within", n_cross, "crosses\n")
    cat("Layouts:", names(object@layout), "\n")
    cat("Active:", object@active,"\n")
    }
  )




#' Retrieve available layout names
#'
#' @param object a CrossLink object
#' @return names of available layouts
#' @export
#'
#' @examples
#'
cl_layouts <- function(object){
  names(object@layout)
}

#' Retrieve and Set active layout
#'
#' @name cl_active
#'
#' @param object a CrossLink object
#' @param ret.data return layout data
#'
#' @return name or data of the active layout
#' @export
#'
#' @examples
#'
cl_active <- function(object, ret.data = F) {
  if(ret.data)
    object@layout[[object@active]]
  else
    object@active
}

#' @param value the name of a layout to be set as active
#' @return an updated CrossLink object
#' @export
#' @rdname cl_active
#'
#' @examples
#'
`cl_active<-` <- function(object, value) {
  if(length(value) != 1) stop("value must be of length 1.")
  if(value %in% names(object@layout)){
    object@active <- value
    return(object)
  }else{
    stop(value, " is not in layout")
  }
}


#' Retrieve ranges of X or Y axis
#'
#' @param object a CrossLink object
#' @param layout name of the layout to be retrived or set
#' @param all also return ranges for each cross. The range does not include header.
#'
#' @return range of X or Y axis for the given layout
#' @export
#' @name cl_xrange
#'
#' @examples
#'
cl_xrange <- function(object, layout = NULL, all = F){
  coord <- object@layout[[nullna_default(layout, cl_active(object))]]
  coord <- coord[coord$node.type != "header", ,drop = F]
  if(!all) return(range(coord[["x"]]))
  else{
    c(list(canvas = range(coord[["x"]])),
            sapply(names(object@cross),
                   function(crs) {
                     range(coord[coord$cross == crs, "x"])
                   },
                   simplify = F, USE.NAMES = T))
    }
}



#' @rdname cl_xrange
#' @export
cl_yrange <- function(object, layout = NULL, all = F){
  coord <- object@layout[[nullna_default(layout, cl_active(object))]]
  coord <- coord[coord$node.type != "header", ,drop = F]
  if(!all) return(range(coord[["y"]]))
  else{
    c(list(canvas = range(coord[["y"]])),
      sapply(names(object@cross),
             function(crs) {
               range(coord[coord$cross == crs, "y"])
             },
             simplify = F, USE.NAMES = T))
  }
}


#' Set Headers for each cross
#'
#' @inheritParams cl_xrange
#' @param header a vector of titles for all crosses. Its length should be equal with the length of crosses (\code{object@cross}). Set NULL for default using \code{names(object@cross)} and set NA to remove headers.
#' @param hjust,vjust a vector for setting x or y position for each header. Repeatedly used if its length less than the length of crosses
#' @param hjust.by.nodes,vjust.by.nodes hjust and vjust will be setted based on nodes ranges (wo blanks) or cross ranges (w/ blanks). Repeatedly used.
#'
#' @return an undated CrossLink object
#' @export
#'
#' @examples
#'
set_header <- function(
  object, header = NULL,
  hjust = 0.5, vjust = 0.95,
  hjust.by.nodes = F, vjust.by.nodes = F,
  layout = NULL){

  layout <- nullna_default(layout, default = cl_active(object))
  coord <- object@layout[[layout]]

  # remove header first if exists
  coord <- coord[coord[["node.type"]] != "header",,drop = F]

  cross_names <- names(object@cross)

  if(!identical(header, NA)) {
    if(is.null(header)) header <- cross_names

    header <- coerce_x_len(header, length(cross_names))

    # cross header
    header.x <- coerce_x_len(hjust, length(header))
    header.y <- coerce_x_len(vjust, length(header))
    hjust.by.nodes <- coerce_x_len(hjust.by.nodes, length(header))
    vjust.by.nodes <- coerce_x_len(vjust.by.nodes, length(header))

    coord <- rbind(
      coord,
      data.frame(
        node = paste0(cross_names, "_HEADER"),
        node.type = "header",
        x = sapply(seq_along(header),
                   function(i) {
                     scales::rescale(header.x[i],
                                     to = range(
                                       coord[
                                         coord[['cross']] == cross_names[i] &
                                         coord[['node.type']] %in%
                                               (if(hjust.by.nodes[i])
                                                 c("node")
                                                else
                                                  c("node", "top", "bottom")),
                                             "x"]),
                                     from = c(0,1))
                     }),
        y = sapply(seq_along(header),
                   function(i) {
                     scales::rescale(header.y[i],
                                     to = range(
                                       coord[
                                         coord[['cross']] == cross_names[i] &
                                         coord[['node.type']] %in%
                                               (if(vjust.by.nodes[i])
                                                 c("node")
                                                else
                                                  c("node", "top", "bottom")),
                                             "y"]),
                                     from = c(0,1))
                   }),
        cross = cross_names
      ))
  }

  object@layout[[layout]] <- coord
  object@params$header <- header

  return(object)
}


#' Get cross data for plotting
#'
#' Retrieve 2D coordinates of crosses
#'
#' @inheritParams cl_xrange
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
get_cross <- function(object, layout = NULL){
  layout <- nullna_default(layout, cl_active(object))
  cross_coord <- object@layout[[layout]]
  cross_coord <- cross_coord[cross_coord[["node.type"]] == "node",,drop = F]
  cross_data <- as.data.frame(
    cbind(cross_coord,
          object@nodes[match(cross_coord$node,
                             object@nodes[[object@params$key.by]]),
                       , drop = F]))
  return(cross_data)
}

#' Get link data for plotting
#'
#' Retrieve 2D coordinates of links
#'
#' @inheritParams get_cross
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
get_link <- function(object, layout = NULL){
  layout <- nullna_default(layout, cl_active(object))
  cross_coord <- object@layout[[layout]]
  cross_coord <- cross_coord[cross_coord[["node.type"]] == "node",,drop = F]

  link_data <- object@edges
  link_data$source <- link_data[[object@params$src.by]]
  link_data$target <- link_data[[object@params$tar.by]]
  link_data[c("x", "y")] <- cross_coord[match(link_data$source, cross_coord$node), c("x", "y")]
  link_data[c("xend", "yend")]<- cross_coord[match(link_data$target, cross_coord$node), c("x", "y")]
  return(link_data)
}

#' Get header data for plotting
#'
#' Retrieve 2D coordinates of headers
#'
#' @inheritParams get_cross
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
get_header <- function(object, layout = NULL){
  layout <- nullna_default(layout, cl_active(object))
  cross_coord <- object@layout[[layout]]

  if(!("header" %in% cross_coord[['node.type']])) {
    warning("Header data not found! You might use set_header to add headers")
    header <- NA
  }else{
    header_coord <- cross_coord[cross_coord[["node.type"]] == "header",,drop = F]
    header <- cbind(
      header_coord,
      header = object@params$header[match(header_coord$cross, names(object@cross))]
    )
  }

  return(header)
}


#' Plot a CrossLink object
#'
#' @param object a CrossLink object
#' @param layout the layout to be plot. Set NULL to plot current active layout
#' @param link a named list of arguments for links. see [ggplot2::geom_segment()]. Set NULL to use default settings, or Set NA to not show.
#' @param cross a named list of arguments for crosses. see [ggplot2::geom_point()]. Set NULL to use default settings, or Set NA to not show.
#' @param label a named list of arguments for node labels. see [ggplot2::geom_text()]. Set NULL to use default settings, or Set NA to not show.
#' @param header a named list of arguments for headers. see [ggplot2::geom_text()]. Set NULL to use default settings, or Set NA to not show.
#' @param ... other arguments passed to ???
#'
#' @return a ggplot2 object
#' @details
#' For link, cross, label, header: "geom" is restricted for setting geom function, such as list(geom = "arc") for link.
#'
#' @import ggplot2
#' @md
#' @export
#'
#' @examples
#'
cl_plot <- function(object, layout = NULL,
                    link = NULL,
                    cross = NULL,
                    label = NULL,
                    header = NULL,
                    ...){
  p_layer <- function(p, list, data_fun, default_aes, default_geom = NULL){
    if(!identical(list, NA)){
      list$mapping <- override_aes(list$mapping, default_aes)
      list$data <- nullna_default(list$data, data_fun(object, layout))
      geom <- paste0("geom_",nullna_default(list$geom, default_geom))
      list$geom <- NULL
      scale <- list$scale
      list$scale <- NULL
      p <- p + do.call(geom, list)
      if(!is.null(scale)) {
        scale_aes = unique(names(scale))
        for(i in seq_along(scale_aes)){
          p <- p+ eval(parse(text = scale[[scale_aes[i]]])) + ggnewscale::new_scale(scale_aes[i])
        }
      }

      return(p)
    }else{
      return(p)
    }
  }

  p <- ggplot()
  # link
  p <- p_layer(p,
               link, data_fun = get_link,
               default_aes = aes(x = x, y = y, xend = xend, yend = yend),
               default_geom = "segment")
  # cross
  p <- p_layer(p,
               cross, data_fun = get_cross,
               default_aes = aes(x = x, y = y),
               default_geom = "point")
  # label
  p <- p_layer(p,
               label, data_fun = get_cross,
               default_aes = aes_string(x = "x", y = "y", label = object@params$key.by),
               default_geom = "text")
  # header
  if(!identical(suppressWarnings(get_header(object, layout)), NA)){
    p <- p_layer(p,
                 header, data_fun = get_header,
                 default_aes = aes(x = x, y = y, label = header),
                 default_geom = "text")
  }

  #p <- p + xlim(cl_xrange(object)) + ylim(cl_yrange(object))

  return(p)
}



#' copy layout and canvas
#'
#' @param object a CrossLink object
#' @param to layout name to save into
#' @param from layout name to copy from, set NULL to use active layout.
#' @param override override the existed layout
#'
#' @return an updated CrossLink object
#' @export
#'
#' @examples
#'
cl_copy <- function(object, to, from = NULL, override = F) {
  cl_layouts <- cl_layouts(object)
  from <- nullna_default(from, default = cl_active(object))

  if(!from %in% cl_layouts) {
    warning(from, " is not in layouts, nothing to be done!")
    return(object)
  }
  if(to %in% cl_layouts){
    if(override) warning(to, " is already in layouts, which will be overided!")
    else stop(to, " is already in layouts, set overided = TRUE if you want to override it!")
  }
  message("Copy layout ", from," into ", to, ", and Set active layout to ", to)
  object@layout[[to]] <- object@layout[[from]]
  # object@canvas[[to]] <- object@canvas[[from]]
  cl_active(object) <- to

  return(object)
}

#' Retrieve coordinates of nodes
#'
#' @param object a CrossLink object
#' @param nodes keys of nodes to be retrieved
#' @param relative return relative values in the canvas or each cross (rel.by.cross = TRUE). values are in range of 0 to 1.
#' @param rel.by.cross return relative values to each cross.
#'
#' @return a vector of values
#' @export
#'
#' @examples
#'
cl_coord <- function(object, nodes, relative = F, rel.by.cross = F, layout = NULL){
  layout <- nullna_default(layout, cl_active(object))
  cl_active(object) <- layout

  coord <- object@layout[[layout]]

  node_coord <- coord[coord$node %in% nodes,,drop = F]
  xrange <- cl_xrange(object, layout, all = T)
  yrange <- cl_yrange(object, layout, all = T)
  if(relative){
    rel.by.cross <- if (rel.by.cross) "canvas" else as.character(node_coord$cross)
    node_coord$x <- do.call(c, mapply(
      scales::rescale, x = as.list(node_coord$x), to = list(c(0,1)), from = xrange[rel.by.cross],
      SIMPLIFY = F
    ))
    node_coord$y <- do.call(c, mapply(
      scales::rescale, x = as.list(node_coord$x), to = list(c(0,1)), from = yrange[rel.by.cross],
      SIMPLIFY = F
    ))
  }
  return(node_coord)
}

#' Convert relative coordinates
#'
#' @param object a CrossLink object
#' @param x,y relative coordinates
#' @param crosses relative about which cross
#' @param by.canvas relative about whole canvas
#' @param layout layout name to retrieve coordinates
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
cl_rel2abs <- function(object, x = 0, y = 0, crosses = NULL, by.canvas = F, layout = NULL){
  layout <- nullna_default(layout, cl_active(object))
  crosses <- nullna_default(crosses, names(object@cross))
  x <- coerce_x_len(x, length(crosses))
  y <- coerce_x_len(y, length(crosses))

  coord <- object@layout[[layout]]
  xrange <- cl_xrange(object, layout, all = T)
  yrange <- cl_yrange(object, layout, all = T)

  rel.by <- if (by.canvas) "canvas" else crosses

  x <- do.call(c, mapply(
    scales::rescale, x = as.list(x), from = list(c(0,1)), to = xrange[rel.by],
    SIMPLIFY = F
  ))
  y <- do.call(c, mapply(
    scales::rescale, x = as.list(y), from = list(c(0,1)), to = yrange[rel.by],
    SIMPLIFY = F
  ))

  data.frame(x = x, y = y)
}
# new_scale
# https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/




#' show available aes
#'
#' @param object a CrossLink object
#'
#' @return NULL
#' @export
#'
#' @examples
#'
show_aes <- function(object){
  cat("Available meta.data names are showing below.\n")
  cat("Cross:", paste(colnames(get_cross(object)), collapse = ", "), "\n")
  cat("Link:", paste(colnames(get_link(object)), collapse = ", "), "\n")
  cat("Header:", paste(colnames(suppressMessages(get_header(object))), collapse = ", "), "\n")
}





#
# pl_crosslink(
#   cl,
#   cross = list(mapping = aes(x, y, color = color, size = size)),
#   link = list(mapping = aes(x, y, xend = xend, yend = yend),
#                    color = "grey90"),
#   header = list(mapping = aes(x,y , label = header)),
#   label = list(mapping = aes(x,y, label = node))
#   )
#
# cl <- CrossLink(
#   nodes %>% mutate(type = factor(type, unique(type))),
#   edges, cross.by = "type", cross.header = NULL, cross.header.pos = c(0, 0.5),
#   xrange = c(0, 7), yrange = c(0, 11))
#
# cl %<>% tf_rotate()
# cl %<>% set_header()
# tmp1 <- get_link(cl)
# tmp2 <- get_link(cl)
# tmp2$x <- tmp2$xend
# tmp2$y <- tmp2$yend
# tmp <- rbind(tmp1, tmp2)
# tmp$group <- rep(1:nrow(tmp1), 2)
# ggplot() +
#   # geom_diagonal0(mapping = aes(x, y, xend = xend, yend = yend),
#   #                color = "grey",
#   #                data = get_link(cl)) +
#   geom_diagonal2(mapping = aes(x, y,group = group, color = as.character(y)),
#                  data = tmp) +
#   ggnewscale::new_scale_color() +
#   geom_point(mapping = aes(x, y, color = color, size = size),
#              data = get_cross(cl)) +
#   scale_color_manual(values = unique(cl@nodes$color)) +
#   # geom_text(mapping = aes(x,y , label = header),
#   #           data = get_header(cl, y.by.each.cross = T)) +
#   xlim(cl_xrange(cl)) + ylim(cl_yrange(cl))# +  theme_void()


# Set Header should be the last
