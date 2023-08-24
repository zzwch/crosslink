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
#' @slot params list
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
#' @param nodes a data frame with unique keys in the 1st (settable by \code{key.by}) column and metadata (used for aesthetic of nodes) in the other columns.
#' @param edges a data frame with source and target nodes in the first two (settable by \code{src.by} and \code{tar.by}) columns and other metadata.
#' @param cross.by the name of a column in \code{nodes}, by which nodes will be grouped into different crosses.
#' To control the order of crosses and the order of nodes, set factor levels for the corresponding column in \code{nodes}. see \code{\link{factor}} or [forcats::fct_relevel()]
#' @param gaps a numeric vector of n-1 length, n is the number of crosses (that is \code{length(object@cross)}).
#'  It's a vector of gaps between adjacent crosses.
#'  set NA or NULL to use equally divided gaps.
#' @param spaces set \code{"equal"} for equidistant globally in all crosses,
#' set \code{"partition"} for equidistant separately in each cross,
#' or set \code{"flank"}, which is similar to \code{"partition"} but with top/bottom flank anchors included.
#' Default: \code{equal}.
#' @param xrange,yrange range of x and y. set NULL or NA to ignore this.
#' @param key.by name or index of the column in \code{nodes} to be used as key of nodes
#' @param src.by name or index of the column in \code{edges} to be used as source of links (edges)
#' @param tar.by name or index of the column in \code{edges} to be used as target of links (edges)
#' @param odd.rm remove odd nodes that are not included in edges (without links).
#' @md
#'
#' @details IMPORTANT! The colnames of 'node', 'cross', 'node.type', 'x', 'y', 'degree' MUST NOT BE included in nodes and edges!
#' @return a CrossLink object
#' @export
#'
#' @examples
#'
crosslink <- function(
  nodes, edges, cross.by, gaps = NULL, spaces = c( "equal", "partition", "flank"),
  xrange = NULL, yrange = NULL,
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

  # node degree
  nodes$degree <- 0
  tab_degree <- table(c(as.character(edges[[src.by]]), as.character(edges[[tar.by]])))
  nodes[match(names(tab_degree), node_key), "degree"] <- tab_degree

  # edge source/target
  edges$src.cross <- node_crs[match(edges[[src.by]], node_key)]
  edges$tar.cross <- node_crs[match(edges[[tar.by]], node_key)]

  # cross
  cross <- tolist_by_group(factor(node_key), node_crs, drop = T) # factor(x) will drop unused levels

  # range
  xrange <- nullna_default(xrange, c(1, length(cross)))
  yrange <- nullna_default(yrange, c(0, do.call(max, lapply(cross, length))+1))
  # xrange = c(0, 10), yrange = c(0, 10),


  # levels from up to down
  cross_anchored <- sapply(names(cross), function(x){
    x_nodes <- c(paste0(x, "_BOTTOM"), rev(levels(factor(cross[[x]]))), paste0(x, "_TOP"))
    factor(x = x_nodes, levels = x_nodes)
  },simplify = F, USE.NAMES = T)

  # spaces
  spaces <- match.arg(spaces)
  space_equal <- 1/(do.call(max, lapply(cross_anchored, length)) - 1)
  spaces <- switch(spaces,
                   equal = sapply(cross,
                                  function(i) {
                                    i_n <- length(i) - 1
                                    ispace <- space_equal
                                    iflank <- (1 - ispace * i_n)/2
                                    c(iflank, rep(ispace, i_n), iflank)
                                    }, simplify = F, USE.NAMES = T),
                   partition = sapply(cross,
                                      function(i) {
                                        i_n <- length(i) - 1
                                        iflank <- space_equal
                                        ispace <- (1 - iflank * 2)/i_n
                                        c(iflank, rep(ispace, i_n), iflank)
                                        }, simplify = F, USE.NAMES = T),
                   flank = sapply(cross,
                                  function(i) {
                                    i_n <- length(i) + 1
                                    rep(1/i_n, i_n)
                                  }, simplify = F, USE.NAMES = T)
                   )

  # layout
  layout <- list(
    default = data.frame(
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
    cross.by = cross.by,
    xscales = gaps,
    yscales = spaces,
    yspace = space_equal,
    xrange = xrange,
    yrange = yrange
  )

  return(
    new("CrossLink",
        nodes = nodes,
        edges = edges,
        cross = cross,
        layout = layout,
        active = "default",
        params = params)
    )
}


#' change spaces in default layout
#'
#' @param object a CrossLink object
#' @param spaces A named list of numeric vectors or characters of "equal", "partition" and "flank" (see \code{\link{crosslink}}).
#' For each numeric vector, its length must be n+1, and n is the number of nodes in the corresponding cross;
#' The vector is c(top flank, spaces between adjacent nodes, bottom flank);
#' "equal", "partition" and "flank" will be converted to numeric vectors internally.
#' Only crosses whose names are among the list's names will be changed.
#' @details
#' This function will only change default layout.
#' So, it is recommended to use this function after \code{\link{crosslink}},
#' and before transformation.
#' @return an updated CrossLink object
#' @export
#'
#' @examples
#'
set_space <- function(object, spaces){
  if(!is.list(spaces)) stop("spaces setting must be a list!")
  yscales <- object@params$yscales
  yspace <- object@params$yspace
  default <- object@layout$default

  for(i in names(spaces)) {
    if(i %in% names(yscales)){
      i_sc <- yscales[[i]]
      i_sp <- spaces[[i]]
      if(!is.numeric(i_sp)){
        if(is.character(i_sp) && length(i_sp) == 1){
          i_n <- length(i_sc) - 1 - 1
          i_sp <- switch(i_sp,
                         equal = c((1-yspace*i_n)/2, rep(yspace, i_n), (1-yspace*i_n)/2),
                         partition = c(yspace, rep((1-2*yspace)/i_n, i_n), yspace),
                         flank = rep(1/length(i_sc), length(i_sc))
          )
        }else{
          stop("Invalid space setting for ", i)
        }
      }else{
        if(length(i_sp) != length(i_sc)) stop("Length is not right for ", i)
      }
      i_sc <- i_sp/sum(i_sp)

      # layout
      i_ind <- default$cross == i & default$node.type != "header"
      default[i_ind, "y"] <- vector_to_coord(factor(default[i_ind, "node"]), i_sc, range(default[i_ind, "y"]))
    }
  }

  object@layout$default <- default
  return(object)
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
#' @param crosses subset crosses. NULL for all crosses.
#'
#' @return range of X or Y axis for the given layout
#' @export
#' @name cl_xrange
#'
#' @examples
#'
cl_xrange <- function(object, layout = NULL, all = F, crosses = NULL, include.flank = T){
  coord <- object@layout[[nullna_default(layout, cl_active(object))]]
  coord <- coord[coord$node.type != "header", ,drop = F]
  if(!include.flank){
    coord <- coord[coord$node.type == "node", ,drop = F]
  }

  crosses <- nullna_default(crosses, names(object@cross))

  if(!all) return(range(coord[coord$cross %in% crosses, "x"]))
  else{
    c(list(canvas = range(coord[["x"]])),
            sapply(crosses,
                   function(crs) {
                     range(coord[coord$cross == crs, "x"])
                   },
                   simplify = F, USE.NAMES = T))
    }
}



#' @rdname cl_xrange
#' @export
cl_yrange <- function(object, layout = NULL, all = F, crosses = NULL, include.flank = T){
  coord <- object@layout[[nullna_default(layout, cl_active(object))]]
  coord <- coord[coord$node.type != "header", ,drop = F]
  if(!include.flank){
    coord <- coord[coord$node.type == "node", ,drop = F]
  }
  crosses <- nullna_default(crosses, names(object@cross))

  if(!all) return(range(coord[coord$cross %in% crosses, "y"]))
  else{
    c(list(canvas = range(coord[["y"]])),
      sapply(crosses,
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
#' @param hjust.by.nodes,vjust.by.nodes hjust and vjust will be setted based on nodes ranges (w/o blanks) or cross ranges (w/ blanks). Repeatedly used.
#'
#' @return an updated CrossLink object
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
  node_data <- object@nodes
  link_data$source <- link_data[[object@params$src.by]]
  link_data$target <- link_data[[object@params$tar.by]]
  link_data$src.degree <- node_data$degree[match(link_data$source, node_data[[object@params$key.by]])]
  link_data$tar.degree <- node_data$degree[match(link_data$target, node_data[[object@params$key.by]])]
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
#' @param add other gg objects to be added to final plot, such as theme(). use list of multiple gg objects to add them all.
#' @param annotation generated by cl_annotation()
#'
#' @return a ggplot2 object
#' @details
#' For link, cross, label, header: "geom" is restricted for setting geom function, such as list(geom = "arc") for link.
#'
#' @import ggplot2
#' @import patchwork
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
                    add = NULL,
                    annotation = cl_annotation()){
  p_layer <- function(p, list, data_fun, default_aes, default_geom, default_param = NULL){
    if(!identical(list, NA)){
      list$mapping <- override_aes(list$mapping, default_aes)
      list$data <- nullna_default(list$data, data_fun(object, layout))
      params <- setdiff(names(default_param), names(list))
      for (i in params) {
        list[[i]] <- default_param[[i]]
      }
      geom <- paste0("geom_", nullna_default(list$geom, default_geom))
      list$geom <- NULL
      scale <- list$scale
      list$scale <- NULL
      p <- p + do.call(geom, list)
      if(!is.null(scale)) {
        scale_aes = unique(names(scale))
        for(i in seq_along(scale_aes)){
          #p <- p+ eval(parse(text = scale[[scale_aes[i]]])) + ggnewscale::new_scale(scale_aes[i])
          p <- ggplot_add(scale[[scale_aes[i]]], p) + ggnewscale::new_scale(scale_aes[i])
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
               default_aes = aes(x = x, y = y, xend = xend, yend = yend, color = src.cross),
               default_geom = "segment")
  # cross
  p <- p_layer(p,
               cross, data_fun = get_cross,
               default_aes = aes(x = x, y = y, size = degree, color = cross),
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
                 default_geom = "text", default_param = list(fontface = "bold"))
  }

  # expand
  p <- p +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1))

  # add
  # if(is.ggproto(add)){
  #  p <- ggplot_add(add, p)
  # }
  if(class(add) == "list"){
    for(i in length(add)){
      p <- ggplot_add(add[[i]], p)
    }
  }else{
    p <- ggplot_add(add, p)
  }

  # Annotations
  if(is.list(annotation)){
    cross_coord <- get_cross(object, layout = layout)
    annotation_flag <- 0
    for(i in c("top", "bottom", "left","right")){
      i_p <- annotation[[i]]
      i_b <- annotation[[paste0(i, ".by")]]

      if(!(is.null(i_p) || is.null(i_b))){
        i_axis <- ifelse(i %in% c("top", "bottom"), "x", "y")
        i_axis_ann <- ifelse(layer_scales(i_p)[[i_axis]][["position"]] %in% c("top", "bottom"), "x", "y") # if coord_flip

        i_range <- range(cross_coord[cross_coord$cross == i_b, i_axis])
        i_prange <- ggplot_build(p)$layout$panel_params[[1]][[paste0(i_axis, ".range")]]
        i_expand <- if(i_axis_ann == "x"){
          if(is.numeric(aplot::xrange(i_p))) scale_x_continuous(expand = expansion(mult = abs(i_range - i_prange)/diff(i_prange)))
          else scale_x_discrete(expand = expansion(mult = abs(i_range - i_prange)/diff(i_range)))
        }else{
          if(is.numeric(aplot::yrange(i_p))) scale_y_continuous(expand = expansion(mult = abs(i_range - i_prange)/diff(i_prange)))
          else scale_y_discrete(expand = expansion(mult = abs(i_range - i_prange)/diff(i_range)))
        }
        annotation[[i]] <- i_p + i_expand
        annotation_flag <- annotation_flag + 1
      }else{
        annotation[[paste0(i, ifelse(i %in% c("top", "bottom"), ".height", ".width"))]] <- 0
        annotation[[i]] <- plot_spacer()
      }
    }
    if(annotation_flag > 0){

      p <- annotation[["top"]] + annotation[["left"]] + p + annotation[["right"]] + annotation[["bottom"]] +
        patchwork::plot_layout(ncol = 3, nrow = 3,
                               widths = c(annotation[["left.width"]], 1, annotation[["right.width"]]),
                               heights = c(annotation[["top.height"]], 1, annotation[["bottom.height"]]),
                               guides = "collect",
                               design = c(patchwork::area(1,2,1,2),
                                          patchwork::area(2,1,2,1),
                                          patchwork::area(2,2,2,2),
                                          patchwork::area(2,3,2,3),
                                          patchwork::area(3,2,3,2))
      )
    }
  }

  return(p)
}



#' Plot a CrossLink object
#'
#' @param object a CrossLink object
#' @param p a ggplot2 object for crosslink plot
#' @param layout the layout to be plot. Set NULL to plot current active layout
#' @param annotation generated by cl_annotation()
#'
#' @return a ggplot2 object
#' @details
#'
#' @import ggplot2
#' @import patchwork
#' @md
#' @export
#'
#' @examples
#'
cl_plot2 <- function(p, object, layout = NULL, annotation = cl_annotation()){
  # Annotations
  if(is.list(annotation)){
    cross_coord <- get_cross(object, layout = layout)
    annotation_flag <- 0
    for(i in c("top", "bottom", "left","right")){
      i_p <- annotation[[i]]
      i_b <- annotation[[paste0(i, ".by")]]

      if(!(is.null(i_p) || is.null(i_b))){
        i_axis <- ifelse(i %in% c("top", "bottom"), "x", "y")
        i_axis_ann <- ifelse(layer_scales(i_p)[[i_axis]][["position"]] %in% c("top", "bottom"), "x", "y") # if coord_flip

        i_range <- range(cross_coord[cross_coord$cross == i_b, i_axis])
        i_prange <- ggplot_build(p)$layout$panel_params[[1]][[paste0(i_axis, ".range")]]
        i_expand <- if(i_axis_ann == "x"){
          if(is.numeric(aplot::xrange(i_p))) scale_x_continuous(expand = expansion(mult = abs(i_range - i_prange)/diff(i_prange)))
          else scale_x_discrete(expand = expansion(mult = abs(i_range - i_prange)/diff(i_range)))
        }else{
          if(is.numeric(aplot::yrange(i_p))) scale_y_continuous(expand = expansion(mult = abs(i_range - i_prange)/diff(i_prange)))
          else scale_y_discrete(expand = expansion(mult = abs(i_range - i_prange)/diff(i_range)))
        }
        annotation[[i]] <- i_p + i_expand
        annotation_flag <- annotation_flag + 1
      }else{
        annotation[[paste0(i, ifelse(i %in% c("top", "bottom"), ".height", ".width"))]] <- 0
        annotation[[i]] <- plot_spacer()
      }
    }
    if(annotation_flag > 0){

      p <- annotation[["top"]] + annotation[["left"]] + p + annotation[["right"]] + annotation[["bottom"]] +
        patchwork::plot_layout(ncol = 3, nrow = 3,
                               widths = c(annotation[["left.width"]], 1, annotation[["right.width"]]),
                               heights = c(annotation[["top.height"]], 1, annotation[["bottom.height"]]),
                               guides = "collect",
                               design = c(patchwork::area(1,2,1,2),
                                          patchwork::area(2,1,2,1),
                                          patchwork::area(2,2,2,2),
                                          patchwork::area(2,3,2,3),
                                          patchwork::area(3,2,3,2))
        )
    }
  }

  return(p)
}


#' Helper for annotation in cl_plot
#'
#' @param top,bottom,left,right ggplot object
#' @param top.by,bottom.by,left.by,right.by name of cross by which to align ggplot
#' @param top.width,bottom.width,left.width,righ.width width of ggplot to be aligned, relative value to main plot.
#'
#' @return a list of arguments
#' @export
#'
#' @examples
#'
cl_annotation <- function(
  top = NULL,  top.by = NULL, top.height = 1,
  bottom = NULL, bottom.by = NULL, bottom.height = 1,
  left = NULL, left.by = NULL, left.width = 1,
  right = NULL, right.by = NULL, right.width = 1){
  as.list(environment())
}

#' copy layout
#'
#' @param object a CrossLink object
#' @param to layout name to save into
#' @param from layout name to copy from, set NULL to use active layout.
#' @param crosses a vector of cross names, and only these crosses will be copied or overrided. set NULL or NA for all crosses.
#' @param override override the existed layout
#'
#' @return an updated CrossLink object
#' @export
#'
#' @examples
#'
cl_copy <- function(object, to, from = NULL, crosses = NULL, override = F) {
  cl_layouts <- cl_layouts(object)
  from <- nullna_default(from, default = cl_active(object))

  if(!from %in% cl_layouts) {
    warning(from, " is not in layouts, nothing to be done!")
    return(object)
  }

  crosses <- nullna_default(crosses, names(object@cross))
  layout_ind <- object@layout[[from]][["cross"]] %in% crosses
  if(to %in% cl_layouts){
    if(override) {
      warning(to, " is already in layouts, and will be overrided!")
      to_layout <- object@layout[[to]]
      to_layout[layout_ind,] <- object@layout[[from]][layout_ind, ]

    }else stop(to, " is already in layouts, set overided = TRUE if you want to override it!")
  }else{
    to_layout <- object@layout[[from]][layout_ind, ]
  }


  message("Copy layout ", from," into ", to, ", and Set active layout to ", to)
  object@layout[[to]] <- to_layout
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




#' show available aesthetic
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



#' Align crosses in different layouts
#'
#' @param object CrossLink object
#' @param crosses.1 crosses in group 1
#' @param crosses.2 crosses in group 2
#' @param align.x align both groups in x
#' @param align.y align both groups in y
#' @param anchor.1 relative coordinates of the anchor in group 1 by which to align
#' @param anchor.2 relative coordinates of the anchor in group 2 by which to align
#' @param layout which layout coordinates to be used
#'
#' @return
#' @export
#'
#' @examples
#'
cl_align <- function(object, crosses.1, crosses.2, align.x = F, align.y = F, anchor.1 = c(0.5, 0.5), anchor.2 = c(0.5, 0.5), layout = NULL){

  layout <- nullna_default(layout, default = cl_active(object))

  anchor.1 <- coerce_x_len(anchor.1, 2)
  anchor.2 <- coerce_x_len(anchor.2, 2)

  anchor.1 <- c(scales::rescale(anchor.1[1], to = cl_xrange(object, crosses = crosses.1, layout = layout), from = c(0,1)),
                scales::rescale(anchor.1[2], to = cl_yrange(object, crosses = crosses.1, layout = layout), from = c(0,1)))

  anchor.2 <- c(scales::rescale(anchor.2[1], to = cl_xrange(object, crosses = crosses.2, layout = layout), from = c(0,1)),
                scales::rescale(anchor.2[2], to = cl_yrange(object, crosses = crosses.2, layout = layout), from = c(0,1)))

  xshift <- ifelse(align.x, anchor.1[1] - anchor.2[1], 0)
  yshift <- ifelse(align.y, anchor.1[2] - anchor.2[2], 0)

  tf_shift(object, x = xshift, y = yshift, crosses = crosses.2, relative = F, layout = layout)
}

#' a theme for crosslink plot
#'
#' @param gg a ggplot object
#' @param th a value returned by theme()
#'
#' @return an updated ggplot object
#' @export
#'
#' @examples
#'
cl_void <- function(gg, th = theme()) {
  base_size <- 11
  half_line <- base_size/2
  gg + theme(line = element_blank(), rect = element_blank(),
             text = element_text(family = "", face = "plain",
                                 colour = "black", size = base_size, lineheight = 0.9,
                                 hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                 debug = FALSE),
             axis.text = element_blank(),
             axis.title = element_blank(),
             axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL,
             axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
             axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
             axis.ticks.length.y.right = NULL, legend.box = NULL,
             legend.key.size = unit(1.2, "lines"), legend.position = "right",
             legend.text = element_text(size = rel(0.8)), legend.title = element_text(hjust = 0),
             strip.text = element_text(size = rel(0.8)), strip.switch.pad.grid = unit(half_line/2,
                                                                                      "pt"), strip.switch.pad.wrap = unit(half_line/2,
                                                                                                                          "pt"), panel.ontop = FALSE, panel.spacing = unit(half_line,
                                                                                                                                                                           "pt"), plot.margin = unit(c(0, 0, 0, 0), "lines"),
             plot.title = element_text(size = rel(1.2), hjust = 0,
                                       vjust = 1, margin = margin(t = half_line)), plot.title.position = "panel",
             plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(t = half_line)),
             plot.caption = element_text(size = rel(0.8), hjust = 1,
                                         vjust = 1, margin = margin(t = half_line)), plot.caption.position = "panel",
             plot.tag = element_text(size = rel(1.2), hjust = 0.5,
                                     vjust = 0.5), plot.tag.position = "topleft",
             complete = TRUE) + th
}
