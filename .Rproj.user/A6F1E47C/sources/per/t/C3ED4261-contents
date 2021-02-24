# http://adv-r.had.co.nz/OO-essentials.html
CrossLink <- setClass(
  "CrossLink",
  slots = c( # slots should be: logical TOC, layer by layer, minimize layers' depth (<= 3)
    nodes = "data.frame",
    edges = "data.frame",
    cross = "list",
    layout = "list", #
    canvas = "list", #
    active.layout = "character",
    params = "list"
  ))

# edges A data frame containing a symbolic edge list in the first two columns. Additional columns are considered as edge attributes.
# nodes A data frame with vertex metadata

# I try to keep one function clean and simple, which should be friendly.
# I prefere using %>% to pipe functions, and this way should be more intuitive and easy to use.
CrossLink <- function(
  nodes, edges,
  cross.by,
  cross.scale.gap = NA, # null or vector
  cross.scale.spacing = NA, # na/null or list, partial match is supported
  cross.header = NA, # NA default name, set NULL to not show headers. vector of length crosses
  cross.header.pos = c(0.5, 1), # n times of 2
  node.key.by = 1,
  edge.source.by = 1,
  edge.target.by = 2,
  node.odd.rm = FALSE,
  xrange = NULL,
  yrange = NULL){

  # coerce to data.frame
  nodes <- as.data.frame(nodes)
  edges <- as.data.frame(edges)

  # default key index
  node.key.by <- index_to_name(x = nodes, ind = node.key.by)
  edge.source.by <- index_to_name(x = edges, ind = edge.source.by)
  edge.target.by <- index_to_name(x = edges, ind = edge.target.by)
  cross.by <- index_to_name(x = nodes, ind = cross.by)

  # clean nodes and edges
  nodes <- nodes[!is.na(nodes[[cross.by]]),] # remove nodes not included in crosses
  edges <- edges[((edges[[edge.source.by]] %in% nodes[[node.key.by]]) & (edges[[edge.target.by]] %in% nodes[[node.key.by]])),]

  node_inedges <- unique(c(edges[[edge.source.by]], edges[[edge.target.by]]))

  if(node.odd.rm){
    nodes <- nodes[nodes[[node.key.by]] %in% node_inedges,]
  }

  node_key <- nodes[[node.key.by]]
  node_crs <- nodes[[cross.by]]

  # check nodes uniqueness
  if(!identical(node_key, unique(node_key))) stop("keys of nodes must be unique!")

  # cross
  cross <- tolist_by_group(factor(node_key), node_crs, drop = T)

  # range
  xrange <- nullna_default(xrange, c(1, length(cross)))
  yrange <- nullna_default(yrange, c(1, do.call(max, lapply(cross, length))))

  # layout
  layout <- list(
    init = data.frame(
      node = unlist(cross),
      x = vector_to_coord(x = factor(rep(names(cross),
                                         times = sapply(cross, length)),
                                     levels = levels(factor(node_crs))),
                          scale = cross.scale.gap,
                          range = xrange),
      y = do.call(c, mapply(FUN = vector_to_coord,
                            x = cross,
                            scale = list_along(names(cross), default = cross.scale.spacing),
                            range = list_along(names(cross), init = yrange))),
      cross = rep(names(cross),
                  times = sapply(cross, length))
    )
  )

  # cross.header
  if(!is.null(cross.header)){
    cross.header.pos <- coerce_x_len(cross.header.pos, 2*length(cross))
    cross.header <- nullna_default(cross.header, names(cross))
    cross.header <- data.frame(
      cross = names(cross),
      header = cross.header,
      x = cross.header.pos[2*(seq_len(length(cross))) -1],
      y = cross.header.pos[2*(seq_len(length(cross)))])
  }


  canvas <- list(
    init = list(
      xrange = xrange,
      yrange = yrange,
      cross.header = cross.header
      )
    )
  active.layout <- "init"

  # params
  params <- list(
    node.key.by = node.key.by,
    edge.source.by = edge.source.by,
    edge.target.by = edge.target.by,
    cross.by = cross.by
  )

  return(
    new("CrossLink",
        nodes = nodes,
        edges = edges,
        cross = cross,
        layout = layout,
        canvas = canvas,
        active.layout = active.layout,
        params = params)
    )
}

#
setMethod(
  f = "show",
  signature = "CrossLink",
  definition = function(object) {
    cat("An object of class CroosLink\n")
    n_node <- nrow(object@nodes)
    n_edge <- nrow(object@edges)
    n_cross <- length(object@cross)
    cat(n_edge, "edges between", n_node, "nodes within", n_cross, "crosses\n")
    cat("Active layout:", object@active.layout,"\n")
    cat("Layouts:", names(object@layout), "\n")
    }
  )

#
layouts <- function(object){
  names(object@layout)
}

#
ActiveLayout <- function(object) {
  object@active.layout
}

#
`ActiveLayout<-` <- function(object, value) {
    if(length(value) != 1) stop("value must be of length 1.")
    if(value %in% names(object@layout)){
      object@active.layout <- value
      return(object)
    }else{
      stop(value, " is not in layout")
    }
}


XRange <- function(object, layout = NULL){
  object@canvas[[nullna_default(layout, ActiveLayout(object))]][["xrange"]]
}
YRange <- function(object, layout = NULL){
  object@canvas[[nullna_default(layout, ActiveLayout(object))]][["yrange"]]
}

`XRange<-` <- function(object, layout = NULL, value){
  if(length(value) != 2) stop("value must be of length 2.")
  object@canvas[[nullna_default(layout, ActiveLayout(object))]][["xrange"]] <- value
  return(object)
}

`YRange<-` <- function(object, layout = NULL, value){
  if(length(value) != 2) stop("value must be of length 2.")
  object@canvas[[nullna_default(layout, ActiveLayout(object))]][["yrange"]] <- value
  return(object)
}

# layout and canvas are coupled.
layout_copy <- function(object, from = "transforming", to = "transformed") {
  layouts <- layouts(object)
  if(!from %in% layouts) {
    warning(from, " is not in layouts, nothing to be done!")
    return(object)
  }
  if(to %in% layouts){
    warning(to, " is already in layouts, which will be overided!")
  }
  message("Copy layout ", from ," into ", to, ", and Set ActiveLayout as ", to)
  object@layout[[to]] <- object@layout[[from]]
  object@canvas[[to]] <- object@canvas[[from]]
  ActiveLayout(object) <- to

  return(object)
}

#
tf_affine <- function(
  object,
  type = c("none", "translate", "scale", "rotate", "shear", "reflect"),
  x = 0, y = 0, angle = NA, counterclockwise = FALSE,
  by.each.cross = F,
  crosses = NULL, # tf these crosses (names)
  layout = "transforming"
) {
  active.layout <- object@active.layout
  coord <- object@layout[[active.layout]]
  canvas <- object@canvas[[active.layout]]
  xrange <- XRange(object, layout = active.layout)
  yrange <- YRange(object, layout = active.layout)

  type <- match.arg(arg = type, several.ok = FALSE)
  theta <- pi*angle/180

  crosses <- nullna_default(crosses, names(object@cross))

  if(by.each.cross){
    n_cross <- length(crosses)
    x <- coerce_x_len(x, n_cross)
    y <- coerce_x_len(y, n_cross)
    theta <- coerce_x_len(theta, n_cross)
    counterclockwise <- coerce_x_len(counterclockwise, n_cross)

    xrange_tf <- NULL
    yrange_tf <- NULL
    for (i in seq_len(n_cross)) {
      ind <- which(coord$cross == crosses[[i]])
      coord_i <- coord[ind, c("x", "y")]
      # origin point
      x0 <- scales::rescale(x[[i]], to = range(coord_i$x), from = c(0, 1))
      y0 <- scales::rescale(y[[i]], to = range(coord_i$y), from = c(0, 1))

      tf_fun_m <- function(m_xy, xx = x0, yy = y0) {
        transform_by_matrix(
          m_xy,
          matrix = transform_matrix_affine(
            type = type,
            x = xx, y = yy, theta = theta[[i]],
            counterclockwise = counterclockwise[[i]]))}

      # cross_coord
      coord[ind, c("x", "y")] <- tf_fun_m(coord_i)

      # xrange yrange
      xyrange <- tf_fun_m(expand.grid(xrange, yrange))
      xrange_tf <- range(c(xrange_tf, xyrange[,1]))
      xrange_tf <- range(c(xrange_tf, xyrange[,2]))

      # header
      if(!is.null(canvas$cross.header)){
        canvas$cross.header[i, c("x", "y")] <- tf_fun_m(
          canvas$cross.header[i, c("x", "y"), drop = F],
          xx = x[[i]], yy = y[[i]])# range(0, 1)
      }
    }
  }else{
    x0 <- scales::rescale(x[[1]], to = canvas$xrange, from = c(0, 1))
    y0 <- scales::rescale(y[[1]], to = canvas$yrange, from = c(0, 1))

    ind <- which(coord$cross %in% crosses)

    tf_fun_m <- function(m_xy, xx = x0, yy = y0) {
      transform_by_matrix(
        m_xy,
        matrix = transform_matrix_affine(
          type = type,
          x = xx, y = yy, theta = theta[[1]],
          counterclockwise = counterclockwise[[1]]))}
    coord[ind, c("x", "y")] <- tf_fun_m(coord[ind, c("x", "y")])

    # range
    xyrange <- tf_fun_m(expand.grid(xrange, yrange))
    xrange_tf <- range(xyrange[,1])
    yrange_tf <- range(xyrange[,2])

    # header
    if(!is.null(canvas$cross.header)){
      canvas$cross.header[,c("x", "y")] <- tf_fun_m(
        canvas$cross.header[, c("x", "y"), drop = F],
        xx = x[[1]], yy = y[[1]]) # range(0, 1)
    }
  }

  object@layout[[layout]] <- coord
  canvas$xrange <- xrange_tf
  canvas$yrange <- yrange_tf
  object@canvas[[layout]] <- canvas
  ActiveLayout(object) <- layout

  return(object)
}

# wrappers
tf_rotate <- function(
  object, x = 0.5, y = 0.5, angle = 90, by.each.cross = F, crosses = NULL,
  layout = "transforming", counterclockwise = FALSE) {

  tf_affine(
    object, type = c("rotate"), x = x, y = y, angle = angle,
    counterclockwise = counterclockwise, by.each.cross = by.each.cross,
    crosses = crosses, layout = layout)
  }

#
tf_flip <- function(
  object, axis = c("x", "y"), by.each.cross = F, crosses = NULL, layout = "transforming") {

  axis <- match.arg(axis)
  x <- 0
  y <- 0
  angle <- switch(axis, x = 0, y = 90)
  tf_affine(
    object, type = c("reflect"), x = x, y = y, angle = angle,
    counterclockwise = T, by.each.cross = by.each.cross,
    crosses = crosses, layout = layout)
}

#
tf_shift <- function(
  object, x = 0, y = 0, by.each.cross = F, crosses = NULL, # tf these crosses (names)
  layout = "transforming") {

  tf_affine(
    object, type = c("translate"), x = x, y = y, theta = NA,
    counterclockwise = F, by.each.cross = by.each.cross, layout = layout)
}

# shear
tf_shear <- function(
  object, axis = c("x", "y"), angle = 15, # no more than 90
  by.each.cross = F, crosses = NULL, layout = "transforming", counterclockwise = FALSE) {

  axis <- match.arg(axis)
  x <- pi*angle/180
  y <- pi*angle/180
  if(axis == "x") y <- 0 else x <- 0

  tf_affine(
    object, type = c("shear"),x = x, y = y, angle = NA,
    counterclockwise = counterclockwise,by.each.cross = by.each.cross,
    crosses = crosses,layout = layout)
}

# scale
tf_scale <- function(
  object, x = 1, y = 1, by.each.cross = F, crosses = NULL, layout = "transforming") {

  tf_affine(
    object, type = c("scale"), x = x, y = y, angle = NA, counterclockwise = F,
    by.each.cross = by.each.cross, crosses = crosses, layout = layout)
}

#
tf_fun <- function(
  object, fun, along = c("x", "y", "xy"),
  layout = NULL,
  xrange.to = NULL, xrange.from = NULL,
  yrange.to = NULL, yrange.from = NULL) {

  layout <- nullna_default(layout, object@active.layout)
  x <- object@coord[[layout]][["x"]]
  y <- object@coord[[layout]][["y"]]

  xy <- transform_by_fun(
    x = x, y = y, fun = fun, along = along,
    xrange.to = xrange.to, xrange.from = xrange.from,
    yrange.to = yrange.to, yrange.from = yrange.from)
  object@coord[[layout]][["x"]] <- xy[["x"]]
  object@coord[[layout]][["y"]] <- xy[["y"]]

  XRange(object, layout = layout) <- range(xy[['x']], XRange(object, layout = layout))
  YRange(object, layout = layout) <- range(xy[['y']], YRange(object, layout = layout))

  return(object)
}


# new_scale
# https://eliocamp.github.io/codigo-r/2018/09/multiple-color-and-fill-scales-with-ggplot2/

getCrossData <- function(object, layout = NULL){
  layout <- nullna_default(layout, ActiveLayout(object))
  cross_coord <- object@layout[[layout]]
  cross_data <- as.data.frame(
    cbind(cross_coord,
          object@nodes[match(cross_coord$node,
                             object@nodes[[object@params$node.key.by]]),
                       , drop = F]))
  return(cross_data)
}

getLinkData <- function(object, layout = NULL, geom = c("segment", "curve", "arc", "bezier")){
  layout <- nullna_default(layout, ActiveLayout(object))
  cross_coord <- object@layout[[layout]]
  link_data <- object@edges
  link_data$source <- link_data[[object@params$edge.source.by]]
  link_data$target <- link_data[[object@params$edge.target.by]]
  link_data[c("x", "y")] <- cross_coord[match(link_data$source, cross_coord$node), c("x", "y")]
  link_data[c("xend", "yend")]<- cross_coord[match(link_data$target, cross_coord$node), c("x", "y")]
  return(link_data)
}

getHeaderData <- function(object, layout = NULL, relative = F, y.by.each.cross = F){
  layout <- nullna_default(layout, ActiveLayout(object))
  cross_coord <- object@layout[[layout]]
  header_pos <- object@canvas[[layout]]$cross.header

  if(is.null(header_pos)) {
    warning("Header not found!")
    return(header_pos)
  }else{
    if(relative)
      return(header_pos)
    else{
      header_coord <- header_pos
      for(i in 1:nrow(header_pos)){
        ind <- cross_coord$cross == header_pos[i, "cross"]
        header_coord[i, "x"] <- scales::rescale(header_pos[i, "x"],
                                                range(cross_coord[ind, "x"]),
                                                c(0,1))

        if(y.by.each.cross){
          yrange <- range(cross_coord[ind, "y"])
          yrange[2] <- yrange[2] + 0.05 * diff(yrange)
        }else{
          yrange <- YRange(object)
        }
        header_coord[i, "y"] <- scales::rescale(header_pos[i, "y"],
                                                yrange,
                                                c(0,1))
      }
      return(header_coord)
    }
  }

}

setHeader <- function(object, cross.header = NULL, cross.header.pos = c(0.5, 1), layout = NULL){
  layout <- nullna_default(layout, default = ActiveLayout(object))

  cross_names <- names(object@cross)
  # cross.header
  if(!is.null(cross.header)){
    cross.header.pos <- coerce_x_len(cross.header.pos, 2*length(cross))
    cross.header <- nullna_default(cross.header, cross_names)
    cross.header <- data.frame(
      cross = cross_names,
      header = cross.header,
      x = cross.header.pos[2*(seq_len(length(cross_names))) -1],
      y = cross.header.pos[2*(seq_len(length(cross_names)))])
    object@canvas[[layout]]$cross.header <- cross.header
  }else{
    object@canvas[[layout]]["cross.header"] <- cross.header # ["name"] <- allow NULL to be assigned
  }

  return(object)
}
# helper function
showAes <- function(object){
  cat("Available meta.data names are showing below.\n")
  cat("Cross:", paste(colnames(getCrossData(object)), collapse = ", "), "\n")
  cat("Link:", paste(colnames(getLinkData(object)), collapse = ", "), "\n")
  cat("Header:", paste(colnames(suppressWarnings(getHeaderData(object))), collapse = ", "), "\n")
}


load("data/columns.rda")
load("data/nodes.rda")
load("data/edges.rda")

library(tidyverse)
library(magrittr)

pl_crosslink <- function(
  object,
  args.cross = NULL,
  args.link = NULL,
  args.header = NULL,
  args.label = NULL){

  p <- ggplot()
  if(!is.null(args.link)) p <- p + do.call(geom_segment, c(args.link, list(data = getLinkData(object))))
  if(!is.null(args.cross)) p <- p +   do.call(geom_point, c(args.cross, list(data = getCrossData(object))))
  if(!is.null(args.header)) {
    header <- getHeaderData(object)
    if(is.null(header)) warning("Header data is not found!")
    else p <- p +  do.call(geom_text, c(args.header, list(data = header)))
  }
  if(!is.null(args.label)) p <- p +  do.call(geom_text, c(args.label, list(data = getCrossData(object))))

  p <-  p +
    xlim(XRange(object)) + ylim(YRange(object)) +
    theme_void()

  return(p)
}

pl_crosslink(
  cl,
  args.cross = list(mapping = aes(x, y, color = color, size = size)),
  args.link = list(mapping = aes(x, y, xend = xend, yend = yend),
                   color = "grey90"),
  args.header = list(mapping = aes(x,y , label = header)),
  args.label = list(mapping = aes(x,y, label = node))
  )

cl <- CrossLink(
  nodes %>% mutate(type = factor(type, unique(type))),
  edges, cross.by = "type", cross.header = NULL, cross.header.pos = c(0, 0.5),
  xrange = c(0, 7), yrange = c(0, 11))

cl %<>% tf_rotate()
cl %<>% setHeader()
tmp1 <- getLinkData(cl)
tmp2 <- getLinkData(cl)
tmp2$x <- tmp2$xend
tmp2$y <- tmp2$yend
tmp <- rbind(tmp1, tmp2)
tmp$group <- rep(1:nrow(tmp1), 2)
ggplot() +
  # geom_diagonal0(mapping = aes(x, y, xend = xend, yend = yend),
  #                color = "grey",
  #                data = getLinkData(cl)) +
  geom_diagonal2(mapping = aes(x, y,group = group, color = as.character(y)),
                 data = tmp) +
  ggnewscale::new_scale_color() +
  geom_point(mapping = aes(x, y, color = color, size = size),
             data = getCrossData(cl)) +
  scale_color_manual(values = unique(cl@nodes$color)) +
  # geom_text(mapping = aes(x,y , label = header),
  #           data = getHeaderData(cl, y.by.each.cross = T)) +
  xlim(XRange(cl)) + ylim(YRange(cl))# +  theme_void()


# Set Header should be the last
