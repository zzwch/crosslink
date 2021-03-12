library(crosslink)
library(ggplot2)
library(magrittr)

rm(list = ls())

n <- 4
demo <- gen_demo(n_cross = n, n_node = rep(n, n), n_link = rep(n, n-1), seed = 66)
nodes <- demo$nodes
edges <- demo$edges
cross.by <- demo$cross.by

object <- crosslink(nodes, edges, cross.by, odd.rm = F)
#########
object %>%
  set_header() %>%
  #layout_row() %>%
  #layout_hive() %>%
  layout_polygon() %>%
  #layout_arc(angles = 180) %>%
  tf_rotate(angle = 45, by.each.cross = F) %>%
  cl_plot(cross = list(mapping = aes(color = type, shape = type),
                       scale = list(color = 'scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral"))',
                                    shape = "scale_shape_manual(values = 1:7)")
                       ),
          #link = list(color = "gold", geom = "curve", curvature = 0.5),
          label = list(mapping = aes(color = type),
                       scale = list(
                         color = 'scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral"))'
                       ))) -> p1
  #cl_void() %>%
  print()

library(aplot)
p1 %>% insert_left(p1) %>% insert_right(p1) %>% insert_top(p1) %>% insert_bottom(p1)

pheatmap::pheatmap(mat = matrix(1:9, 3)) -> p2

patchwork::align_plots(p1, p2$gtable$grobs)
######
layout <- NULL
cross <- get_cross(object)
link <- get_link(object)
header <- get_header(object, layout)

ggplot() +
  geom_segment(mapping = aes(x = x, y =y, xend = xend, yend = yend), data = link) +
  #geom_point(mapping = aes(x = x, y = y), data = cross)+
  geom_point(mapping = aes(x = x, y = y, width = 0.1, height = 0.1, shape = type), size =5, data = cross)+
  geom_text(mapping = aes(x = x, y = y, label = header), data = header, vjust = 1, fontface = "bold", color = "red", size = 5) +
  geom_text(mapping = aes(x = x, y = y, label = key), data = cross, vjust = 2, fontface = "italic") +
  xlim(cl_xrange(object)) + ylim(cl_yrange(object)) +
  theme_void()



layout_asYouLike <- function(object){
  ActiveLayout(object) <- "init"
  object %>%
    set_gaps() %>%
    set_spacing() %>%
    tf_rotate(crosses = crosses) %>%
    tf_shift(crosses = crosses)
}

