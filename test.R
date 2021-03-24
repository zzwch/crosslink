library(crosslink)
#library(ggplot2)
library(magrittr)

rm(list = ls())

n <- 6
demo <- gen_demo(n_cross = n, n_node = 1:n, n_link = 1:(n-1), seed = 66)
nodes <- demo$nodes
edges <- demo$edges
cross.by <- demo$cross.by

object <- crosslink(nodes, edges, cross.by, odd.rm = F, spaces = "flank")
#########
object %>%
  set_header(header = c("header1", "header2", "header3", "header4")) %>%
  set_space(spaces = list(B = "partition", C = "equal", D = c(0.1, 0.2, 0.3, 0.2, 0.1))) %>%
  layout_row(crosses = c("A", "B", "C"), layout_save = "custom") %>%
  tf_shift(x = 6, y = -1.5, crosses = c("A", "B", "C"), relative = F) %>%
  #layout_hive() %>%
  #layout_polygon() %>%
  #layout_arc(angles = seq(180, 30, length.out = 6)) %>%
  #tf_rotate(angle = 45, by.each.cross = F) %>%
  cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(#color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral")),
                                    shape = scale_shape_manual(values = 1:7))),
          link = list(geom = "segment", curvature = 0.5),
          label = list(mapping = aes(color = type), color = "black", nudge_y = -0.2,
                       scale = list(
                         #color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Spectral"))
                         ))
          , annotation = cl_annotation(p1, "B",1,
                                       p1, "C",1,
                                       p1, "D",1,
                                       p1, "F",1)) %>%
  #cl_void() %>%
  print()

library(gridExtra)
arrangeGrob(grobs = list(NULL, top, NULL, left, center, right, NULL, bottom, NULL))

library(aplot)
p1 %>% insert_left(p2) %>% insert_right(p3) %>% insert_top(p4) %>% insert_bottom(p1)

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

library(tidyverse)

df <- data.frame(x = c(1, 2, 3), y = 1)
df2 <- data.frame(x = factor(c("A", "B", "C")), y = 1)

ggplot() +
  geom_bar(mapping = aes(x, y), stat = "identity", data = df) -> p1
ggplot() +
  geom_bar(mapping = aes(x, y), stat = "identity", data = df2) -> p2

ggplot_build(p1) -> p1b
ggplot_build(p2) -> p2b

ggplot_gtable(p1b) -> p1t


x <- 2
(p2+ scale_x_discrete(expand = expansion(add = x))) %>% ggplot_build() -> tmp; tmp$layout$panel_params[[1]]$x.range
(p2+ scale_x_discrete(expand = expansion(add = c(2, 6))))
