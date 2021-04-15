# crosslink
Network Visualization Tailed for Grouped Nodes

## Install
```r
remotes::install_github("zzwch/crosslink", build_vignettes = TRUE)
```

## Quick start
```r
library(crosslink)

# generate a CrossLink object
cl <- crosslink(demo$nodes, demo$edges, demo$cross.by, odd.rm = F,spaces = "flank")

# set headers if needed
cl %<>% set_header(header = c("A","B","C","D","E","F"))

# plot the network

# By default, node color is coded and node size is proportional to its degree (calculated internally).
# And edge color is coded by the cross group of the edge's source node.
cl %>% cl_plot()
```

## Vignette
For more, see the [Vignette](https://zzwch.github.io/crosslink/articles/vignette.html)
