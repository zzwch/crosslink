
# crosslink

<!-- badges: start -->
<!-- badges: end -->

The goal of crosslink is to visualize the network of grouped nodes

## Installation

You can install the released version of crosslink from [github](https://github.com/zzwch/crosslink) with:

``` r
install.packages("crosslink")
```

## Example

This is a basic example which shows you how to solve a common problem:
including the basic function and expand usage of crosslink packages.

### 1. Generate test grouped nodes data 

Crossproject need nodes (must have two columns: node name and node type), edges data (must have two columns: source node and target node). 
Crosslink provide the function 'gen_demo' to generate demo data. 
Users can optional define n to generate the specific numbered grouped data.

``` r
# library R packages
library(crosslink)
library(ggplot2)
library(magrittr)
library(rlang)
library(dplyr)
library(aplot)
library(reshape)

n <- 6
demo <- gen_demo(n_cross = n,n_node = 1:n, n_link = 1:(n-1), seed = 66)
nodes <- demo$nodes
head(nodes)
edges <- demo$edges
head(edges)
cross.by <- demo$cross.by
head(demo)

```

### 2. 'crosslink' function to generate crosslink object data for plot

crosslink can help to generate an object of class CroosLink for plot.
users can define 'odd.rm' to choose if remove the nodes have zero relationship with any other nodes 

``` r
cl <- crosslink(nodes, edges, cross.by, odd.rm = F,spaces = "flank")

cl %>% get_cross()   # get node information
cl %>% get_link()    # get edges information
cl %>% cl_xrange()   # get xrange of crosslink object 
cl %>% cl_yrange()   # get yrange of crosslink object 
cl %>% cl_layouts()  # get layouts information of crosslink object
cl %>% cl_active()   # get active layouts information of crosslink object
# Header can be customized through 'set_header'
set_header(cl,header = c("A","B","C","D","E","F")) -> cl
cl %>% get_header()  # get header of crosslink object 

```


### 3. 'cl_plot' function combined ggplot2 output relational graph simply

crosslink meet the needs of directly generating network diagrams 

``` r

# define theme
theme_use <- theme(legend.position = "none",
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   panel.background = element_blank())


# defalut layout by column; 
# use object to generate figure directly

cl %>% cl_plot() -> p1
p1

## Beautify nodes, lines through the interface 'cross', 'link','label' and 'header'.
## Beautify the figure as ggplot2 theme provided. 
cl %>% 
   cl_plot(cross = list(mapping = aes(color = type,shape=type),
                        scale   = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                     shape = scale_shape_manual(values = 15:22)
                                    ),
                        size    = 10
                       ),
          link   = list(color="grey55",size=1,linetype=3),
          label  = list(color="white"),
          header = list(mapping = aes(color= cross),
                        scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                        size=5
                       ),
          add    = theme_use)
                       
```

### 4. Crosslink offers several layout transformation options

#### a. layout by row
``` r

cl %>% layout_row() %>%
   cl_plot(cross = list(mapping = aes(color  = type,fill=type),
                        scale   = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                        fill = scale_fill_manual( values = RColorBrewer::brewer.pal(8, "Dark2"))),
                        size=8,shape=24
                       ),
          link   = list(color=RColorBrewer::brewer.pal(8, "Set2")[7],size=1.5,linetype=12),
          label  = list(color="white"),
          header = list(mapping = aes(color= cross),
                        scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                        size=5
                       ),
          add=theme_use)

```


#### b. layout by rotate
``` r
cl %>% 
   tf_rotate(angle = 30, by.each.cross = T) %>%
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=16
                       ),
          link = list(color="grey75",size=1,linetype=15),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ),
          add=theme_use)
          
```

#### c. layout by polygon
``` r
cl %>% layout_polygon() %>%
   #tf_rotate(angle = 45, by.each.cross = F) %>%  # If the grouping is 4, rotate by this parameter and change from diamond to square 
   cl_plot(cross = list(mapping = aes(color = type),
                        scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                        size=10,shape=16
                       ),
           link  = list(color="orange",size=2,linetype=15,geom="curve"),
           label = list(color="white"),
           header= list(mapping = aes(color= cross),
                        scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                        size=5
                        ),
           add  =  theme_use )

```

#### d. layout by hive

``` r
cl %>% layout_hive(angles=rep(60,6)) %>% #length of angles must be same with the numbers of groups
       cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=18
                       ),
               link  = list(color="purple",size=1,linetype=15),
               label = list(color="white"),
               header= list(mapping = aes(color= cross),
                            scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                            size=5
                            ),
               add   = theme_use )

```

#### e. layout by arc

``` r
cl %>% layout_arc(angles=c(60,120,180),crosses=c("D","E","F")) %>% #length of angles must be same with the numbers of crosses
       cl_plot(cross = list(mapping = aes(color = type),
                            scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                            size=10,shape=18
                           ),
               link  = list(color="orange",size=1,linetype=3),#,geom="curve"
               label = list(color="white"),
               header= list(mapping = aes(color= cross),
                            scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                            size=5
                           ),
               add   =  theme_use)

```

#### e. layout by combination of several methods

``` r

# demo data
n <- 5
demo <- gen_demo(n_cross = n, n_node = rep(n, n), n_link = rep(n, n-1), seed = 66)
nodes <- demo$nodes
edges <- demo$edges
cross.by <- demo$cross.by


cl <- crosslink(nodes, edges, cross.by, odd.rm = F)
cl <- set_header(cl,header = c("A","B","C","D","E"))

cl %>% 
   layout_polygon(crosses = c("A","B","C","D")) %>%
   tf_rotate(crosses=c("A","B","C","D"),angle = rep(45,4)) %>%
   tf_shift(x=0.8*(-1),y=0.05,crosses="E",layout="default") %>%
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=19
                       ),
          link   = list(color="grey75",size=1,linetype=3),
          label  = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ),
           add   =  theme_use)
           
```

### 5. Example1: for multi columns data

``` r
source("R/example1.R")

cl <- crosslink(eg_nodes, eg_edges, cross.by="type", odd.rm = F,)

cl %>% cl_plot(cross = list(mapping = aes(color = type, shape = type,size=strength,fill=type),
                            scale   = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                           fill = scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                           shape = scale_shape_manual(values = c(15:18,25)),
                                           size=scale_size_continuous(range=c(1,8))
                                           )
                           ),
               link  = list(mapping = aes(color = maintype),size=0.5,linetype=8,
                            scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2")))),
               label = list(color="black"),
               add   =  theme_use)
               
```


### 6. Example2: for complex data

``` r

source("R/data.R")

theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) ->theme_use2


## crosslink project
cl <- crosslink(nodes,edges,cross.by="type")

cl %>% cl_plot()

cl <- set_header(cl,header=unique(get_cross(cl)$cross))

cl %>% layout_polygon(crosses = c("Mir","Meth","Gene","Drug"),layout_based = "default") %>%
       tf_rotate(crosses= c("Mir","Meth","Gene","Drug"),angle = rep(45,4),layout="default") %>%
       tf_shift(x=0.2*(-1),y=1.5,crosses=c("Mir","Meth"),layout="default") -> cl


# plot annotation
top <- nodes$id[nodes$type == "Mir"] # set the order as you like
bottom <- nodes$id[nodes$type == "Gene"] # set the order as you like
right <- nodes$id[nodes$type == "Meth"] # set the order as you like

# Top plot
topAnn <-  mirData %>%
  mutate(mir_f = factor(mir, top)) %>%
  ggplot(mapping = aes(x=mir,y=-lfc)) +
  geom_bar(fill = "#E7298A",
           stat = "identity",
           width = 0.5) + # 间距跟bar同宽
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_use2
topAnn

# Bottom plot
botAnn <- geneData %>%
  mutate(meth_f = factor(gene, bottom)) %>%
  ggplot(mapping = aes(x=gene,y=-lfc)) +
  geom_bar(fill =  RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)][2],
           stat = "identity",
           width = 0.5) +
  labs(x = NULL, y = "Difference") +
  theme_use2
botAnn

# right plot
rgtAnn <- methData %>%
  mutate(mir_f = factor(meth, right)) %>%
  ggplot(mapping = aes(x=meth,y=-lfc)) +
  geom_bar(fill = RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)][3],
           stat = "identity", 
           width = 0.5) +
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_use2 +
  coord_flip()

rgtAnn

# left plot
mat = matrix(sample(1:100, 64, replace = T), nrow = 8)
colnames(mat)=as.character(cl@cross$Drug)
ggplot(data = melt(mat), aes(X1, X2, fill = value))+
     geom_tile(color = "white")+ 
     scale_fill_gradient2(low = "blue", high = "darkgreen", mid = "white",midpoint = 0) + 
     xlab("")+ylab("")+theme_classic()+
     theme(legend.position = "right",
          axis.line = element_blank(),
          axis.ticks = element_blank(),
           axis.text = element_blank()) -> lftAnno


cl_plot(cl,
       annotation=cl_annotation(top = topAnn,top.by = "Mir",top.height = 0.5,
                                bottom = botAnn,bottom.by = "Gene",bottom.height = 0.5,
                                right = rgtAnn,right.by ="Meth",right.width = 0.5,
                                left = lftAnno,left.by = "Drug" ,left.width = 0.5),
       cross = list( mapping = aes(color = type,size=degree,shape=type),
       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)]),
       shape = scale_shape_manual(values = 15:23),
       size  = scale_size_continuous(range=c(1,5)))),
       link  = list(mapping = aes(color = type),
                    scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2")[c(1:5,7:9)])),linetype=1,size=1),
       label = list(color="black"
                    ,nudge_x=c(rep(-1,8),rep(0,10),rep(1,10),rep(0,6))
                    ,nudge_y=c(rep(0,8),rep(-1,10),rep(0,10),rep(1,6))),
           add   =  theme_use)


```

