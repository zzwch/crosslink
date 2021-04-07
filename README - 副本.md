
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
(写完回来补充，增加超链接)
Basic functions: 
Expand usage:

### 1. Generate test grouped nodes data 

Crossproject need nodes (must have two columns: node name and node type), edges data (must have two columns: source node and target node). 
Crosslink provide the function 'gen_demo' to generate demo data. 
Users can optional define n to generate the specific numbered grouped data.

``` r
library(crosslink)
library(ggplot2)
library(magrittr)
library(rlang)

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

# defalut layout by column; 
# use object to generate figure directly

cl %>% cl_plot() -> p1
print(p1)

## Beautify nodes, lines through the interface 'cross', 'link','label' and 'header'.
## Beautify the figure as ggplot2 theme provided. 
cl %>% 
   cl_plot(cross = list(mapping = aes(color = type,shape=type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                       shape = scale_shape_manual(values = 15:22)
                       ),
                       size=10
                       ),
          link = list(color="grey75",size=1,linetype=11),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())

```

### 4. Crosslink offers several layout transformation options

#### a. layout by row
``` r
## set_space 设置后在图上看不出区别,不然去掉？？？
cl %>% #set_space(spaces=list(crossF="partition")) %>%
   layout_row() %>%
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))
                       #,shape = scale_shape_manual(values = rev(15:20))
                       ),
                       size=10,shape=16
                       ),
          link = list(color="grey75",size=1,linetype=12),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
#p1

p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())

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
                       ))  -> p1
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())
          
          
```

#### c. layout by polygon
``` r
cl %>% 
   #set_header(header = c("s1","tar1","s2","tar2","s3","tar3")) %>%
   layout_polygon() %>%
   #tf_rotate(angle = 45, by.each.cross = F) %>%  # If the grouping is 4, rotate by this parameter and change from diamond to square 
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=16
                       ),
          link = list(color="orange",size=1,linetype=10,geom="curve"),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
                       
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())

```

#### d. layout by hive

``` r
cl %>% 
   #set_header(header = c("s1","tar1","s2","tar2")) %>%
   layout_hive(angles=rep(30,3),crosses=c("D","E","F")) %>% #length of angles must be same with the numbers of groups
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=18
                       ),
          link = list(color="purple",size=1,linetype=15),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
                       
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())

```

#### e. layout by arc

``` r
cl %>% 
   #set_header(header = c("s1","tar1","s2","tar2")) %>%
   layout_arc(angles=rep(30,3),crosses=c("D","E","F")) %>% #length of angles must be same with the numbers of crosses
   cl_plot(cross = list(mapping = aes(color = type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=10,shape=18
                       ),
          link = list(color="orange",size=1,linetype=3),#,geom="curve"
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
                       
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())

```

#### e. layout by combination of several methods

``` r
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
          link = list(color="grey75",size=1,linetype=3),
          label = list(color="white"),
          header = list(mapping = aes(color= cross),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2"))),
                       size=5
                       ))  -> p1
p1
                      
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())->p1

p1          
```

### 5. Example1: for multi columns data

``` r
# generate data 
# nodes
nodes <- data.frame(
  id = c(paste0("P", 1:30),
         paste0("RBP", 1:20),
         paste0("N", 1:30)),
  type = c(rep("P1", 15),rep("P2", 15),
           rep("RBP", 20),
           rep("N1", 15),rep("N2", 15)),
  maintype=c(rep("P", 30),
           rep("RBP", 20),
           rep("N", 30)),
  strength=sample(3:10, 80, replace = T)
)

# edges
edges <- data.frame(rbind(
  # Positive pathway vs RBP
  data.frame(
    source = sample(nodes$id[nodes$type == "P1" | nodes$type == "P2"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("P",100)),
  # Negative pathway vs RBP
  data.frame(
    source = sample(nodes$id[nodes$type == "N1" | nodes$type == "N2"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("N",100))
))

nodes$type=factor(nodes$type,levels=unique(nodes$type))

cl <- crosslink(nodes, edges, cross.by="type", odd.rm = F,)

cl %>% cl_plot(cross = list(mapping = aes(color = type, shape = type,size=strength,fill=type),
                       scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                    fill = scale_fill_manual(values = RColorBrewer::brewer.pal(8, "Dark2")),
                                    shape = scale_shape_manual(values = c(15:18,25)),
                                    size=scale_size_continuous(range=c(1,8))
                                    )
                       ),
               link = list(mapping = aes(color = maintype),size=0.5,linetype=8,
                            scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2")))),
               label = list(color="black"))-> p1

                       
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank()) -> p1
          
p1

```


### 6. Example2: for complex data

``` r
library(dplyr)
library(aplot)
library(reshape)
library(pacman)
pacman::p_load(tidyverse,ggtree,aplot,reshape,ggExtra)

## generate data
# nodes
nodes <- data.frame(
  id = c(paste0("Gene", 1:10), paste0("Meth", 1:10),
         paste0("Mir", 1:6), paste0("Drug", 1:8)),
  type = c(rep("Gene", 10), rep("Meth", 10), 
           rep("Mir", 6), rep("Drug", 8))
  )

# edges
edges <- data.frame(rbind(
  # gene vs mir
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 30),
    target = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 30),
    type=rep("Gene",30)),
  # gene vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100),
    type=rep("Gene",100)),
  # meth vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Meth"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100),
    type=rep("Meth",100)),
  # mir vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 20),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 20),
    type=rep("Drug",20))
))


# gene annotation
geneData <- data.frame(
  gene = nodes$id[nodes$type == "Gene"],
  lfc = runif(length(nodes$id[nodes$type == "Gene"]), 0, 6))


# meth annotation
methData <- data.frame(
  meth = nodes$id[nodes$type == "Meth"],
  lfc = runif(length(nodes$id[nodes$type == "Meth"]), -0.5, -0.2))


# mir annotation
mirData <- data.frame(
  mir = nodes$id[nodes$type == "Mir"],# set the order as you want
  lfc = runif(length(nodes$id[nodes$type == "Mir"]), -4,0))

## crosslink project
cl<-crosslink(nodes,edges,cross.by="type")

cl %>% cl_plot()
cl <-set_header(cl,header=unique(get_cross(cl)$cross))

cl %>% 
     layout_polygon(crosses = c("Mir","Meth","Gene","Drug"),layout_based = "default") %>%
     tf_rotate(crosses= c("Mir","Meth","Gene","Drug"),angle = rep(45,4),layout="default") %>%
     tf_shift(x=0.2*(-1),y=1.5,crosses=c("Mir","Meth"),layout="default") %>%
     cl_plot(cross = list( mapping = aes(color = type,size=degree,shape=type),
                           scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)]),
                           shape = scale_shape_manual(values = 15:23),
                           size = scale_size_continuous(range=c(1,8)))),
              link = list(color="grey75",linetype=6),
              label = list(color="black"))->p1

p1+theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank()) ->p1
p1 

top <- nodes$id[nodes$type == "Mir"] # set the order as you like
bottom <- nodes$id[nodes$type == "Gene"] # set the order as you like
right <- nodes$id[nodes$type == "Meth"] # set the order as you like



topAnn <-  mirData %>%
  mutate(mir_f = factor(mir, top)) %>%
  ggplot(mapping = aes(x=mir,y=-lfc)) +
  geom_bar(fill = "#E7298A",
           stat = "identity",
           width = 0.5) + # 间距跟bar同宽
  geom_text(mapping = aes(label = mir), angle = 90, hjust =-lfc, fontface ="italic") +
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
topAnn

# Bottom plot
botAnn <- geneData %>%
  mutate(meth_f = factor(gene, bottom)) %>%
  ggplot(mapping = aes(x=gene,y=-lfc)) +
  geom_bar(fill =  RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)][2],
           stat = "identity",
           width = 0.5) +
  geom_text(mapping = aes(label = gene), angle = 90, hjust =-0.2, fontface ="italic") +
  labs(x = NULL, y = "Difference") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
botAnn

# left plot
lftAnn <- methData %>%
  mutate(mir_f = factor(meth, left)) %>%
  ggplot(mapping = aes(x=meth,y=-lfc)) +
  geom_bar(fill = RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)][3],
           stat = "identity", 
           width = 0.5) +
  geom_text(mapping = aes(y = 0,label = meth), 
            #fontface ="italic", 
            vjust = -1, 
            hjust = 1) +
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()

lftAnn
mat = matrix(sample(1:100, 64, replace = T), nrow = 8)
colnames(mat)=as.character(cl@cross$Drug)

ggplot(data = melt(mat), aes(X1, X2, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0) +xlab("")+ylab("")+
  theme_minimal()+ coord_fixed() + theme(legend.position = "left")->p2
 

library(cowplot)
plot_grid(NULL,topAnn,NULL,p2,p1,lftAnn,NULL,botAnn,NULL,ncol = 3)

    

```

### 6. Example2: for complex data version2

``` r
library(dplyr)
library(aplot)
library(reshape)
library(pacman)
pacman::p_load(tidyverse,ggtree,aplot,reshape,ggExtra)

## generate data
# nodes
nodes <- data.frame(
  id = c(paste0("Gene", 1:10), paste0("Meth", 1:10),
         paste0("Mir", 1:6), paste0("Drug", 1:8),
         paste0("Tar", 1:8), paste0("Path", 1:6)),
  type = c(rep("Gene", 10), rep("Meth", 10), 
           rep("Mir", 6), rep("Drug", 8),
           rep("Tar", 8), rep("Path", 6))
  )

# edges
edges <- data.frame(rbind(
  # gene vs mir
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 30),
    target = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 30),
    type=rep("Gene",30)),
  # gene vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100),
    type=rep("Gene",100)),
  # meth vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Meth"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 100),
    type=rep("Meth",100)),
  # mir vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 20),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 20),
    type=rep("Drug",20)),
  
  # drug vs target
  data.frame(
    source = nodes$id[nodes$type == "Drug"],
    target = nodes$id[nodes$type == "Tar"],
    type=rep("Drug",length(nodes$id[nodes$type == "Drug"]))
    ),
  # target vs path
  data.frame(
    source = sample(nodes$id[nodes$type == "Tar"],
                    replace = T, 15),
    target = sample(nodes$id[nodes$type == "Path"],
                    replace = T, 15),
    type=rep("Drug",15))
))


# gene annotation
geneData <- data.frame(
  gene = nodes$id[nodes$type == "Gene"],
  lfc = runif(length(nodes$id[nodes$type == "Gene"]), 0, 6))


# meth annotation
methData <- data.frame(
  meth = nodes$id[nodes$type == "Meth"],
  lfc = runif(length(nodes$id[nodes$type == "Meth"]), -0.5, -0.2))


# mir annotation
mirData <- data.frame(
  mir = nodes$id[nodes$type == "Mir"],# set the order as you want
  lfc = runif(length(nodes$id[nodes$type == "Mir"]), -4,0))

## crosslink project
cl<-crosslink(nodes,edges,cross.by="type")

cl %>% cl_plot()
cl <-set_header(cl,header=unique(get_cross(cl)$cross))

cl %>% 
     layout_polygon(crosses = c("Mir","Meth","Gene","Drug"),layout_based = "default") %>%
     tf_rotate(crosses= c("Mir","Meth","Gene","Drug"),angle = rep(45,4),layout="default") %>%
     tf_shift(x=0.2*(-1),y=0.65,crosses=c("Mir","Meth"),layout="default") %>%
     tf_shift (x=0.6*(-1),y=0.05,crosses="Tar",layout="default") %>%
     tf_shift (x=0.4*(-1),y=0.05,crosses="Path",layout="default") %>%
     cl_plot(cross = list( mapping = aes(color = type,size=degree,shape=type),
                           scale = list(color = scale_color_manual(values = RColorBrewer::brewer.pal(8, "Dark2")[c(1:5,7:9)]),
                           shape = scale_shape_manual(values = 15:23),
                           size = scale_size_continuous(range=c(1,8)))),
              link = list(color="grey75",linetype=6),
              label = list(color="black"))->p1

p1+theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank()) ->p1
p1 

top <- nodes$id[nodes$type == "Gene"] # set the order as you like
bottom <- nodes$id[nodes$type == "Meth"] # set the order as you like
left <- nodes$id[nodes$type == "Mir"] # set the order as you like
right <- nodes$id[nodes$type == "Drug"] # set the order as you like

 
          
topAnn <- geneData %>%
  mutate(gene_f = factor(gene, top)) %>%
  ggplot(mapping = aes(x=gene,y=-lfc)) +
  geom_bar(fill = "red",
           stat = "identity",
           width = 0.5) + # 间距跟bar同宽
  geom_text(mapping = aes(label = gene), angle = 90, hjust =-0.2, fontface ="italic") +
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
topAnn

# Bottom plot
botAnn <- methData %>%
  mutate(meth_f = factor(meth, bottom)) %>%
  ggplot(mapping = aes(x=meth,y=-lfc)) +
  geom_bar(fill = "blue",
           stat = "identity",
           width = 0.5) +
  #geom_text(mapping = aes(label = gene), angle = 90, hjust =-0.2, fontface ="italic") +
  labs(x = NULL, y = "Difference") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())
botAnn

# left plot
lftAnn <- mirData %>%
  mutate(mir_f = factor(mir, left)) %>%
  ggplot(mapping = aes(x=mir,y=-lfc)) +
  geom_bar(fill = "red",
           stat = "identity", 
           width = 0.5) +
  geom_text(mapping = aes(y = 0,label = mir), 
            #fontface ="italic", 
            vjust = -1, 
            hjust = 1) +
  labs(x = NULL, y = "log2(Fold Change)") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()

lftAnn
mat = matrix(sample(1:100, 36, replace = T), nrow = 6)
colnames(mat)=as.character(cl@cross$Path)

ggplot(data = melt(mat), aes(X1, X2, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0) +
  theme_minimal()+ coord_fixed()->p2
 

library(cowplot)
plot_grid(NULL,botAnn,NULL,p2,p1,lftAnn,NULL,topAnn,NULL,ncol = 3)


grid.arrange('',botAnn,'',p1,p1,lftAnn,'',topAnn,'',nrow = 3)   

```

### others

``` r

library(aplot)
library(reshape)
library(pacman)
pacman::p_load(tidyverse,ggtree,aplot,reshape,ggExtra)

# generate data 
# nodes
nodes <- data.frame(
  id = c(paste0("P", 1:30),
         paste0("RBP", 1:20),
         paste0("N", 1:30)),
  type = c(rep("P1", 15),rep("P2", 15),
           rep("RBP", 20),
           rep("N1", 15),rep("N2", 15)),
  maintype=c(rep("P", 30),
           rep("RBP", 20),
           rep("N", 30)),
  strength=sample(3:10, 80, replace = T)
)

# edges
edges <- data.frame(rbind(
  # Positive pathway vs RBP
  data.frame(
    source = sample(nodes$id[nodes$type == "P1" | nodes$type == "P2"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("P",100)),
  # Negative pathway vs RBP
  data.frame(
    source = sample(nodes$id[nodes$type == "N1" | nodes$type == "N2"],
                    replace = T, 100),
    target = sample(nodes$id[nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("N",100))
))

nodes$type=factor(nodes$type,levels=unique(nodes$type))

cl <- crosslink(nodes, edges, cross.by="type", odd.rm = F,)

cl %>% cl_plot(cross = list(mapping = aes(color = type, shape = maintype,size=strength),
                       scale = list(color = 'scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set1"))',
                                    shape = "scale_shape_manual(values = 15:22)",
                                    size='scale_size_continuous(range=c(1,6))'
                                    ),
                       ),
               link = list(mapping = aes(color = maintype),scale = list(color = 'scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set2"))')),
               label = list(color="black"))-> p1

                       
p1 + theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank()) -> p1


## aplot need the same xrange or yrange for combination

bar_data<-data.frame(id=c(-2,2,6,10),value=c(-15,-5,-10,-10))
ggplot(bar_data,aes(x=id,y=value))+geom_line()+theme_classic() -> p2
bar_data<-data.frame(id=c(-2,2,6,10),value=c(5,5,12,15))
ggplot(bar_data,aes(x=id,y=value))+geom_point()+theme_classic() -> p4


## generate heatmap
pacman::p_load(tidyverse,ggtree,aplot,reshape,ggExtra)

d <- matrix(rnorm(16), ncol=4) %>% data.frame()
rownames(d) <- paste0('D', 4:1)
colnames(d) <- paste0('t', 1:4)
hc <- hclust(dist(d))  
hcc <- hclust(dist(t(d))) 
phr <- ggtree(hc)  #行聚类树
phc <- ggtree(hcc) + layout_dendrogram() #列聚类树

#d <- data.frame(d)

d$gene <- rownames(d) 
dd <- melt(d)

p <- ggplot(dd, aes(variable,gene,fill=value)) + geom_tile() + 
  scale_fill_viridis_c() +
  scale_y_discrete(position="right") +
  theme_minimal() + 
  xlab(NULL) + ylab(NULL) 


y <- seq(-15,-5,by=3)
x <- seq(-15,-5)
data <- expand.grid(X=x, Y=y)
data$Z <- runif(44, 0, 5)

ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() + theme(legend.position = "none",
          axis.title = element_blank(),
          plot.margin = margin(rep(30,4)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())->p3


ggarrange('',botAnn,'','',p1,lftAnn,'',topAnn,'',ncol = 3,nrow = 3,common.legend = T)

```



