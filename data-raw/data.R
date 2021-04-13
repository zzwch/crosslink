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
                    replace = T, 15),
    target = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 15),
    type   = rep("Mir",15),
    cor    = sample(seq(0,1,by=0.05),15)
    ),
  # gene vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Gene"],
                    replace = T, 10),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 10),
    type=rep("Gene",10),
    cor    = sample(seq(0,1,by=0.01),10)),
  # meth vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Meth"],
                    replace = T, 8),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 8),
    type=rep("Meth",8),
    cor    = sample(seq(0,1,by=0.05),8)),
  # mir vs drug
  data.frame(
    source = sample(nodes$id[nodes$type == "Mir"],
                    replace = T, 5),
    target = sample(nodes$id[nodes$type == "Drug"],
                    replace = T, 5),
    type=rep("Drug",5),
    cor    = sample(seq(0,1,by=0.1),5))
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

n <- 4
set.seed(n)
demo1 <- gen_demo(n_cross = n,
                 n_node = sample(4:6, size = n, replace = T),
                 n_link = sample(4:6, size = n-1, replace = T),
                 seed = 1314)

demo2 <- list(edges = edges,
              nodes = nodes,
              geneData = geneData,
              methData = methData,
              mirData = mirData)

usethis::use_data(demo1, demo2, overwrite = T)
