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

usethis::use_data(edges, nodes, geneData, methData, mirData, overwrite = T)
