# generate data
# nodes
eg_nodes <- data.frame(
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
eg_edges <- data.frame(rbind(
  # Positive pathway vs RBP
  data.frame(
    source = sample(eg_nodes$id[eg_nodes$type == "P1" | eg_nodes$type == "P2"],
                    replace = T, 100),
    target = sample(eg_nodes$id[eg_nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("P",100)),
  # Negative pathway vs RBP
  data.frame(
    source = sample(eg_nodes$id[eg_nodes$type == "N1" | eg_nodes$type == "N2"],
                    replace = T, 100),
    target = sample(eg_nodes$id[eg_nodes$type == "RBP"],
                    replace = T, 100),
    maintype=rep("N",100))
))

eg_nodes$type=factor(eg_nodes$type,levels=unique(eg_nodes$type))

usethis::use_data(eg_edges, eg_nodes, overwrite = T)

