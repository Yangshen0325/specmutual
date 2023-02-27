# we have matrix in the mainland M0
library(igraph)

M0 <- matrix(1, ncol = 10, nrow = 10)
M0[lower.tri(M0)] <- 0
net <- graph_from_incidence_matrix(t(M0))
# V(net)$type
V(net)$color <- c("black", "green")[V(net)$type+1]
E(net)$width <- 3
plot(net,
     vertex.label=NA,
     vertex.size=10,
     layout=layout.bipartite,
     frame = TRUE)

plot(net)
