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

#### simple statistics of empirical data  #######
data <- read.csv("D:/Desktop/network_info.csv", header=TRUE)
boxplot(data$plant, data$animal, data$total,
        names=c("Plant", "Animal", "Total"),
        main="Distribution of Plant, Animal and Total",
        ylab="Count")
boxplot(data$plant, data$animal, data$total,
        names=c("Plant", "Animal", "Total"),
        main="Distribution of Plant, Animal and Total",
        ylab="Count", ylim=c(0, 300))
#################




