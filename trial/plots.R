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

#### Compute modularity, nestedness and connectance ####
library(bipartite)
library(igraph)

my_path <- "D:/PhD_Yang/literature/weboflife/web-of-life_MPL_allnet"
file_list <- list.files(path = my_path, pattern = "\\.csv", full.names = TRUE)

compute_network_metrics <- function(network) {
  binary_network <- ifelse(network > 0, 1, 0) # those networks are the
                                 #number of visiting observation

  modularity <- DIRT_LPA_wb_plus(binary_network)$likelihood
  nestedness <- nested(binary_network)
  ## connectance
  possible_links <- nrow(binary_network) * ncol(binary_network)
  real_links <- sum(binary_network)
  connectance <- possible_links/real_links
  # Some of them can be NULL
  if (is.null(modularity)) {
    metrics <- data.frame(Modularity = NA, Nestedness = nestedness, Connectance = connectance)
  } else if (is.null(nestedness)) {
    metrics <- data.frame(Modularity = modularity, Nestedness = NA, Connectance = connectance)
  } else if (is.null(connectance)) {
    metrics <- data.frame(Modularity = modularity, Nestedness = nestedness, Connectance = NA)
  } else {
    metrics <- data.frame(Modularity = modularity, Nestedness = nestedness, Connectance = connectance)
  }

  return(metrics)
}

results <- list()
for (file in file_list) {
  print(file)
  network <- read.csv(file, header = TRUE)
  metrics <- compute_network_metrics(network)
  results[[file]] <- metrics
}

#### plot data from island and mainland ####
library(ggplot2)
library(rnaturalearth) # can have a world map
library(readxl) # or save as .csv (I wanna see whole data)

data <- read_excel("D:/Desktop/network_info.xlsx")
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray80")+
  geom_point(data = data, aes(x = Longitude, y = Latitude, color = Type, shape = Type), size = 3) +
  scale_fill_manual(values = c("mainland" = "blue", "island" = "red")) +
  labs(title = "Observations",
       x = "Longitude",
       y = "Latitude",
       color = "Type") +
  theme_minimal()


