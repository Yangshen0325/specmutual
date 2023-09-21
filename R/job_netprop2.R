library(bipartite)
library(igraph)
# connectance
compute_connectance <- function(network) {
  possible_links <- nrow(network) * ncol(network)
  real_links <- sum(network > 0)
  connectance <-  real_links / possible_links
  return(connectance)
}

#Species richness, PA ratio, modularity, nestedness
compute_all <- function(network) {
  binary_network <- ifelse(network > 0, 1, 0)
  paratio <- dim(binary_network)[1]/dim(binary_network)[2]
  richness <- dim(binary_network)[1] + dim(binary_network)[2]
  modularity <- computeModules(binary_network)@likelihood
  nestedness <- nested(binary_network)
  connectance <- compute_connectance(network = binary_network)

  metrics <- data.frame(PAratio = paratio,
                        Richness = richness,
                        Modularity = modularity,
                        Nestedness = nestedness,
                        Connectance = connectance)

  return(metrics)
}

netset_files <- c("netset1.rds", "netset6.rds", "netset11.rds", "netset12.rds",
                  "netset17.rds", "netset22.rds")


for (netset_file in netset_files) {
  netset <- readRDS(paste0("~/netprop/", netset_file))
  results <- data.frame(Richness = numeric(length(netset)),
                        PAratio = numeric(length(netset)),
                        Connectance = numeric(length(netset)),
                        Modularity = numeric(length(netset)),
                        Nestedness = numeric(length(netset)))
  for (i in seq_along(netset)) {
    print(i)
    file <- netset[[i]]
    metrics <- compute_all(network = file)
    results[i, ] <- metrics
  }
  #outset_file <- paste0("~/netprop/out", gsub(".rds", "", netset_file), ".rds")
  outset_file <- paste0("~/netprop/out", gsub(".rds", "", netset_file), ".rds")
  saveRDS(results, file = outset_file)
}
