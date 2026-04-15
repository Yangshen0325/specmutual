
# Closeness centrality is meaningful only for connected graphs. In disconnected graphs, consider
# using the harmonic centrality with harmonic_centrality()
# harmonic centrality measures the 'average' distance of a node to the other nodes in the network
# https://symbio6.nl/en/blog/analysis/harmonic-centrality
#
# Eigenvector centrality is meaningful only for (strongly) connected graphs. Undirected graphs that
# are not connected should be decomposed into connected components, and the eigenvector centrality
# calculated for each separately. This function does not verify that the graph is connected. If it is not,
# in the undirected case the scores of all but one component will be zeros.


# Degree centrality: direct link a node has (the sum of links a species has)

# Betweenness centrality: the number of shortest paths that pass through a node (how often a node can
# bridge in the shortest path between other nodes). e.g. node A1 being 0.25 means
# that A1 lies on 25% of the shortest paths between all other nodes in the network.
# In other words, if we were to consider all possible pairs of nodes and find the
# shortest path between them, A1 would be a part of a quarter of those shortest paths.

# Harmonic centrality: how close a node is to all other nodes. Higher values indicate that a node can
# reach other nodes in the network quite efficiently, with a high degree of “closeness” to the other nodes.

# components: the connected components of the graph.


library(igraph)


get_NLH_centra <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  # Process none, medium and high files and store the results
  centra_info <- lapply(file_names, getCentrality)

  names(centra_info) <- c("None", "Medium", "High")
  all_centra_df <- bind_rows(centra_info, .id = "Type")

  all_centra_df$Type <- factor(all_centra_df$Type, levels = c("None", "Medium", "High"))

  return(all_centra_df)

}


getCentrality <- function(dataset_name) {

  # Read data
  data <- readRDS(dataset_name)

  centrality_list <- lapply(data, function(rep_outputs) {

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # No poin in computing the centrality
    if(nrow(true_Mt) == 0 | ncol(true_Mt) == 0) {

      data.frame(
        plant_degree = NA,
        plant_degree_sd = NA,
        animal_degree = NA,
        animal_degree_sd = NA
        # plant_between = NA,
        # animal_between = NA,
        # plant_harmonic = NA,
        # animal_harmonic = NA
      )

    } else {

      # Assign row names as "p1", "p2", "p3", ...
      rownames(true_Mt) <- paste0("p", seq_len(nrow(true_Mt)))

      # Assign column names as "a1", "a2", "a3", ...
      colnames(true_Mt) <- paste0("a", seq_len(ncol(true_Mt)))

      # Convert it to igraph object
      g <- graph_from_biadjacency_matrix(true_Mt)

      # Degree centrality (the average of it)
      plant_degree<- mean(degree(g, v = V(g)$type == FALSE))# Plant degree centrality
      plant_degree_sd <- sd(degree(g, v = V(g)$type == FALSE))
      animal_degree <- mean(degree(g, v = V(g)$type == TRUE)) # Animal degree centrality
      animal_degree_sd <- sd(degree(g, v = V(g)$type == TRUE))

      # Betweenness centrality
      # plant_between <- mean(betweenness(g, v = V(g)$type == FALSE))
      # animal_between <- mean(betweenness(g, v = V(g)$type == TRUE))
      #
      # # Harmonic centrality
      # plant_harmonic <- mean(harmonic_centrality(g, vids = V(g)$type == FALSE))
      # animal_harmonic <- mean(harmonic_centrality(g, vids = V(g)$type == TRUE))

      data.frame(
        plant_degree = plant_degree,
        plant_degree_sd = plant_degree_sd,
        animal_degree = animal_degree,
        animal_degree_sd = animal_degree_sd
        # plant_between = plant_between,
        # animal_between = animal_between,
        # plant_harmonic = plant_harmonic,
        # animal_harmonic = animal_harmonic
      )
    }
  })

  # Combine together
  centra_df <- bind_rows(centrality_list)
  return(centra_df)

}




