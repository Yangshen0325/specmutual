
library(igraph)


# Normalised the distribution, mean and sd --------------------------------
combine_distributions <- function(distributions) {

  # Get all unique degrees (sorted for clarity)
  all_degrees <- sort(as.numeric(unique(unlist(lapply(distributions, names)))))

  # Create a matrix to store each simulation's degree proportions
  #    Rows correspond to degrees; columns correspond to simulations
  dist_matrix <- matrix(0, nrow = length(all_degrees), ncol = length(distributions),
                        dimnames = list(all_degrees, NULL))

  # Fill the matrix with values from each simulation
  for (i in seq_along(distributions)) {
    dist <- distributions[[i]]
    for (deg in names(dist)) {
      # Convert degree to character to index the row name
      dist_matrix[as.character(deg), i] <- as.numeric(dist[[deg]])
    }
  }

  # Compute the mean and standard deviation for each degree across simulations
  degree_mean <- rowMeans(dist_matrix, na.rm = TRUE)
  degree_sd   <- apply(dist_matrix, 1, sd, na.rm = TRUE)

  # Combine the results into a single data frame
  degree_mean_sd <- data.frame(degree = all_degrees, mean = degree_mean, sd = degree_sd)

  return(degree_mean_sd)
}




# Get the  distribution --------------------------------------------



getDeg_dist <- function(dataset_name){

  # Read data
  data <- readRDS(dataset_name)

  deg_dist_list <- lapply(data, function(rep_outputs) {

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    # This is the matrix on the island
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # No point in dealing with 0 * X or X *0
    if(nrow(true_Mt) == 0 | ncol(true_Mt) == 0) {

      deg_dist = list(plant_deg_dist = NA,
                      animal_deg_dist = NA)

    } else {

      # Assign row names as "p1", "p2", "p3", ...
      rownames(true_Mt) <- paste0("p", seq_len(nrow(true_Mt)))

      # Assign column names as "a1", "a2", "a3", ...
      colnames(true_Mt) <- paste0("a", seq_len(ncol(true_Mt)))

      # Convert it to igraph object
      g <- graph_from_biadjacency_matrix(true_Mt)

      # Degree for each species. plant is FALSE, animal is TRUE
      # which could also use `plant_deg_dist <- degree_distribution(g, v = V(g)$type == FALSE)`, same output.
      # With `table` I could read the column names.
      plant_degrees <- degree(g, v = V(g)$type == FALSE)
      animal_degrees <- degree(g, v = V(g)$type == TRUE)

      # Normalize the degree distribution, e.g. what's the proportion of species has 0 links, 1 links, etc.
      plant_deg_dist <- table(plant_degrees) / length(plant_degrees)
      animal_deg_dist <- table(animal_degrees) / length(animal_degrees)

      deg_dist = list(plant_deg_dist = plant_deg_dist,
                             animal_deg_dist = animal_deg_dist)

    }
})


    # First sublist is plant data, second sublist is animal data
    plant_deg_dist <- lapply(deg_dist_list, '[[', 1)
    animal_deg_dist <- lapply(deg_dist_list, '[[', 2)

    degree_p <- combine_distributions(distributions = plant_deg_dist)
    degree_a <- combine_distributions(distributions = animal_deg_dist)

   return(list(degree_p = degree_p,
               degree_a = degree_a))
}


# get_NLH_degree ----------------------------------------------------------
get_NLH_degree <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  Type <- c("None", "Medium", "High")

  # Process none, medium and high files and store the results
  degree_all <- lapply(file_names, getDeg_dist)

  # For each result, extract degree_p and add a label for the mutualism effect
  degree_dist_p <- lapply(seq_along(degree_all), function(i) {
    df <- degree_all[[i]]$degree_p  # data frame with columns: degree, mean, sd
    df$Effects <- Type[i]
    return(df)
  })

  # Similarly, for the animal
  degree_dist_a <- lapply(seq_along(degree_all), function(i) {
    df <- degree_all[[i]]$degree_a
    df$Effects <- Type[i]
    return(df)
  })

  degree_dist_p <- do.call(rbind, degree_dist_p)
  degree_dist_a <- do.call(rbind, degree_dist_a)

  return(list(degree_dist_p = degree_dist_p,
              degree_dist_a = degree_dist_a))

}

















