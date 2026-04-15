

#### Get degree and corresponding rank under none, medium, high effects for one scenario
get_NLH_deg_rank <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  Type <- c("None", "Medium", "High")

  # Process none, medium and high files and store the results
  degree_rank_all <- lapply(file_names, getDeg_rank)

  degree_rank_p <- lapply(seq_along(degree_rank_all), function(i) {
    df <- degree_rank_all[[i]]$plant_deg_rank
    df$Effects <- Type[i]
    return(df)
  })

  degree_rank_a <- lapply(seq_along(degree_rank_all), function(i) {
    df <- degree_rank_all[[i]]$animal_deg_rank
    df$Effects <- Type[i]
    return(df)
  })

  degree_rank_p <- do.call(rbind, degree_rank_p)
  degree_rank_a <- do.call(rbind, degree_rank_a)

  degree_rank_p$Effects <- factor(degree_rank_p$Effects, levels = c("None", "Medium", "High"))
  degree_rank_a$Effects <- factor(degree_rank_a$Effects, levels = c("None", "Medium", "High"))

  return(list(plant_deg_rank = degree_rank_p,
              animal_deg_rank = degree_rank_a))

}




#### Get the degree and the corresponding rank for one dataset

getDeg_rank <- function(dataset_name){

  # Read data
  data <- readRDS(dataset_name)

  deg_list <- lapply(data, function(rep_outputs) {

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    # This is the matrix on the island
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # No point in dealing with 0 * X or X *0
    if(nrow(true_Mt) == 0 | ncol(true_Mt) == 0) {

      list(plant_degree = NA,
           animal_degree = NA)

    } else {

      # Assign row names as "p1", "p2", "p3", ...
      rownames(true_Mt) <- paste0("p", seq_len(nrow(true_Mt)))

      # Assign column names as "a1", "a2", "a3", ...
      colnames(true_Mt) <- paste0("a", seq_len(ncol(true_Mt)))

      # Convert it to igraph object
      g <- graph_from_biadjacency_matrix(true_Mt)

      # Degree for each species. plant is FALSE, animal is TRUE

      plant_degree <- degree(g, v = V(g)$type == FALSE)
      animal_degree <- degree(g, v = V(g)$type == TRUE)

      list(plant_degree = plant_degree,
          animal_degree = animal_degree)

    }
  })

  # Get the plant degree and the corresponding rank
  plant_deg_list <- unlist(lapply(deg_list, '[[', 1))
  plant_deg_tb <- table(plant_deg_list)
  plant_deg_rank <- as.data.frame(plant_deg_tb)
  colnames(plant_deg_rank) <- c("Degree", "Count")
  plant_deg_rank$Degree <- as.numeric(as.character(plant_deg_rank$Degree))

  # Order by Count in descending order and assign ranks
  plant_deg_rank <- plant_deg_rank[order(-plant_deg_rank$Degree), ]
  plant_deg_rank$Rank <- seq_len(nrow(plant_deg_rank))


  # plant_deg_list <- unique(unlist(lapply(deg_list, '[[', 1)))
  # sorted_degree_p <- sort(plant_deg_list, decreasing = TRUE)
  # rank_plant <- 1:length(sorted_degree_p)
  # plant_deg_rank <- data.frame(degree = sorted_degree_p,
  #                            rank = rank_plant)

  # Get the animal degree and the corresponding rank
  animal_deg_list <- unlist(lapply(deg_list, '[[', 2))
  animal_deg_tb <- table(animal_deg_list)
  animal_deg_rank <- as.data.frame(animal_deg_tb)
  colnames(animal_deg_rank) <- c("Degree", "Count")
  animal_deg_rank$Degree <- as.numeric(as.character(animal_deg_rank$Degree))

  # Order by Count in descending order and assign ranks
  animal_deg_rank <- animal_deg_rank[order(-animal_deg_rank$Degree), ]
  animal_deg_rank$Rank <- seq_len(nrow(animal_deg_rank))

  return(list(plant_deg_rank = plant_deg_rank,
              animal_deg_rank = animal_deg_rank))
}
