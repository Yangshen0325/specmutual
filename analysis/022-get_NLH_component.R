
# get the none, medium, and high mutualism data
get_NLH_cmpnt <- function(the_path, the_case) {

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))


  # Process none, medium and high files and store the results
  cmpnt_info <- lapply(file_names, get_cmpnt)

  names(cmpnt_info) <- c("None", "Medium", "High")
  all_cmpnt_df <- bind_rows(cmpnt_info, .id = "Type")

  all_cmpnt_df$Type <- factor(all_cmpnt_df$Type, levels = c("None", "Medium", "High"))

  return(all_cmpnt_df)

}


# compute `components` for one dataset

get_cmpnt <- function(dataset_name) {

  # Read data
  data <- readRDS(dataset_name)

  cmpnt_list <- lapply(data, function(rep_outputs){

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    # This is the matrix on the island
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # Convert it to igraph object
    g <- graph_from_biadjacency_matrix(true_Mt)

    cmpnt <- components(g)
    cmpnt_sizes <- as.vector(cmpnt$csize)
    largest_cmpnt <- max(cmpnt_sizes)
    n_components <- cmpnt$no

    return(list(
      n_components = n_components,
      largest_cmpnt = largest_cmpnt
      #cmpnt_sizes = cmpnt_sizes
    ))
  })


   cmpnt_df <- data.frame(
    n_components = unlist(lapply(cmpnt_list, "[[", 1)),
    largest_component = unlist(lapply(cmpnt_list, "[[", 2))
  )
   return(cmpnt_df)

}
