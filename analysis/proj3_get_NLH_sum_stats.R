



get_NLH_sum_stats <- function(the_path, the_case) {

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))


  # Summary statistics for each dataset
  sum_info_list <- lapply(file_names, get_NLH_summary_stats_single)

  sum_info <- bind_rows(sum_info_list, .id = "Type")
  # Add dataset names as a new column
  sum_info$Type <- rep(c("None", "Medium", "High"), each = 100)

  sum_info$Type <- factor(sum_info$Type, levels = c("None", "Medium", "High"))

  return(sum_info)

}



get_NLH_summary_stats_single <- function(dataset_name) {

  # read data
  data <- readRDS(dataset_name)

  # Extract summary information for each replicate
  sum_info_list <- lapply(data, function(rep) {

    # Number of plant species on the island
    island_p <-sum(rep[["status_p"]])
    # Number of animal species on the island
    island_a <- sum(rep[["status_a"]])

    # Extract species endemism info
    stt_table <- rep[["island"]][["stt_table"]]
    island_sp_emdemism <- stt_table[nrow(stt_table), ]

    island_endemic_p = island_sp_emdemism[3] + island_sp_emdemism[4]
    island_nonendemic_p = island_sp_emdemism[2]
    island_endemic_a = island_sp_emdemism[6] + island_sp_emdemism[7]
    island_nonendemic_a = island_sp_emdemism[5]

    # Network Connectance
    Mt <- rep$Mt
    status_p <- rep$status_p
    status_a <- rep$status_a
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]
    connectance <- cal_C(true_Mt)

    # Proportion of unconnected
    # the number of plant and animal species unconnected in network
    if (nrow(true_Mt) == 0) {
      disconnect_p <- NA
    } else {
      disconnect_p <- length(which(rowSums(true_Mt) == 0))
    }

    if (ncol(true_Mt) == 0) {
      disconnect_a <- NA
    } else {
      disconnect_a <- length(which(colSums(true_Mt) == 0))
    }

    # Component and size of the largest component
    g <- igraph::graph_from_biadjacency_matrix(true_Mt)

    cmpnt <- igraph::components(g)
    cmpnt_sizes <- as.vector(cmpnt$csize)
    largest_cmpnt <- max(cmpnt_sizes)
    n_components <- cmpnt$no

    ## NLTT, plant
    clades_info_plant <- rep[["island"]][["clades_info_plant"]]
    brt_p <- lapply(clades_info_plant, "[[", "branching_times")
    end_ltt_p <- end_ltt_mutual(clades_info_plant, brt_p)

    nonend_ltt_p <- end_ltt_p$nonend_ltt
    singleton_ltt_p <- end_ltt_p$singleton_ltt
    multi_ltt_p <- end_ltt_p$multi_ltt

    # NLTT-nonend
    if (nonend_ltt_p[1, 1] == 0) {
      nonend_nltt_p <- 0
    } else {
      nonend_nltt_p <- nLTT::nltt_diff_exact_extinct(
        event_times = nonend_ltt_p$nonend_brt,
        species_number = nonend_ltt_p$n_nonend,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    #NLTT-singleton
    if (singleton_ltt_p[1, 1] == 0) {
      singleton_nltt_p <- 0
    } else {
      singleton_nltt_p <- nLTT::nltt_diff_exact_extinct(
        event_times = singleton_ltt_p$singleton_brt,
        species_number = singleton_ltt_p$n_singleton,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    #NLTT-multi
    if (multi_ltt_p[1, 1] == 0) {
      multi_nltt_p <- 0
    } else {
      multi_nltt_p <- nLTT::nltt_diff_exact_extinct(
        event_times = multi_ltt_p$multi_brt,
        species_number = multi_ltt_p$n_multi,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    ## NLTT, animal
    clades_info_animal <- rep[["island"]][["clades_info_animal"]]
    brt_a <- lapply(clades_info_animal, "[[", "branching_times")
    end_ltt_a <- end_ltt_mutual(clades_info_animal, brt_a)

    nonend_ltt_a <- end_ltt_a$nonend_ltt
    singleton_ltt_a <- end_ltt_a$singleton_ltt
    multi_ltt_a <- end_ltt_a$multi_ltt

    # NLTT-nonend
    if (nonend_ltt_a[1, 1] == 0) {
      nonend_nltt_a <- 0
    } else {
      nonend_nltt_a <- nLTT::nltt_diff_exact_extinct(
        event_times = nonend_ltt_a$nonend_brt,
        species_number = nonend_ltt_a$n_nonend,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    #NLTT-singleton
    if (singleton_ltt_a[1, 1] == 0) {
      singleton_nltt_a <- 0
    } else {
      singleton_nltt_a <- nLTT::nltt_diff_exact_extinct(
        event_times = singleton_ltt_a$singleton_brt,
        species_number = singleton_ltt_a$n_singleton,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    #NLTT-multi
    if (multi_ltt_a[1, 1] == 0) {
      multi_nltt_a <- 0
    } else {
      multi_nltt_a <- nLTT::nltt_diff_exact_extinct(
        event_times = multi_ltt_a$multi_brt,
        species_number = multi_ltt_a$n_multi,
        event_times2 = 0,
        species_number2 = 0,
        distance_method = "abs",
        time_unit = "ago",
        normalize = FALSE
      )
    }

    # Store the summary information
    data.frame(
      island_p = island_p,
      island_a = island_a,
      island_endemic_p = island_endemic_p ,
      island_nonendemic_p = island_nonendemic_p,
      island_endemic_a = island_endemic_a,
      island_nonendemic_a = island_nonendemic_a,
      island_total = island_p + island_a,
      island_endemic_total = island_endemic_p + island_endemic_a,
      island_nonendemic_total = island_nonendemic_p + island_nonendemic_a,
      connectance = connectance,
      disconnect_p = disconnect_p,
      disconnect_a = disconnect_a,
      largest_cmpnt = largest_cmpnt,
      n_components = n_components,
      nonend_nltt_p = nonend_nltt_p,
      singleton_nltt_p = singleton_nltt_p,
      multi_nltt_p = multi_nltt_p,
      nonend_nltt_a = nonend_nltt_a,
      singleton_nltt_a = singleton_nltt_a,
      multi_nltt_a = multi_nltt_a
    )
  })
}








