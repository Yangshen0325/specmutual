## New functions to
# (1) First get the degree and the corresponding rank for each replicate, the proportion
# (2) Then get the average value of species number with that degree rank

#### Get degree and corresponding rank under none, medium, high effects for one scenario
get_NLH_deg_bin <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  Type <- c("None", "Medium", "High")

  # Process none, medium and high files and store the results
  degree_bin_all <- lapply(file_names, getDeg_bin)

  degree_bin_p <- lapply(seq_along(degree_bin_all), function(i) {
    df <- degree_bin_all[[i]]$plant
    df$Effects <- Type[i]
    return(df)
  })

  degree_bin_a <- lapply(seq_along(degree_bin_all), function(i) {
    df <- degree_bin_all[[i]]$animal
    df$Effects <- Type[i]
    return(df)
  })

  degree_bin_p <- do.call(rbind, degree_bin_p)
  degree_bin_a <- do.call(rbind, degree_bin_a)

  degree_bin_p$Effects <- factor(degree_bin_p$Effects, levels = c("None", "Medium", "High"))
  degree_bin_a$Effects <- factor(degree_bin_a$Effects, levels = c("None", "Medium", "High"))

  return(list(plant_deg_bin = degree_bin_p,
              animal_deg_bin = degree_bin_a))

}




#### Get the degree and the corresponding rank for one dataset

getDeg_bin <- function(dataset_name){

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

      return(list(
        plant = tibble(degree_bin = NA, Count = NA),
        animal = tibble(degree_bin = NA, Count = NA)
      ))

    }
      # Assign row names as "p1", "p2", "p3", ...
      rownames(true_Mt) <- paste0("p", seq_len(nrow(true_Mt)))

      # Assign column names as "a1", "a2", "a3", ...
      colnames(true_Mt) <- paste0("a", seq_len(ncol(true_Mt)))

      # Convert it to igraph object
      g <- igraph::graph_from_biadjacency_matrix(true_Mt)

      # Degree for each species. plant is FALSE, animal is TRUE
      plant_degree <- igraph::degree(g, v = V(g)$type == FALSE)
      animal_degree <- igraph::degree(g, v = V(g)$type == TRUE)

      list(
        plant = bin_degree(plant_degree),
        animal = bin_degree(animal_degree)
      )
  })

  plant_df <- deg_list |>
    lapply(`[[`, "plant") |>
    bind_rows(.id = "replicate")

  animal_df <- deg_list |>
    lapply(`[[`, "animal") |>
    bind_rows(.id = "replicate")

  # Summarise plant
  plant_summary <- plant_df |>
    filter(!is.na(degree_bin)) |>
    group_by(degree_bin) |>
    summarise(
      #mean_count = mean(Count, na.rm = TRUE),
      #sd_count = sd(Count, na.rm = TRUE),
      mean_prop = mean(Proportion, na.rm = TRUE),
      se_prop = sd(Proportion, na.rm = TRUE) / sqrt(n()), # Standard error
      lower_ci = pmax(mean_prop - 1.96 * se_prop, 0),
      upper_ci = pmin(mean_prop + 1.96 * se_prop, 1),
      .groups = "drop"
    ) |>
    mutate(
      degree_bin = factor(degree_bin, levels = c(
        "[0]", "[1]", "[2]", "[3-4]", "[5-8]", "[9-16]", "[17-32]",
        "[33-64]", "[65-128]", "[129-256]", "[257-512]", "[513-1024]", "[1025-]"
      ))
    )

  # Summarise animal
  animal_summary <- animal_df |>
    filter(!is.na(degree_bin)) |>
    group_by(degree_bin) |>
    summarise(
      #mean_count = mean(Count, na.rm = TRUE),
      #sd_count = sd(Count, na.rm = TRUE),
      mean_prop = mean(Proportion, na.rm = TRUE),
      se_prop = sd(Proportion, na.rm = TRUE) / sqrt(n()), # Standard error
      lower_ci = pmax(mean_prop - 1.96 * se_prop, 0),
      upper_ci = pmin(mean_prop + 1.96 * se_prop, 1),
      .groups = "drop"
    ) |>
    mutate(
      degree_bin = factor(degree_bin, levels = c(
        "[0]", "[1]", "[2]", "[3-4]", "[5-8]", "[9-16]", "[17-32]",
        "[33-64]", "[65-128]", "[129-256]", "[257-512]", "[513-1024]", "[1025-]"
      ))
    )

  list(
    plant = plant_summary,
    animal = animal_summary
  )

}

# Categorise the degree into bins, and count the number of species in each bin

bin_degree <- function(degree_vector) {
  tibble(Degree = degree_vector) |>
    mutate(degree_bin = case_when(
      Degree == 0 ~ "[0]",
      Degree == 1 ~ "[1]",
      Degree == 2 ~ "[2]",
      Degree %in% 3:4 ~ "[3-4]",
      Degree %in% 5:8 ~ "[5-8]",
      Degree %in% 9:16 ~ "[9-16]",
      Degree %in% 17:32 ~ "[17-32]",
      Degree %in% 33:64 ~ "[33-64]",
      Degree %in% 65:128 ~ "[65-128]",
      Degree %in% 129:256 ~ "[129-256]",
      Degree %in% 257:512 ~ "[257-512]",
      Degree %in% 513:1024 ~ "[513-1024]",
      Degree > 1024 ~ "[1025-]"
    )) |>
    count(degree_bin, name = "Count") |>
    mutate(Proportion = Count / sum(Count))
}
