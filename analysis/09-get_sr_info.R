
# Summarize the number of plant, animal, and total species on the island, plot it.
# Summarize the number of endemic plant species, the number of endemic animal species



# Function to get species richness and endemism ---------------------------

# List of dataset file names
dataset_files <- c("_none.rds", "_medium.rds", "_high.rds")

# Function to get species richness
get_sr_single <- function(dataset_name) {

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

    # Store the summary information
    data.frame(
      island_p = island_p,
      island_a = island_a,
      island_endemic_p = island_sp_emdemism[3] + island_sp_emdemism[4],
      island_nonendemic_p = island_sp_emdemism[2],
      island_endemic_a = island_sp_emdemism[6] + island_sp_emdemism[7],
      island_nonendemic_a = island_sp_emdemism[5]
    )
  })
  # Combine all sum_info into a single data frame for this dataset
  sum_info_df <- bind_rows(sum_info_list)
}



# Function to scan all file paths -----------------------------------------

get_sr_all <- function(the_path) {

  data_sets <- paste0(the_path, dataset_files)

  all_results <- lapply(data_sets, get_sr_single)

  # Combine results into a single data frame
  final_table <- bind_rows(all_results, .id = "Effects")

  effects_labels <- c("None", "Medium", "High")

  final_table$Effects <- effects_labels[as.numeric(final_table$Effects)]
  final_table$Effects <- factor(final_table$Effects, levels = c("None", "Medium", "High"))

  final_table <- final_table |>
    mutate(
      island_total = island_p + island_a,
      island_endemic_total = island_endemic_p + island_endemic_a,
      island_nonendemic_total = island_nonendemic_p + island_nonendemic_a
    )

  return(final_table)

}

