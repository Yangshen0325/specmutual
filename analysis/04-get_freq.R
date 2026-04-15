

# Function to get the frequency of each event happened on the island across replicates

# plot the average frequencies on the island
get_freq <- function(island_replicates) {
  reps <- length(island_replicates)
  frequencies <- sapply(island_replicates, function(replicate) {
    evo_table <- replicate$evo_table
    second_column <- evo_table[, 2]
    table(second_column)
  })

  # Initialize an empty list to store each replicate's standardized data,1:11
  all_frequencies <- list()

  for (i in 1:length(frequencies)) {

    the_freq <- frequencies[[i]]

    # Create a complete event data frame with all 11 event types, filling in missing ones with zero frequency
    complete_data <- data.frame(event_id = 1:11, frequency = 0)

    # Update the frequency for event types present in the current simulation
    complete_data$frequency[as.integer(names(the_freq))] <- as.numeric(the_freq)

    complete_data$replicate <- i

    all_frequencies[[i]] <- complete_data
  }

  x_labels <- c("immig_P", "ext_P", "clado_P", "ana_P", "immig_A", "ext_A", "clado_A",
                "ana_A", "cospec", "gain", "loss")

  # label each event
  evt_freq <- bind_rows(all_frequencies) |>
    mutate(event_is = factor(event_id, levels = 1:11, labels = x_labels))

  return(evt_freq)
}

