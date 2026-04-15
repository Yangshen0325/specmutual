

# Function to get each kind of rate over time.

# Function to process each replicate and unify data
get_rate_ptn_dynmc <- function(replicate_data, standard_time) {
  # Extract lists for the current replicate
  rates_list <- replicate_data[["rates_list"]]
  timeval_list <- replicate_data[["timeval_list"]]
  richness_p_list <- replicate_data[["richness_p_list"]]
  richness_a_list <- replicate_data[["richness_a_list"]]

  #sum_partners_p <- replicate_data[["sum_partners_p"]]
  #sum_partners_a <- replicate_data[["sum_partners_a"]]

  # Sum rates across all species and remove last element (exceeds total time)
  rates_each_sum <- lapply(rates_list, function(x) sapply(x, sum))
  rates_each_sum <- do.call(rbind, rates_each_sum[-length(rates_each_sum)])

  # Sum partners for plant and animal, and remove last element (exceeds total time)
  #sum_partners_p <- sum_partners_p[-length(sum_partners_p)]
  #sum_partners_a <- sum_partners_a[-length(sum_partners_a)]

  # Adjust time values, remove last entry (exceeds total time)
  timeval <- total_time - unlist(timeval_list[-length(timeval_list)])

  # Extract richness data
  richness_p <- unlist(richness_p_list)
  richness_a <- unlist(richness_a_list)

  # Create a data frame with the processed data for the replicate
  data_sum_df <- as.data.frame(rates_each_sum)
  data_sum_df$richness_p <- richness_p
  data_sum_df$richness_a <- richness_a
  #data_sum_df$sum_partners_p <-sum_partners_p
  #data_sum_df$sum_partners_a <-sum_partners_a
  data_sum_df <- cbind(timeval = timeval, data_sum_df)
  data_sum_df[1, ] <- c(10, rep(0, ncol(data_sum_df) - 1))

  # Initialize matrix to store unified data for this replicate
  unify_data_sum_df <- matrix(nrow = length(standard_time), ncol = ncol(data_sum_df))
  colnames(unify_data_sum_df) <- colnames(data_sum_df)
  unify_data_sum_df[, "timeval"] <- standard_time
  unify_data_sum_df[1, 2:ncol(unify_data_sum_df)] <- rep(0, ncol(unify_data_sum_df) - 1)

  # Fill in data based on the closest time values
  for (i in 2:nrow(unify_data_sum_df)) {
    the_time <- unify_data_sum_df[i, "timeval"]
    unify_data_sum_df[i, 2:ncol(unify_data_sum_df)] <- unlist(data_sum_df[max(which(data_sum_df[, "timeval"] >= the_time)), 2:ncol(data_sum_df)])
  }

  # Convert to a data frame
  return(as.data.frame(unify_data_sum_df))
}



