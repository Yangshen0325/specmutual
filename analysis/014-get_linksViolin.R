
## Get the total links of networks on the island for 16 scenarios. Total links
# at the last time step. For violin plot.
get_NLH_links_Violin <- function(the_path, the_case) {
  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_low.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  # Process none, low and high files and store the results
  links_violin <- lapply(file_names, getalllinks)

  names(links_violin) <- c("None", "Low", "High")
  all_links_violin <- bind_rows(links_violin, .id = "Type")

  all_links_violin$Type <- factor(all_links_violin$Type, levels = c("None", "Low", "High"))

  return(all_links_violin)

}


## Get all links on island for one single scenario, e.g., for the case of mutualism
# effect is "none".

getalllinks <- function(dataset_name) {

  # read data
  data <- readRDS(dataset_name)

  # Extract the total links for each replicate
  sum_links_list <- lapply(data, function(rep){

    Mt <- rep$Mt
    status_p <- rep$status_p
    status_a <- rep$status_a
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]
    total_links <- sum(true_Mt)

    data.frame(total_links = total_links)
  })
# Combine all sum_info into a single data frame for this dataset
sum_links_df <- bind_rows(sum_links_list)

}
