
## Function to get the total links in community for links plot

# For a single combination of parameter case, with mutualism none, low and high

get_NLH_links <- function(the_path, the_case, total_time, box_data) {

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                 paste0(the_path, the_case, "_low.rds"),
                 paste0(the_path, the_case, "_high.rds"))

  # Process each file and store results
  links_data <- lapply(file_names, function(file) {
    # Read the data
    data <- readRDS(file)

    # Get total links over time
    links_data <- get_linksOverTime(data = data,
                                    total_time = total_time,
                                    box_data = box_data)
  })

  # Combine all data into a single data.frame
  names(links_data) <- c("None", "Low", "High")
  all_links_data <- bind_rows(links_data, .id = 'Type')

  all_links_data$Type <- factor(all_links_data$Type, levels = c("None", "Low", "High"))

  return(all_links_data)
}


















