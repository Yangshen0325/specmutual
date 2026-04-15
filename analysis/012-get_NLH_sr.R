

# Get the species richness on the island for plant and animal species,
# bar plot
get_NLH_sr <- function(the_path, the_case) {
  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_low.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  # Process each file and store the results
  sr_NLH_data <- lapply(file_names, get_sr_single)

  names(sr_NLH_data) <- c("None", "Low", "High")
  all_sr_NLH_data <- bind_rows(sr_NLH_data, .id = "Type")

  all_sr_NLH_data$Type <- factor(all_sr_NLH_data$Type, levels = c("None", "Low", "High"))

  all_sr_NLH_data <-  all_sr_NLH_data|>
    mutate(
      island_total = island_p + island_a,
      island_endemic_total = island_endemic_p + island_endemic_a,
      island_nonendemic_total = island_nonendemic_p + island_nonendemic_a
    )
    return(all_sr_NLH_data)
  }
