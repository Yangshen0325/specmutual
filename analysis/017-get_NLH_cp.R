
# The number of competitors for one plant species should is the number of other
# share at least one animal with it. (Their rows have overlapping 1 in the same column)



# Example -----------------------------------------------------------------


# wed <- matrix(c(1, 1, 1, 0, 1, 1), nrow = 3)
# web
# rownames(wed) <- c("Plant1", "Plant2", "Plant3")
# colnames(wed) <- c("Animal1", "Animal2")

# plant_competitors <- wed %*% t(wed)  # Matrix multiplication: plants x plants
# diag(plant_competitors) <- 0         # Set diagonal to 0 (a plant doesn't compete with itself)
# plant_competitors
#             Plant1 Plant2 Plant3
# Plant1       0       1       1 # Plant1 competes with Plant2 and Plant3.
# Plant2       1       0       2 # Plant2 competes with Plant1 and Plant3.
# Plant3       1       2       0 # Plant3 competes with Plant1 and Plant2.





# Function ----------------------------------------------------------------


# Function to get the number of competitors for plant and animal across 16 scenarios.
get_NLH_cp <- function(the_path, the_case){

  # Data source
  file_names <- c(paste0(the_path, the_case, "_none.rds"),
                  paste0(the_path, the_case, "_medium.rds"),
                  paste0(the_path, the_case, "_high.rds"))

  # Process none, medium and high files and store the results
  cp_info <- lapply(file_names, getCP)

  names(cp_info) <- c("None", "Medium", "High")
  all_cp_df <- bind_rows(cp_info, .id = "Type")

  all_cp_df$Type <- factor(all_cp_df$Type, levels = c("None", "Medium", "High"))

  return(all_cp_df)

}




# Function to get competitors number --------
# For one case, e.g., none mutualism effects

getCP <- function(dataset_name) {
  # Read data
  data <- readRDS(dataset_name)

  # Deal with data
  competitor_list <- lapply(data, function(rep_outputs){

    Mt <- rep_outputs$Mt
    status_p <- rep_outputs$status_p
    status_a <- rep_outputs$status_a

    # the community on islands `true_Mt` (with species with 0 links)
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    # the number of competitors for plant and animal species
    if (nrow(true_Mt) == 0 | ncol(true_Mt) == 0) {
      avg_p <- NA
      avg_a <- NA
      sd_p <- NA
      sd_a <- NA
    } else {
      # For plant
      plant_net <- true_Mt %*% t(true_Mt)
      diag(plant_net) <- 0
      competitor_p <- rowSums(plant_net > 0)
      avg_p <- mean(competitor_p)
      sd_p <- sd(competitor_p)

      # For animal
      animal_net <- t(true_Mt) %*% true_Mt
      diag(animal_net) <- 0
      competitor_a <- rowSums(animal_net > 0)
      avg_a <- mean(competitor_a)
      sd_a <- sd(competitor_a)

    }

    data.frame(

      "avg_cp_p" = avg_p, # avrage number of competitors for plant species
      "avg_cp_a" = avg_a, # avrage number of competitors for animal species
      "sd_cp_p" = sd_p, # standard deviation of competitors for plant species
      "sd_cp_a" = sd_a # standard deviation of competitors for animal species

    )
  })

  # Combine together
  cp_df <- bind_rows(competitor_list)
  return(cp_df)

}











