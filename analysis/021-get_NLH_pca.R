

# Function to select the data to perform PCA, plant and animal, separately
# `data` is centrality data
# `vars`, plant or animal data, e.g. plant_vars <- c("plant_degree", "plant_between", "plant_harmonic")
# `the_scenario`, the scenario to perform PCA


perform_pca <- function(data, vars, the_scenario) {

  # secelt the data for analysis
  selected_data <- data |>
    filter(Scenario == the_scenario) |>
    select(Type, all_of(vars)) |>
    drop_na()

  # perform PCA
  pca_result <- prcomp(selected_data[, c(2:4)], center = TRUE, scale = TRUE)

  # create a data frame
  pca_data <- as.data.frame(pca_result$x)

  min_PC1 <- min(pca_data$PC1)
  max_PC1 <- max(pca_data$PC1)
  min_PC2 <- min(pca_data$PC2)
  max_PC2 <- max(pca_data$PC2)

  pca_data$Type <- selected_data$Type

  return(list(pca_result = pca_result,
              pca_data = pca_data,
              min_PC1 = min_PC1,
              min_PC2 = min_PC2,
              max_PC1 = max_PC1,
              max_PC2 = max_PC2))

}


# Function to plot
plot_pca <- function(datatoPlot, min_PC1_val, max_PC1_val, min_PC2_val, max_PC2_val) {

  p <-  ggplot(datatoPlot, aes(x = PC1, y = PC2, color = Type)) +
    geom_point(alpha = 0.6) +
    scale_y_continuous(limits = c(min_PC2_val, max_PC2_val)) +
    scale_x_continuous(limits = c(min_PC1_val, max_PC1_val)) +
    scale_color_manual(values = c("None" = "green", "Medium" = "blue", "High" = "red")) +
    labs(color = "Mutualism Effect") +
    theme_bw(base_size = 12) +
    theme(
      aspect.ratio = 3/4
    )
}



# Get all pca data, ready to plot
getPCA_data <- function(data, vars) {

  pca_result_list <- list()
  pca_data_list <- list()

  min_max_df <- data.frame(
    min_PC1 = numeric(0),
    max_PC1 = numeric(0),
    min_PC2 = numeric(0),
    max_PC2 = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in 1:16) {
    the_scenario <- paste0("Scenario_", i)
    spec_pca <- perform_pca(data = data,
                            vars = vars,
                            the_scenario = the_scenario)

    pca_result_list[[i]] <-  spec_pca$pca_result

    pca_data_list[[i]] <- spec_pca$pca_data

    min_max <- data.frame(min_PC1 = spec_pca$min_PC1,
                          max_PC1 = spec_pca$max_PC1,
                          min_PC2 = spec_pca$min_PC2,
                          max_PC2 = spec_pca$max_PC2)

    # Update min_max_df
    min_max_df <- rbind(min_max_df, min_max)

  }

  return(list(pca_result_list = pca_result_list,
              pca_data_list = pca_data_list,
              min_max_df = min_max_df))
}








