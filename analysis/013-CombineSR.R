

# Combine 16 plots of species richness bar plots
# p1: total number of species of plant and animal
# p2: number of endemic plant and animal species

plot_sr_NLH <- function(the_path, the_case, show_legend) {

  all_sr_NLH_data <- get_NLH_sr(the_path, the_case)

  plot_data <- all_sr_NLH_data |>
    group_by(Type) |>
    summarise(avg_plant = mean(island_p, na.rm = TRUE),
              avg_animal = mean(island_a, na.rm = TRUE)) |>
    pivot_longer(
      cols = c(avg_plant, avg_animal),
      names_to = "species",
      values_to = "value"
    )

  # Adjust species names for better labels
  plot_data$species <- recode(plot_data$species,
                              avg_plant = "Plant",
                              avg_animal = "Animal")

  plot_data$species <- factor(plot_data$species, levels = c("Plant", "Animal"))

  # Plot
  p <- ggplot(plot_data, aes(x = Type, y = value, fill = species)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c("darkgreen", "orange")) +
    labs(
      x = "Mutualism Effects",
      y = "Number of Species",
      fill = "Species"
    ) +
    theme_minimal(base_size = 12) +
    theme(aspect.ratio = 3/4,
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)

}
