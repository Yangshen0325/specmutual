
## Functions for 16 combo plots (stt)
## `species_type` options: "all", "plant", "animal".
# "all" includes both plant and animal species.
# "plant" includes only plant species.
# "animal" includes only animal species.

# Filter `endemism_data`, `non_endemic_dat`, `endemic_dat`, `total_dat` for
# non-endemic, endemic and total species analysis.
# `data_type` options: "non_endemic_dat", "endemic_dat", "total_dat"

# Function to process data, can choose total(endemic + non-endemic data), endemic or non-endemic data
get_endemism_data <- function(the_path, the_case, species_type, data_type,
                              M0, total_time, sample_freq) {

  NLH_endemism_list <- get_NLH_endemism_list(
    the_path = the_path,
    the_case = the_case,
    species_type = species_type,
    M0 = M0,
    total_time = total_time,
    sample_freq = sample_freq
  )

  endemism_data <- NLH_endemism_list |>
    filter(Type == data_type)

  return(endemism_data)
}

# Function to create a single plot
plot_endemism_data <- function(data, y_limit, show_legend = NULL) {

  p <- ggplot(data, aes(x = Time, y = Median + 1, color = Group, fill = Group)) +
    geom_line(linewidth = 1.0) +
    geom_ribbon(aes(ymin = Q0.25 + 1, ymax = Q0.75 + 1), alpha = 0.2, color = NA) +
    scale_x_reverse() +
    scale_y_continuous(
      limits = c(1, y_limit + 100),
      trans = "log10"
    ) +
    scale_color_manual(values = c("green", "blue", "red")) +
    scale_fill_manual(values = c("green", "blue", "red")) +
    labs(
      x = "Time to Present",
      y = "No.species (+ 1)",
      color = "Mutualism Effects",
      fill = "Mutualism Effects"
    ) +
    theme_bw(base_size = 12) +
    theme(
      # axis.text = element_text(size = 10),
      # axis.title = element_text(size = 12, face = "bold"),
      # legend.text = element_text(size = 10),
      # legend.title = element_text(size = 12),
      # plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      aspect.ratio = 3/4
    )

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

# Function combining the above functions to generate a single plot
generate_combined_plot <- function(the_path, the_case, species_type, data_type, M0, y_limit,
                                   total_time, sample_freq, show_legend) {

    # Process data to get the endemism data
    data <- get_endemism_data(
      the_path = the_path,
      the_case = the_case,
      species_type = species_type,
      data_type = data_type,
      M0 = M0,
      total_time = total_time,
      sample_freq = sample_freq
    )

    p <- plot_endemism_data(data = data, y_limit = y_limit, show_legend = show_legend)

    return(p)
}















