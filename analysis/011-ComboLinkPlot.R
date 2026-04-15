


## Function to combine 16 links plots, for a single scenario.

plot_links_data <- function(the_path, the_case, total_time,
                            box_data, show_legend,
                            y_limit) {

  all_links_data <- get_NLH_links(the_path = the_path,
                                  the_case = the_case,
                                  total_time = total_time,
                                  box_data = box_data)

  if (box_data) {

    # Plot box plot of links over `Time` across `Type`
    longer_data <- all_links_data |>
      pivot_longer(cols = !Type,
                   names_to = "Time",
                   values_to = "Values")
    longer_data$Time <- factor(longer_data$Time, levels = as.character(seq(10, 0, -0.5)))

    p <- ggplot(longer_data, aes(x = Time, y = Values + 1, fill = Type)) +
      geom_boxplot(alpha = 0.8) +
      scale_y_continuous(
        limits = c(1, y_limit + 100),
        trans = "log10"
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Time to Present",
        y = "Number of links (+ 1)",
        fill = "Mutualism Effects"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            aspect.ratio = 3/4,
            axis.text.x = element_text(angle = 45, hjust = 1))

  } else{
  p <- ggplot(all_links_data, aes(x = Time, y = Links_median + 1, color = Type,
                                  fill = Type)) +
    geom_line(linewidth = 1.0) +
    geom_ribbon(aes(ymin = Links_q25 + 1, ymax = Links_q75 + 1), alpha = 0.2, color = NA) +
    scale_x_reverse() +
    scale_y_continuous(
      limits = c(1, y_limit + 100),
      trans = "log10"
    ) +
    scale_color_manual(values = c("darkgrey", "orange", "red")) +
    scale_fill_manual(values = c("darkgrey", "orange", "red")) +
    labs(
      x = "Time to Present",
      y = "Number of Links (+ 1)",
      color = "Mutualism Effects",
      fill = "Mutualism Effects"
    ) +

    theme_minimal(base_size = 12) +
    theme(
      # axis.text = element_text(size = 10),
      # axis.title = element_text(size = 12, face = "bold"),
      # legend.text = element_text(size = 10),
      # legend.title = element_text(size = 12),
      # plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      aspect.ratio = 3/4
    )
  }

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)

}


