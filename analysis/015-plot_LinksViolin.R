
#### Function to plot total links across mutualism effects for 16 scenarios.
# Violink plot



format_exp <- function(x) {

  parse(text = sprintf("10^%s", as.character(round(log10(x), 0))))

}

plot_links_violin <- function(data, y_limit, show_legend = NULL) {

  p <- ggplot(data, aes(x = Type, y = total_links + 1, fill = Type)) +
    geom_violin(trim = TRUE) +
    stat_summary(fun = function(x) (log10(mean(10^x))),
                 geom = "point", size = 1.0, color = "orange") +
    scale_y_continuous(trans = 'log10',
                       limits = y_limit + 100,
                       labels = format_exp) +
    scale_fill_brewer(palette = "Blues") +
    labs(x = "Mutualism Effects", y = "Total Links (log)") +
    theme_classic()
  # theme(axis.title.x = element_text(vjust = -2, size = 12, face = "bold"),
  #       axis.title.y = element_text(vjust = 2, size = 12, face = "bold"),
  #       axis.text = element_text(size = 10, face = "bold"),
  #       legend.position = "none")

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}















