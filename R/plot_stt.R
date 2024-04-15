#' @title Plot species through time (stt) plots on island
#'
#' @description plot_stt() plots three figures based on total species, plant species and animal
#' species on island. Each of them has the total, endemic and non-endemic with the
#' simulated entire time span. 2.5-97.5th percentiles are plotted in light grey,
#' 25-75th percentiles plotted in dark grey.
#'
#' @param several_islands islands' information
#' @param total_time simulation time
#'
#' @return R plot with a row of three figures.
#' @export
#'
#' @examples
#' plot_stt(
#'   several_island = several_island,
#'   total_time = total_time
#' )
plot_stt <- function(total_time,
                     sim_outputs) {
  plot_list <- get_plot_list(sim_outputs = sim_outputs)
  all_species <- plot_list[["all_species"]]
  plant <- plot_list[["plant"]]
  animal <- plot_list[["animal"]]
  par(mfrow = c(1, 3))
  # all species
  suppressWarnings(
    graphics::plot(
      NULL, NULL,
      xlim = rev(c(0, total_time)), ylim = c(1, max(all_species$stt_q0.975_all)),
      main = "Species-through-time - All species",
      xlab = "Time before present", ylab = "No of species + 1",
      cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2,
      xaxs = "i", yaxt = "s",
      bty = "l", log = "y"
    )
  )
  graphics::polygon(c(all_species$stt_average_all[, "Time"], rev(all_species$stt_average_all[, "Time"])),
    c(
      all_species$stt_q0.025_all[, "Total_all"] + 1,
      rev(all_species$stt_q0.975_all[, "Total_all"] + 1)
    ),
    col = "light grey", border = NA
  )
  graphics::polygon(c(all_species$stt_average_all[, "Time"], rev(all_species$stt_average_all[, "Time"])),
    c(
      all_species$stt_q0.25_all[, "Total_all"] + 1,
      rev(all_species$stt_q0.75_all[, "Total_all"] + 1)
    ),
    col = "dark grey", border = NA
  )
  graphics::lines(all_species$stt_average_all[, "Time"],
    all_species$stt_average_all[, "Total_all"] + 1,
    lwd = 2
  )
  graphics::lines(all_species$stt_average_all[, "Time"],
    all_species$stt_average_all[, "nI_all"] + 1,
    lwd = 2, col = "cyan3"
  )
  graphics::lines(all_species$stt_average_all[, "Time"],
    all_species$stt_average_all[, "Endemic_all"] + 1,
    lwd = 2, col = "dodgerblue1"
  )
  legend_names <- c("Total_all", "Non-endemic_all", "Endemic_all")
  legend_colors <- c("black", "cyan3", "dodgerblue1")
  graphics::legend(
    x = "topleft", legend = legend_names, col = legend_colors,
    lty = 1, lwd = 0.4, cex = 0.4, border = NA, bty = "n"
  )
  # plant
  suppressWarnings(
    graphics::plot(
      NULL, NULL,
      xlim = rev(c(0, total_time)), ylim = c(1, max(plant$stt_q0.975_p)),
      main = "Species-through-time - Plant",
      xlab = "Time before present", ylab = "No of species + 1",
      cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2,
      xaxs = "i", yaxt = "s",
      bty = "l", log = "y"
    )
  )
  graphics::polygon(c(plant$stt_average_p[, "Time"], rev(plant$stt_average_p[, "Time"])),
    c(
      plant$stt_q0.025_p[, "Total_p"] + 1,
      rev(plant$stt_q0.975_p[, "Total_p"] + 1)
    ),
    col = "light grey", border = NA
  )
  graphics::polygon(c(plant$stt_average_p[, "Time"], rev(plant$stt_average_p[, "Time"])),
    c(
      plant$stt_q0.25_p[, "Total_p"] + 1,
      rev(plant$stt_q0.75_p[, "Total_p"] + 1)
    ),
    col = "dark grey", border = NA
  )
  graphics::lines(plant$stt_average_p[, "Time"],
    plant$stt_average_p[, "Total_p"] + 1,
    lwd = 2
  )
  graphics::lines(plant$stt_average_p[, "Time"],
    plant$stt_average_p[, "nIp"] + 1,
    lwd = 2, col = "cyan3"
  )
  graphics::lines(plant$stt_average_p[, "Time"],
    plant$stt_average_p[, "Endemic_p"] + 1,
    lwd = 2, col = "dodgerblue1"
  )
  legend_names <- c("Total_p", "Non-endemic_p", "Endemic_p")
  legend_colors <- c("black", "cyan3", "dodgerblue1")
  graphics::legend(
    x = "topleft", legend = legend_names, col = legend_colors,
    lty = 1, lwd = 0.4, cex = 0.4, border = NA, bty = "n"
  )

  # animal
  suppressWarnings(
    graphics::plot(
      NULL, NULL,
      xlim = rev(c(0, total_time)), ylim = c(1, max(animal$stt_q0.975_a)),
      main = "Species-through-time - Animal",
      xlab = "Time before present", ylab = "No of species + 1",
      cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2,
      xaxs = "i", yaxt = "s",
      bty = "l", log = "y"
    )
  )
  graphics::polygon(c(animal$stt_average_a[, "Time"], rev(animal$stt_average_a[, "Time"])),
    c(
      animal$stt_q0.025_a[, "Total_a"] + 1,
      rev(animal$stt_q0.975_a[, "Total_a"] + 1)
    ),
    col = "light grey", border = NA
  )
  graphics::polygon(c(animal$stt_average_a[, "Time"], rev(animal$stt_average_a[, "Time"])),
    c(
      animal$stt_q0.25_a[, "Total_a"] + 1,
      rev(animal$stt_q0.75_a[, "Total_a"] + 1)
    ),
    col = "dark grey", border = NA
  )
  graphics::lines(animal$stt_average_a[, "Time"],
    animal$stt_average_a[, "Total_a"] + 1,
    lwd = 2
  )
  graphics::lines(animal$stt_average_a[, "Time"],
    animal$stt_average_a[, "nIa"] + 1,
    lwd = 2, col = "cyan3"
  )
  graphics::lines(animal$stt_average_a[, "Time"],
    animal$stt_average_a[, "Endemic_a"] + 1,
    lwd = 2, col = "dodgerblue1"
  )
  legend_names <- c("Total_a", "Non-endemic_a", "Endemic_a")
  legend_colors <- c("black", "cyan3", "dodgerblue1")
  graphics::legend(
    x = "topleft", legend = legend_names, col = legend_colors,
    lty = 1, lwd = 0.4, cex = 0.4, border = NA, bty = "n"
  )
}
