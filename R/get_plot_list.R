# prepare plot list for stt plot
# sim_outputs <- sim_mutualism(total_time = total_time,
#                              replicates = replicates,
#                              mutualism_pars = mutualism_pars,
#                              sample_freq = sample_freq,
#                              verbose = TRUE)


get_plot_list <- function(sim_outputs) {
  island_total <- sim_outputs$island_total

  rep <- length(island_total)
  s_freq <- length(island_total[[1]][[1]]$stt_all[, 1])
  complete_arr_all <- array(dim = c(s_freq, 6, rep))
  complete_arr_p <- array(dim = c(s_freq, 6, rep))
  complete_arr_a <- array(dim = c(s_freq, 6, rep))
  for (x in 1:rep) {
    # All: plant and animal
    sum_endemics_all <- island_total[[x]][[1]]$stt_all[, "nAp"] +
      island_total[[x]][[1]]$stt_all[, "nCp"] +
      island_total[[x]][[1]]$stt_all[, "nAa"] +
      island_total[[x]][[1]]$stt_all[, "nCa"]
    total_all <- island_total[[x]][[1]]$stt_all[, "nIp"] +
      island_total[[x]][[1]]$stt_all[, "nAp"] +
      island_total[[x]][[1]]$stt_all[, "nCp"] +
      island_total[[x]][[1]]$stt_all[, "nIa"] +
      island_total[[x]][[1]]$stt_all[, "nAa"] +
      island_total[[x]][[1]]$stt_all[, "nCa"]
    nI_all <- island_total[[x]][[1]]$stt_all[, "nIp"] +
      island_total[[x]][[1]]$stt_all[, "nIa"]
    nA_all <- island_total[[x]][[1]]$stt_all[, "nAp"] +
      island_total[[x]][[1]]$stt_all[, "nAa"]
    nC_all <- island_total[[x]][[1]]$stt_all[, "nCp"] +
      island_total[[x]][[1]]$stt_all[, "nCa"]
    complete_arr_all[, , x] <- cbind(
      island_total[[x]][[1]]$stt_all[, "Time"],
      nI_all,
      nA_all,
      nC_all,
      sum_endemics_all,
      total_all
    )
    # Plant
    sum_endemics_p <- island_total[[x]][[1]]$stt_all[, "nAp"] +
      island_total[[x]][[1]]$stt_all[, "nCp"]
    total_p <- island_total[[x]][[1]]$stt_all[, "nIp"] +
      island_total[[x]][[1]]$stt_all[, "nAp"] +
      island_total[[x]][[1]]$stt_all[, "nCp"]
    complete_arr_p[, , x] <- cbind(
      island_total[[x]][[1]]$stt_all[, c(1:4)],
      sum_endemics_p,
      total_p
    )
    # Animal
    sum_endemics_a <- island_total[[x]][[1]]$stt_all[, "nAa"] +
      island_total[[x]][[1]]$stt_all[, "nCa"]
    total_a <- island_total[[x]][[1]]$stt_all[, "nIa"] +
      island_total[[x]][[1]]$stt_all[, "nAa"] +
      island_total[[x]][[1]]$stt_all[, "nCa"]
    complete_arr_a[, , x] <- cbind(
      island_total[[x]][[1]]$stt_all[, c(
        "Time",
        "nIa", "nAa", "nCa"
      )],
      sum_endemics_a,
      total_a
    )
  }
  # All species
  stt_average_all <- apply(complete_arr_all, c(1, 2), stats::median)
  stt_q0.025_all <- apply(complete_arr_all, c(1, 2), stats::quantile, 0.025)
  stt_q0.25_all <- apply(complete_arr_all, c(1, 2), stats::quantile, 0.25)
  stt_q0.75_all <- apply(complete_arr_all, c(1, 2), stats::quantile, 0.75)
  stt_q0.975_all <- apply(complete_arr_all, c(1, 2), stats::quantile, 0.975)
  colnames_all <- c("Time", "nI_all", "nA_all", "nC_all", "Endemic_all", "Total_all")
  colnames(stt_average_all) <- colnames_all
  colnames(stt_q0.025_all) <- colnames_all
  colnames(stt_q0.25_all) <- colnames_all
  colnames(stt_q0.75_all) <- colnames_all
  colnames(stt_q0.975_all) <- colnames_all
  all_species <- list(
    stt_average_all = stt_average_all,
    stt_q0.025_all = stt_q0.025_all,
    stt_q0.25_all = stt_q0.25_all,
    stt_q0.75_all = stt_q0.75_all,
    stt_q0.975_all = stt_q0.975_all
  )
  # Plant
  stt_average_p <- apply(complete_arr_p, c(1, 2), stats::median)
  stt_q0.025_p <- apply(complete_arr_p, c(1, 2), stats::quantile, 0.025)
  stt_q0.25_p <- apply(complete_arr_p, c(1, 2), stats::quantile, 0.25)
  stt_q0.75_p <- apply(complete_arr_p, c(1, 2), stats::quantile, 0.75)
  stt_q0.975_p <- apply(complete_arr_p, c(1, 2), stats::quantile, 0.975)
  colnames_p <- c("Time", "nIp", "nAp", "nCp", "Endemic_p", "Total_p")
  colnames(stt_average_p) <- colnames_p
  colnames(stt_q0.025_p) <- colnames_p
  colnames(stt_q0.25_p) <- colnames_p
  colnames(stt_q0.75_p) <- colnames_p
  colnames(stt_q0.975_p) <- colnames_p
  plant <- list(
    stt_average_p = stt_average_p,
    stt_q0.025_p = stt_q0.025_p,
    stt_q0.25_p = stt_q0.25_p,
    stt_q0.75_p = stt_q0.75_p,
    stt_q0.975_p = stt_q0.975_p
  )
  # Animal
  stt_average_a <- apply(complete_arr_a, c(1, 2), stats::median)
  stt_q0.025_a <- apply(complete_arr_a, c(1, 2), stats::quantile, 0.025)
  stt_q0.25_a <- apply(complete_arr_a, c(1, 2), stats::quantile, 0.25)
  stt_q0.75_a <- apply(complete_arr_a, c(1, 2), stats::quantile, 0.75)
  stt_q0.975_a <- apply(complete_arr_a, c(1, 2), stats::quantile, 0.975)
  colnames_a <- c("Time", "nIa", "nAa", "nCa", "Endemic_a", "Total_a")
  colnames(stt_average_a) <- colnames_a
  colnames(stt_q0.025_a) <- colnames_a
  colnames(stt_q0.25_a) <- colnames_a
  colnames(stt_q0.75_a) <- colnames_a
  colnames(stt_q0.975_a) <- colnames_a
  animal <- list(
    stt_average_a = stt_average_a,
    stt_q0.025_a = stt_q0.025_a,
    stt_q0.25_a = stt_q0.25_a,
    stt_q0.75_a = stt_q0.75_a,
    stt_q0.975_a = stt_q0.975_a
  )
  return(list(
    all_species = all_species,
    plant = plant,
    animal = animal
  ))
}
