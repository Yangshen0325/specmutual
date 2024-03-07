#### put plant and animal together

format_all_full_stt <- function(island_replicates,
                                total_time,
                                sample_freq,
                                mutualism_pars,
                                verbose) {
  M0 <- mutualism_pars$M0
  several_islands <- list()
  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]][["island"]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal
    clades_info_total <- c(clades_info_plant, clades_info_animal)

    island_list <- list()
    if(sum(the_stt[nrow(the_stt), 2:7]) == 0) {
      island_list[[1]] <- list(island_age = total_time,
                               not_present = nrow(M0) + ncol(M0),
                               stt_all = the_stt)
    } else {
      island_list[[1]] <- list(
        island_age = total_time,
        not_present = nrow(M0) + ncol(M0) - length(clades_info_total),
        stt_all = the_stt)
      for (y in 1:length(clades_info_total)) {
        island_list[[y + 1]] = clades_info_total[[y]]
      }
    }
    island_list <- DAISIE:::add_brt_table(island_list)
    several_islands[[rep]] <- island_list
    if (verbose == TRUE) {
      message(
        "Island being formatted: ", rep, "/", length(island_replicates)
      )
    }
  }
  return(several_islands)
}
