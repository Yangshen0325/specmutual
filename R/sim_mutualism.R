#' @title sim_mutualism

sim_mutualism <- function(total_time,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          plot_sims = TRUE,
                          verbose = TRUE) {
  island_replicates <- list()
  for (rep in 1:replicates) {
    island_replicates[[rep]] <- sim_core_mutualism(total_time = total_time,
                                                   mutualism_pars = mutualism_pars)
  }
  if (verbose == TRUE) {
    print(paste("Island replicate ", rep, sep = ""))
  }
  island_replicates <- format_island(island_replicates = island_replicates,
                                   simtime = simtime,
                                   sample_freq = sample_freq,
                                   mutualism_pars = mutualism_pars)
  if (plot_sims == TRUE){
    plot_stt(several_islands = several_islands,
             simtime = simtime)
  }
  return (list(island_replicates = island_replicates,
               several_islands = several_islands))
}
# replicates means how many times I wanna repeat the code, not the same meaning
# from DAISIE running two different macro-evolutionary processes.
