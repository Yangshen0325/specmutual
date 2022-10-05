#' @title sim_mutualism
#' @export sim_mutualism
sim_mutualism <- function(simtime,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          plot_sims = TRUE){
  island_replicates <- list()
  for (rep in 1:replicates){
    print(rep)
    island_replicates[[rep]] <- sim_core_mutualism(
      simtime = simtime,
      mutualism_pars = mutualism_pars
    )
  }
  several_islands <- format_island(island_replicates = island_replicates,
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
