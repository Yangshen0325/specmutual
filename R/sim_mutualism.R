sim_mutualism <- function(simtime,
                          replicates,
                          mutualism_pars){
  island_replicates <- list()
  for (rep in 1:replicates){
    island_replicates[[rep]] <- sim_core_mutualism(
      simtime = simtime,
      mutualism_pars = mutualism_pars
    )
  }
  return (island_replicates)
}
