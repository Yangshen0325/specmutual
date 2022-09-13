#' @title sim_mutualism
#' @export sim_mutualism
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
# replicates means how many times I wanna repeat the code, not the same meaning
# from DAISIE running two different macro-evolutionary processes.
