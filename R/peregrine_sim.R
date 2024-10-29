#' Running it in cluster
#'
#' @param total_time simulation time
#' @param replicates replicates
#' @param mutualism_pars muutalism-related stuff
#' @param verbose
#'
#' @return a list
#' @export
#'
peregrine_sim <- function(total_time,
                          replicates,
                          mutualism_pars,
                          return_parts,
                          verbose = TRUE) {
  island_replicates <- list()
  for (rep in 1:replicates) {
    island_replicates[[rep]] <- sim_core_mutualism(
      total_time = total_time,
      mutualism_pars = mutualism_pars,
      return_parts = return_parts
    )
    if (verbose == TRUE) {
      print(paste("Island replicate ", rep, sep = ""))
    }
  }

  return(island_replicates = island_replicates)
}
