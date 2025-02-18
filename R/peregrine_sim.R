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

  tried <- 0
  i <- 1

  while(i <= replicates) {

    sim_output <- sim_core_mutualism(
      total_time = total_time,
      mutualism_pars = mutualism_pars,
      return_parts = return_parts
    )

    Mt <- sim_output$Mt
    status_p <- sim_output$status_p
    status_a <- sim_output$status_a
    true_Mt <- Mt[status_p == 1, status_a == 1, drop = FALSE]

    tried <- tried + 1

    # Check if the network is at least 2 x 2
    if(nrow(true_Mt) >= 2 && ncol(true_Mt) >= 2) {
      island_replicates[[i]] <- sim_output
      i <- i + 1
      if (verbose == TRUE) {
        print(paste("Island replicate ", i, sep = ""))
      }
    } else {
      message("Simulation attempt ", tried, " dropped: true_Mt is smaller than 2x2.")
    }
  }

  return(island_replicates = island_replicates)

}
