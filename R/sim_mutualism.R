#' @title sim_mutualism
#' @description Simulates mutualistic interactions over macro-evolutionary time with specified parameters.
#'
#' @param total_time Numeric. Total time for the simulation.
#' @param replicates Integer. Number of replicates to run.
#' @param mutualism_pars List. Parameters used in the simulation.
#' @param sample_freq Numeric. Frequency of sampling data, fpr plotting.
#' @param cond_p Integer. Condition for plants (e.g., minimum number of present plants on the island).
#' @param cond_a Integer. Condition for animals (e.g., minimum number of present animals on the island).
#' @param verbose Logical. If TRUE, prints progress updates.
#'
#' @return A list containing:
#'   - island_replicates: List of island replicates from simulations.
#'   - island_segments: Formatted data for plants and animals separated.
#'   - island_total: Formatted data for plants and animals combined.
#'
#' @export sim_mutualism
#'
#'
sim_mutualism <- function(total_time,
                          replicates,
                          mutualism_pars,
                          sample_freq,
                          cond_p,
                          cond_a,
                          verbose) {
  # Initialize list to store island replicates
  island_replicates <- vector("list", replicates)

  for (rep in seq_len(replicates)) {
    # Check conditions for presence of plants and animals
    number_present_p <- ifelse(cond_p == 0 & cond_a == 0, -1, 0)
    number_present_a <- ifelse(cond_p == 0 & cond_a == 0, -1, 0)

    # Simulation loop until conditions are met
    while (number_present_p < cond_p | number_present_a < cond_a) {

      island_replicates[[rep]] <- sim_core_mutualism(
        total_time = total_time,
        mutualism_pars = mutualism_pars
      )

      # Extract the "island" information. This island has the same structure as it is in DAISIE
      temp_island <- island_replicates[[rep]][["island"]]

      # Calculate present clades on the island
      number_present_p <- length(temp_island[["clades_info_plant"]])
      number_present_a <- length(temp_island[["clades_info_animal"]])
    }

    # Verbose output for progress tracking
    if (verbose) {
      message(paste("Island replicate", rep))
    }
  }

  # M0 will be used to format the island information
  M0 <- mutualism_pars$M0

  # Format data for plants and animals separately
  island_segments <- format_island_mutual_pa(
    island_replicates = island_replicates,
    total_time = total_time,
    sample_freq = sample_freq,
    M0 = M0,
    verbose = verbose
  )

  # Format combined data for plants and animals
  island_total <- format_island_mutual_all(
    island_replicates = island_replicates,
    total_time = total_time,
    sample_freq = sample_freq,
    M0 = M0,
    verbose = verbose
  )

  return(list(
    island_replicates = island_replicates,
    island_segments = island_segments,
    island_total = island_total
  ))
}
