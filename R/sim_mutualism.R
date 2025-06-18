#' @title sim_mutualism
#' @description Simulate mutualistic interactions over macro-evolutionary time with specified parameters.
#'
#' @param total_time Numeric. Total time for the simulation (island age)
#' @param replicates Integer. Number of replicates to run.
#' @param mutualism_pars List. Parameters related to mutualism used in the simulation.
#' @param sample_freq Numeric. Frequency of sampling data, for plotting.
#' @param cond_p Integer. Condition for plants (e.g., minimum number of present plant clades on the island).
#' @param cond_a Integer. Condition for animals (e.g., minimum number of present animal clades on the island).
#' @param verbose Logical. If TRUE, prints progress updates.
#' @param return_parts Character. Specifies which part of the result to return. Options are:
#' - `"island_parts"` (default): Returns only the core simulation results, including the following elements:
#'   - `Mt`: A matrix representing the "false" matrix on the island, including extinct and non-immigrated species occupying
#'            their original indices from the mainland.
#'   - `M_true_list`: A list of "true" matrices left on the island (every certain ages).
#'   - `status_p`: A one column matrix indicating the presence of the plant species
#'   on the island (Presence (1) or absence (0)).
#'   - `status_a`: A one column matrix indicating the presence of the animal species
#'   on the island (Presence (1) or absence (0)).
#'   - `island_spec`: DAISIE-alike data frame. A table showing the evolutionary trajectory of species on the island, including their colonization
#'                    and branching details.
#'   - `island`: DAISIE-alike island information. A list containing three components: `stt_table`, `clades_info_plant`, and `clades_info_animal`.
#'   - `evo_table`: A data frame summarizing the evolutionary history of the species, including branching times and other evolutionary metrics.
#' - `"additional_parts"`: Returns only additional information,for checking the rates and species richness. It contains:
#'   - `rates_list`: A list containing all types of rates
#'   - `timeval_list`: A list containing all values of time
#'   - `richness_p_list`: A list containing all plant richness on the island across simulation time
#'   - `richness_a_list`: A list containing all animal richness on the island across simulation time
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
                          return_parts = "island_parts",
                          verbose) {

  if (return_parts == "island_parts") {
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
          mutualism_pars = mutualism_pars,
          return_parts = return_parts
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
  } else {
    cat("No need to return island outputs, please use `sim_core_mutualism` and specify `return_parts = \"additional_parts\"`.\n")
  }

}
