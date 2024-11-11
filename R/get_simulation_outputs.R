
#### Functions for dealing with data

#
#' Get simulation parameters for different scenarios
#'
#' @param the_path save the outputs
#' @param lac0 initial cladogenesis value
#' @param mu0 initial extinction value
#' @param K0 initial carrying capacity
#' @param gam0 initial immigration value
#' @param laa0 initial anagenesis
#' @param effect mutualsim effects, could be "none", "low", "medium", "high"
#' @param prefix parameters combination
#' @param total_time island age
#' @param replicates replicates
#'
#' @return simulation outputs
#' @export

get_simulation_outputs <- function(the_path,
                                    params,
                                      effect,
                                      prefix,
                                   total_time,
                                   replicates) {


  check_and_create_folder(the_path, prefix)

    out <- peregrine_sim(total_time = total_time,
                        replicates = replicates,
                        mutualism_pars = params,
                        return_parts = "island_parts",
                        verbose = TRUE)

    save_parameters(out, the_path, prefix, effect)

  }







