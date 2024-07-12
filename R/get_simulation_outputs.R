
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
                                      lac0,
                                      mu0,
                                      K0,
                                      gam0,
                                      laa0,
                                      effect,
                                      prefix,
                                   total_time,
                                   replicates) {


  check_and_create_folder(the_path, prefix)

  params <- create_mutualism_pars(
    lac_pars = c(lac0, lac0),
    mu_pars = c(mu0, mu0, mutualism_effects[[effect]]$mu1, mutualism_effects[[effect]]$mu1),
    K_pars = c(K0, K0, mutualism_effects[[effect]]$K1, mutualism_effects[[effect]]$K1),
    gam_pars = c(gam0, gam0),
    laa_pars = c(laa0, laa0, mutualism_effects[[effect]]$laa1, mutualism_effects[[effect]]$laa1),
    qgain = qgain,
    qloss = qloss,
    lambda0 = as.numeric(mutualism_effects[[effect]]$lambda_0),
    M0 = M0,
    transprob = transprob,
    alphaa = alphaa
  )

  ## set seed
  set_random_seed <- function() {
    seed <- as.integer(runif(1, min = 1, max = .Machine$integer.max))
    set.seed(seed)
    return(seed)
  }
  seed <- set_random_seed()
  cat("Random seed set to:", seed, "\n")


    message("Running param set: ", prefix, "_", effect)

    out <- peregrine_sim(total_time = total_time,
                        replicates = replicates,
                        mutualism_pars = params,
                        verbose = TRUE)

    save_parameters(out, the_path, prefix, effect)

  }







