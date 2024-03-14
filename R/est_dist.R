## estimated parameters distribution, robustness
# mutualism_pars <- create_mutualism_pars(
#   lac_pars = c(0.3, 0.3),
#   mu_pars = c(0.1, 0.1, 0, 0),
#   K_pars = c(50, 50, 50, 50),
#   gam_pars = c(0.05, 0.05),
#   laa_pars = c(0.5, 0.5, 0, 0),
#   qgain = 0.001,
#   qloss = 0.001,
#   lambda0 = 0.005,
#   M0 = M0,
#   transprob = 1.0,
#   alphaa = 100)

#' @title est_dist
#' @export est_dist

est_dist <- function(loops) {
  passed_novel_mls <- list()

  while (length(passed_novel_mls) < loops) {
    sim_output <- sim_mutualism(
      total_time = 5,
      replicates = 1,
      mutualism_pars = mutualism_pars,
      sample_freq = Inf,
      cond_p = 5,
      cond_a = 5,
      verbose = TRUE
    )
    plant_sim <- sim_output[["island_segments"]][[1]]
    k_approx <- DAISIErobustness:::calc_max_spec(plant_sim) + 1

    novel_ml <- DAISIErobustness:::calc_ml(
      sim = plant_sim,
      initial_parameters = c(0.05, 0.05, k_approx, 0.0001, 0.05)
    )

    novel_ml_constraints <- DAISIErobustness:::ml_constraints(ml = novel_ml)
    if (novel_ml_constraints == TRUE) {
      passed_novel_mls[[length(passed_novel_mls) + 1]] <- novel_ml
    }
  }
  return(passed_novel_mls)
}

#est_dist_list <- est_dist(loops=10)

#data_list<- dplyr:::bind_rows(est_dist_list)






