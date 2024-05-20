# robustness pipeline， test how many failed




mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.3, 0.3),
  mu_pars = c(0.1, 0.1, 0, 0),
  K_pars = c(50, 50, 50, 50),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0, 0),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.005,
  M0 = M0,
  transprob = 1.0,
  alphaa = 100
)

run_robustness <- function(mutualism_pars,
                           replicates,
                           distance_method = "abs",
                           save_output = TRUE,
                           test = FALSE) {

  passed_novel_mls <- list()
  failed_novel_mls <- list()
  passed_novel_sims <- list()
  failed_novel_sims <- list()
  passed_oceanic_mls <- list()
  failed_oceanic_mls <- list()
  passed_oceanic_sims_1 <- list()
  passed_oceanic_sims_2 <- list()
  failed_oceanic_sims <- list()

  while (length(passed_novel_mls) < replicates) {
    novel_sim <- sim_mutualism(
      total_time = 5,
      replicates = 1,
      mutualism_pars = mutualism_pars,
      sample_freq = Inf,
      cond_p = 5,
      cond_a = 5,
      verbose = FALSE
    )

    # set plant as an example
    several_islands_plants <- novel_sim[["island_segments"]][["several_islands_plant"]]
    novel_sim <- several_islands_plants
    names(novel_sim[[1]][[1]])[names(novel_sim[[1]][[1]]) == "not_present_p"] <- "not_present"
    names(novel_sim[[1]][[1]])[names(novel_sim[[1]][[1]]) == "stt_all_plant"] <- "stt_all"

    k_approx <- DAISIErobustness:::calc_max_spec(novel_sim) + 30

    novel_ml <- DAISIE::DAISIE_ML_IW(datalist = novel_sim,
                                      initparsopt = c(0.05, 0.05, 33, 0.0001, 0.05),
                                      parsfix = NULL,
                                      idparsfix = NULL,
                                      ddmodel = 21,
                                      cond = 5)

    # check if MLE passed or errored
    novel_ml_constraints <- DAISIErobustness:::ml_constraints(ml = novel_ml)

    if (novel_ml_constraints == TRUE) {
      message("novel_ml: ", sapply(X = novel_ml, FUN = paste0, " "))

      oceanic_sim_1 <- DAISIE:::DAISIE_sim_cr(
        time = 5,
        M = 135,
        divdepmodel = "IW",
        pars = as.numeric(novel_ml[1:5]),
        replicates = 1,
        nonoceanic_pars = c(0, 0),
        sample_freq = Inf,
        cond = 5,
        plot_sims = FALSE,
        verbose = FALSE
      )



    }
  }
}
