# robustness pipeline
run_robustness <- function(mutualism_pars,replicates,
distance_method = "abs",
save_output = TRUE,
test = FALSE) {

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
    alphaa = 100)

  passed_novel_mls <- list()
  failed_novel_mls <- list()
  passed_novel_sims <- list()
  failed_novel_sims <- list()
  passed_oceanic_mls <- list()
  failed_oceanic_mls <- list()
  passed_oceanic_sims_1 <- list()
  passed_oceanic_sims_2 <- list()
  failed_oceanic_sims <- list()
  spec_nltt_error <- c()
  num_spec_error <- c()
  num_col_error <- c()
  endemic_nltt_error <- c()
  nonendemic_nltt_error <- c()
  spec_baseline_nltt_error <- c()
  num_spec_baseline_error <- c()
  num_col_baseline_error <- c()
  endemic_baseline_nltt_error <- c()
  nonendemic_baseline_nltt_error <- c()

  while (length(passed_novel_mls) < replicates) {
    novel_sim <- sim_mutualism(
      total_time=5,
      replicates = 1,
      mutualism_pars=mutualism_pars,
      sample_freq = Inf,
      cond_p = 5,
      cond_a = 5,
      verbose = FALSE
    )
    # set plant as an example
    novel_sim <- novel_sim$island_segments[[1]]
    k_approx <- DAISIErobustness:::calc_max_spec(novel_sim) + 1

     novel_ml <- DAISIErobustness:::calc_ml(
       sim = novel_sim,
       initial_parameters = c(0.05, 0.05, k_approx, 0.0001, 0.05)
     )
     # check if MLE passed or errored
     novel_ml_constraints <- DAISIErobustness:::ml_constraints(ml = novel_ml)

     if (novel_ml_constraints == TRUE) {
       message("novel_ml: ", sapply(X = novel_ml, FUN = paste0, " "))
       # oceanic_sim_1 <- run_oceanic_sim(
       #   ml = novel_ml,
       #   mutualism_pars =  mutualism_pars)

       oceanic_sim_1 <- DAISIE:::DAISIE_sim_cr_iw(
         total_time =5,
         M=135,
         pars=as.numeric(novel_ml[1:5]),
         replicates = 1,
         nonoceanic_pars = c(0, 0),
         sample_freq = Inf,
         hyper_pars = DAISIE:::create_hyper_pars(d = 0, x = 0),
         area_pars = DAISIE:::create_area_pars(
           max_area = 1,
           current_area = 1,
           proportional_peak_t = 0,
           total_island_age = 0,
           sea_level_amplitude = 0,
           sea_level_frequency = 0,
           island_gradient_angle = 0),
         cond=5,
         verbose =FALSE)

       error <- DAISIErobustness:::calc_error(
         sim_1 = novel_sim,
         sim_2 = oceanic_sim_1,
         sim_pars = mutualism_pars,
         replicates = replicates,
         distance_method = distance_method)





     }


  }










}




