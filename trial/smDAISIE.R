mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 2500, replace = TRUE), ncol = 50, nrow = 50),
  transprob = 1)
set.seed(1)
results <- sim_mutualism(simtime = 1,
              replicates = 2,
              mutualism_pars = mutualism_pars,
              sample_freq = 25,
              plot_sims = TRUE)
