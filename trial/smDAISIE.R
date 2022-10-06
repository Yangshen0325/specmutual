mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0, 0.2, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1, 0, 1, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5),
  transprob = 0)
