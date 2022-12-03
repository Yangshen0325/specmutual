mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(sample(c(0, 1), 1000, replace = TRUE), ncol = 10, nrow = 100),
  transprob = 1)
output_one <- peregrine_sim(total_time = 1,
                            replicates = 2,
                            mutualism_pars = mutualism_pars,
                            verbose = TRUE)
dir.create("outputs")
save(output_one, file = "outputs/output_one.RData")

