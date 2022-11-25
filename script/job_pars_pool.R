args <- commandArgs(TRUE)
param_set <- as.numeric(args[1])

mutualism_pars_set1 <- list(
  lac_pars = c(0.2, 0.2),
  mu_pars = c(0.5, 0.5, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set2 <- list(
  lac_pars = c(0.2, 0.2),
  mu_pars = c(0.5, 0.5, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.1, 0.1),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set3 <- list(
  lac_pars = c(0.2, 0.2),
  mu_pars = c(0.5, 0.5, 0.0, 0.0),
  K_pars = c(100, 100, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set4 <- list(
  lac_pars = c(0.4, 0.4),
  mu_pars = c(0.5, 0.5, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set5 <- list(
  lac_pars = c(0.2, 0.2),
  mu_pars = c(0.8, 0.8, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set6 <- list(
  lac_pars = c(0.2, 0.2),
  mu_pars = c(0.5, 0.5, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_set7 <- list(
  lac_pars = c(1.2, 1.2),
  mu_pars = c(0.6, 0.6, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.02, 0.02),
  laa_pars = c(1.2, 1.2, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = matrix(
    sample(c(0, 1), 10000, replace = TRUE),
    ncol = 100,
    nrow = 100
  ),
  transprob = 1.0
)

mutualism_pars_pool <- list(
  mutualism_pars_set1,
  mutualism_pars_set2,
  mutualism_pars_set3,
  mutualism_pars_set4,
  mutualism_pars_set5,
  mutualism_pars_set6,
  mutualism_pars_set7
)

out_parpool <- function(param_set) {
  set.seed(param_set)
  message("Running param set: ", param_set)
  sim_pars <- mutualism_pars_pool[[param_set]]
  out <- specmutual::peregrine_sim(total_time = 5,
                                   replicates = 1000,
                                   mutualism_pars = sim_pars,
                                   verbose = TRUE)
  return (out)
}

  outs_parpool <- out_parpool(param_set = param_set)
  path <- paste0("~/specmutual/result/out_parpool", param_set, ".RData")
  save(outs_parpool, file = path)

