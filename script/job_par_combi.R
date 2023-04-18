args <- commandArgs(TRUE)
param_set <- as.numeric(args[1])

load("~/specmutual/script/network.RData")
mutualism_pars_set1 <- create_mutualism_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(1.0, 1.0, 0.001, 0.001),
  K_pars = c(10, 10, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(2.0, 2.0, 1.0, 1.0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.01,
  M0 = network,
  transprob = 1.0
)

mutualism_pars_set2 <- create_mutualism_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(1.0, 1.0, 0.001, 0.001),
  K_pars = c(10, 10, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(2.0, 2.0, 1.0, 1.0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.1,
  M0 = network,
  transprob = 1.0
)

mutualism_pars_set3 <- create_mutualism_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(1.0, 1.0, 0.001, 0.001),
  K_pars = c(10, 10, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(2.0, 2.0, 1.0, 1.0),
  qgain = 0.005,
  qloss = 0.005,
  lambda0 = 0.1,
  M0 = network,
  transprob = 1.0
)

mutualism_pars_set4 <- create_mutualism_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(1.0, 1.0, 0.001, 0.001),
  K_pars = c(20, 20, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(2.0, 2.0, 1.0, 1.0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.1,
  M0 = network,
  transprob = 1.0
)

mutualism_pars_set5 <- create_mutualism_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(1.0, 1.0, 0.001, 0.001),
  K_pars = c(20, 20, 1.0, 1.0),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(2.0, 2.0, 1.0, 1.0),
  qgain = 0.005,
  qloss = 0.005,
  lambda0 = 0.1,
  M0 = network,
  transprob = 1.0
)

mutualism_pars_pool <- list(
  mutualism_pars_set1,
  mutualism_pars_set2,
  mutualism_pars_set3,
  mutualism_pars_set4,
  mutualism_pars_set5)


set.seed(29)
par_combi <- function(param_set) {
  message("Running param set: ", param_set)
  sim_pars <- mutualism_pars_pool[[param_set]]
  out <- specmutual::sim_mutualism(total_time = 5,
                                   replicates = 1000,
                                   mutualism_pars = sim_pars,
                                   sample_freq = 25,
                                   verbose = TRUE)
  return (out)
}

outs_par_combi <- par_combi(param_set = param_set)
path <- paste0("~/specmutual/result/outs_par_combi_", param_set, ".RData")
save(outs_par_combi, file = path)








