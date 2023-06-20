# job pars 2-5
args <- commandArgs(trailingOnly = TRUE)
param_set <- as.numeric(args[[1]])

library(specmutual)
load("~/specmutual/script/M0.RData")

mutualism_pars_set2 <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.1, 0.1, 0.0001, 0.0001),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.1, 0.1),
  qgain = 0.05,
  qloss = 0.05,
  lambda0 = 0.1,
  M0 = M0,
  transprob = 1.0
)

mutualism_pars_set3 <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.1, 0.1, 0.0001, 0.0001),
  K_pars = c(100, 100, 0.5, 0.5),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.1, 0.1),
  qgain = 0.05,
  qloss = 0.05,
  lambda0 = 0.1,
  M0 = M0,
  transprob = 1.0
)

mutualism_pars_set4 <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.1, 0.1, 0.0001, 0.0001),
  K_pars = c(200, 200, 0.5, 0.5),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.1, 0.1),
  qgain = 0.05,
  qloss = 0.05,
  lambda0 = 0.1,
  M0 = M0,
  transprob = 1.0
)

mutualism_pars_set5 <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.1, 0.1, 0.0001, 0.0001),
  K_pars = c(200, 200, 1.0, 1.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.1, 0.1),
  qgain = 0.05,
  qloss = 0.05,
  lambda0 = 0.1,
  M0 = M0,
  transprob = 1.0
)

mutualism_pars_pool <- list(
  mutualism_pars_set2,
  mutualism_pars_set3,
  mutualism_pars_set4,
  mutualism_pars_set5)

intended_seed <- 13 * param_set + 1
cat(intended_seed, "\n")
set.seed(intended_seed)


par_combo <- function(param_set_local) {
  message("Running param set: ", param_set_local)
  sim_pars <- mutualism_pars_pool[[param_set_local]]
  out <- specmutual::peregrine_sim(total_time = 10,
                                   replicates = 10,
                                   mutualism_pars = sim_pars,
                                   verbose = TRUE)
  path <- paste0("~/specmutual/result/outrep10", param_set_local+1, ".rds")
  saveRDS(out, file = path)
}

outsrep10 <- par_combo(param_set_local = param_set)




