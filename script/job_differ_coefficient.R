args <- commandArgs(TRUE)
param_set <- as.numeric(args[1])

load("~/specmutual/script/network.RData")
## without mutulaism
mutualism_pars_set1 <- list(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  M0 = network,
  transprob = 1.0
)

## with mutualism, coefficient 1.0
mutualism_pars_set2 <- list(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0.001, 0.001),
  K_pars = c(Inf, Inf, 1.0, 1.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 1.0, 1.0),
  qgain = 1.0,
  qloss = 1.0,
  lambda0 = 1.0,
  M0 = network,
  transprob = 1.0
)

## with mutualism, coefficient 2.0
mutualism_pars_set3 <- list(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0.002, 0.002),
  K_pars = c(Inf, Inf, 2.0, 2.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 2.0, 2.0),
  qgain = 1.0,
  qloss = 1.0,
  lambda0 = 2.0,
  M0 = network,
  transprob = 1.0
)
mutualism_pars_pool <- list(
  mutualism_pars_set1,
  mutualism_pars_set2,
  mutualism_pars_set3)

out_parpool <- function(param_set) {
  set.seed(28)
  message("Running param set: ", param_set)
  sim_pars <- mutualism_pars_pool[[param_set]]
  out <- specmutual::peregrine_sim(total_time = 5,
                                   replicates = 1000,
                                   mutualism_pars = sim_pars,
                                   verbose = TRUE)
  return (out)
}

outs_differ_coefficient <- out_parpool(param_set = param_set)
path <- paste0("~/specmutual/result/outs_differ_coefficient", param_set, ".RData")
save(outs_differ_coefficient, file = path)

