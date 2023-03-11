## parameter set3 from job_differ_coefficient.R using cluster
## with mutualism, coefficient 2.0
load("~/specmutual/script/network.RData")
mutualism_pars_set3 <- list(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0.002, 0.002),
  K_pars = c(500, 500, 2.0, 2.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 2.0, 2.0),
  qgain = 1.0,
  qloss = 1.0,
  lambda0 = 2.0,
  M0 = network,
  transprob = 1.0
)

sim_pars <- mutualism_pars_set3
set.seed(28)
out_set3 <- specmutual::peregrine_sim(total_time = 1,
                                  replicates = 1000,
                                  mutualism_pars = sim_pars,
                                  verbose = TRUE)
path <- paste0("~/specmutual/result/out_set3.RData")
save(out_set3, file = path)
