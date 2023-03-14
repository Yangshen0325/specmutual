## parameter set2 from job_differ_coefficient.R using cluster
## with mutualism, coefficient 1.0
load("~/specmutual/script/network.RData")
mutualism_pars_set2 <- list(
  lac_pars = c(0.5, 0.5),
  mu_pars = c(0.2, 0.2, 0.001, 0.001),
  K_pars = c(500, 500, 1.0, 1.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 1.0, 1.0),
  qgain = 1.0,
  qloss = 1.0,
  lambda0 = 1.0,
  M0 = network,
  transprob = 1.0
)

sim_pars <- mutualism_pars_set2
set.seed(1)
out_set2 <- specmutual::peregrine_sim(total_time = 3,
                                  replicates = 100,
                                  mutualism_pars = sim_pars,
                                  verbose = TRUE)
path <- paste0("~/specmutual/result/out_set2.RData")
save(out_set2, file = path)
