## set2 in job_differ_coefficient.R, OUT_OF_MEMERY.
## here I seperate them for 250 steps
## Below is out1

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
set.seed(28)
out1 <- specmutual::peregrine_sim(total_time = 5,
                                  replicates = 250,
                                  mutualism_pars = sim_pars,
                                  verbose = TRUE)
path <- paste0("~/specmutual/result/out1.RData")
save(out1, file = path)
