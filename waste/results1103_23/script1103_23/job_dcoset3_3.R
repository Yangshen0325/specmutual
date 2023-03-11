## set3 in job_differ_coefficient.R, OUT_OF_MEMERY.
## here I seperate them for 300, 300 and 400 steps
## Below is out3
## with mutualism, coefficient 2.0
load("~/specmutual/script/network.RData")
mutualism_pars_set1 <- list(
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

sim_pars <- mutualism_pars_set1
set.seed(28)
out3 <- specmutual::peregrine_sim(total_time = 5,
                                  replicates = 400,
                                  mutualism_pars = sim_pars,
                                  verbose = TRUE)
path <- paste0("~/specmutual/result/out3.RData")
save(out3, file = path)
