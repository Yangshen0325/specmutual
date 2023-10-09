library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.3, 0.3),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(20, 20, 1.0, 1.0),
  gam_pars = c(0.008, 0.0004),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)

set.seed(1)
set1 <- peregrine_sim(total_time = 5,
                      replicates = 100,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(set1, file="~/specmutual/result/set1.rds")
