library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0.1, 0.1),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.5,
  M0 = M0,
  transprob = 1.0
)

set.seed(12)
par1 <- peregrine_sim(total_time = 5,
                      replicates = 500,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(par1, file="~/specmutual/result/par1.rds")







