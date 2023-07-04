library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.2, 0.2, 0, 0),
  K_pars = c(Inf, Inf, 0, 0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(1.0, 1.0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = M0,
  transprob = 1
)

set.seed(12)
par1 <- peregrine_sim(total_time = 5,
                      replicates = 500,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(par1, file="~/specmutual/result/par1.rds")







