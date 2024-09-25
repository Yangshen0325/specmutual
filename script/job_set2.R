library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.015, 0.0075),
  laa_pars = c(0.5, 0.5, 1.0, 1.0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.5,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)

set.seed(2)
set2 <- peregrine_sim(total_time = 5,
                      replicates = 100,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(set2, file="~/specmutual/result/set2.rds")
