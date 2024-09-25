library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0.1, 0.1, 0.01, 0.01),
  K_pars = c(50, 50, 10, 10),
  gam_pars = c(0.015, 0.0075),
  laa_pars = c(0.5, 0.5, 10, 10),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 5,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)

set.seed(3)
set3 <- peregrine_sim(total_time = 5,
                      replicates = 100,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(set3, file="~/specmutual/result/set3.rds")
