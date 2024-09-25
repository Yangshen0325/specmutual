library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.4, 0.4),
  mu_pars = c(0.2, 0.2, 0.001, 0.001),
  K_pars = c(Inf, Inf, 0.5, 0.5), #mutualism affects immigration with 1.0
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.1, 0.1, 0.5, 0.5),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.5,
  M0 = M0,
  transprob = 1.0
)

set.seed(13)
par2 <- peregrine_sim(total_time = 5,
                      replicates = 500,
                      mutualism_pars = mutualism_pars,
                      verbose = TRUE)
saveRDS(par2, file="~/specmutual/result/par2.rds")
