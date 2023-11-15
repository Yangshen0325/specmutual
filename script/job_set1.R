library(specmutual)
load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0.1, 0.1, 0, 0),
  K_pars = c(50, 50, 1000, 1000),
  gam_pars = c(0.015, 0.0075),
  laa_pars = c(0.5, 0.5, 0, 0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)

set.seed(5)
for (i in 1:100) {

  # Run a replicate simulation
  set <- specmutual::peregrine_sim(total_time = 5,
                                   replicates = 1,
                                   mutualism_pars = mutualism_pars,
                                   verbose = TRUE)

  # And save it
  path <- paste0("~/specmutual/result/set5", "_", i, ".rds")
  saveRDS(set, file = path)

}
