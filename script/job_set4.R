library(specmutual)
load("~/specmutual/script/artM.rds")
mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 100.0, 100.0),
  gam_pars = c(0.015, 0.0075),
  laa_pars = c(0.5, 0.5, 100.0, 100.0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.05,
  M0 = artM,
  transprob = 1.0,
  alphaa = 20
)
set.seed(24)

for (i in 1:100) {

  # Run a replicate simulation
  set <- specmutual::peregrine_sim(total_time = 5,
                                    replicates = 1,
                                    mutualism_pars = mutualism_pars,
                                    verbose = TRUE)

  # And save it
  path <- paste0("~/specmutual/result/set", "_", i, ".rds")
  saveRDS(set, file = path)

}

