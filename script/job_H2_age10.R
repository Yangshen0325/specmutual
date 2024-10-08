
# K_A1=K_P1=1000, island_age = 10

library(specmutual)
load("~/specmutual/script/M0.RData")


mutualism_pars <- create_mutual_pars(
  lac_pars = c(0.3, 0.3),
  mu_pars = c(0.1, 0.1, 0, 0),
  K_pars = c(50, 50, 1000, 1000),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0, 0),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.005,
  M0 = M0,
  transprob = 1.0,
  alpha = 100)

for (i in 1:100) {

  # Run a replicate simulation
  out2 <- specmutual::peregrine_sim(total_time = 10,
                                    replicates = 1,
                                    mutualism_pars = mutualism_pars,
                                    verbose = TRUE)

  # And save it
  #path <- paste0("/Users/yangshen/Downloads/temp", param_set_local, "_", i, ".rds")
  path <- paste0("~/specmutual/result/out2", "_", i, ".rds")
  saveRDS(out2, file = path)
}
