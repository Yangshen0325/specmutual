# job set2, set3, and set10~set18
args <- commandArgs(trailingOnly = TRUE)
param_set <- as.numeric(args[[1]])

library(specmutual)
load("~/specmutual/script/M0.RData")

#it was set2 in excel sheet
mutualism_pars_set1 <- create_mutual_pars(
  lac_pars = c(1.5, 1.5),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.1, 0.05),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
# it was set3 in excel sheet
mutualism_pars_set2 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.1, 0.05),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set10
mutualism_pars_set3 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.1, 0.1),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set11
mutualism_pars_set4 <- create_mutual_pars(
  lac_pars = c(1.5, 1.5),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.15, 0.15),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set12
mutualism_pars_set5 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.15, 0.15),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set13
mutualism_pars_set6 <- create_mutual_pars(
  lac_pars = c(1.5, 1.5),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set14
mutualism_pars_set7 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set15
mutualism_pars_set8 <- create_mutual_pars(
  lac_pars = c(1.5, 1.5),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.15, 0.075),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set16
mutualism_pars_set9 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.15, 0.075),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set17
mutualism_pars_set10 <- create_mutual_pars(
  lac_pars = c(1.5, 1.5),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.05, 0.025),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)
#it was set18
mutualism_pars_set11 <- create_mutual_pars(
  lac_pars = c(2.0, 2.0),
  mu_pars = c(0.1, 0.1, 0.001, 0.001),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.05, 0.025),
  laa_pars = c(0.5, 0.5, 0.1, 0.1),
  qgain = 0.001,
  qloss = 0.001,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 20
)

mutualism_pars_pool <- list(
  mutualism_pars_set1,
  mutualism_pars_set2,
  mutualism_pars_set3,
  mutualism_pars_set4,
  mutualism_pars_set5,
  mutualism_pars_set6,
  mutualism_pars_set7,
  mutualism_pars_set8,
  mutualism_pars_set9,
  mutualism_pars_set10,
  mutualism_pars_set11)

intended_seed <- 13 * param_set + 1
cat(intended_seed, "\n")
set.seed(intended_seed)

par_combo <- function(param_set_local) {
  message("Running param set: ", param_set_local)
  sim_pars <- mutualism_pars_pool[[param_set_local]]

  # For each replicate...
  for (i in 1:500) {

    # Run a replicate simulation
    out <- specmutual::peregrine_sim(total_time = 5,
                                     replicates = 1,
                                     mutualism_pars = sim_pars,
                                     verbose = TRUE)

    # And save it
    path <- paste0("~/specmutual/result/out", param_set_local, "_", i, ".rds")
    saveRDS(out, file = path)

  }
}

outs <- par_combo(param_set_local = param_set)





