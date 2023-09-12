# 2109 job set1~set11
args <- commandArgs(trailingOnly = TRUE)
param_set <- as.numeric(args[[1]])

library(specmutual)
load("~/specmutual/script/M0.RData")

lac_pool <- list(
  c(0.5, 0.5),
  c(0.6, 0.6),
  c(0.7, 0.7),
  c(0.8, 0.8),
  c(0.9, 0.9),
  c(1.0, 1.0),
  c(1.1, 1.1),
  c(1.2, 1.2),
  c(1.3, 1.3),
  c(1.4, 1.4),
  c(1.5, 1.5))

intended_seed <- 13 * param_set + 1
cat(intended_seed, "\n")
set.seed(intended_seed)

par_combo <- function(param_set_local) {
  message("Running param set: ", param_set_local)
  mutualism_pars <- create_mutualism_pars(
    lac_pars = lac_pool[[param_set_local]],
    mu_pars = c(0.5, 0.5, 0.001, 0.001),
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

  # For each replicate...
  for (i in 1:500) {

    # Run a replicate simulation
    mu01 <- specmutual::peregrine_sim(total_time = 5,
                                     replicates = 1,
                                     mutualism_pars = mutualism_pars,
                                     verbose = TRUE)

    # And save it
    path <- paste0("~/specmutual/result/mu01", param_set_local, "_", i, ".rds")
    saveRDS(mu01, file = path)

  }
}

mu01s <- par_combo(param_set_local = param_set)



