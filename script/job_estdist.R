#changing K_A1 and K_P1, seq(0, 1000, 50)

args <- commandArgs(trailingOnly = TRUE)
param_set <- as.numeric(args[[1]])

library(specmutual)
load("~/specmutual/script/M0.RData")

K1 <- seq(0, 1000, 50)
mutualism_pool <- list()
for (i in seq(length(K1))) {
  mutualism_pars <- create_mutualism_pars(
    lac_pars = c(0.3, 0.3),
    mu_pars = c(0.1, 0.1, 0, 0),
    K_pars = c(50, 50, K1[i], K1[i]),
    gam_pars = c(0.05, 0.05),
    laa_pars = c(0.5, 0.5, 0, 0),
    qgain = 0.001,
    qloss = 0.001,
    lambda0 = 0.005,
    M0 = M0,
    transprob = 1.0,
    alphaa = 100)
  mutualism_pool[[i]] <- mutualism_pars
}

intended_seed <- param_set + 1
cat(intended_seed, "\n")
set.seed(intended_seed)

habrok_est_dist <- function(param_set_local) {
  message("Running param set: ", param_set_local)
  sim_pars <- mutualism_pool[[param_set_local]]
  passed_novel_mls <- est_dist(loops=1000)
  path <- paste0("~/specmutual/result/out_", param_set_local, ".rds")
  saveRDS(passed_novel_mls, file = path)
}

outs <- habrok_est_dist(param_set_local = param_set)



