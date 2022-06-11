args <- commandArgs(TRUE)
param_set <- as.numeric(args[1])

## create input parameter sets
# set1 with 500 initial carrying capacity for each guilds.
# mutualism_pars_set1 <- list(
#   lac_pars = c(0.1, 0.1),
#   mu_pars = c(0.2, 0.2, 0.5, 0.5),
#   K_pars = c(500, 500, 0.5, 0.5),
#   gam_pars = c(0.3, 0.3),
#   laa_pars = c(0.2, 0.2, 0.5, 0.5),
#   qgain = 0.5,
#   qloss = 0.5,
#   lambda0 = 0.3,
#   M0 = matrix(
#     sample(c(0, 1), 2500, replace = TRUE),
#     ncol = 50,
#     nrow = 50
#   ),
#   transprob = 0.5
# )
#
# # set2 with 100 initial carrying capacity for each guilds.
# mutualism_pars_set2 <- list(
#   lac_pars = c(0.1, 0.1),
#   mu_pars = c(0.2, 0.2, 0.5, 0.5),
#   K_pars = c(100, 100, 0.5, 0.5),
#   gam_pars = c(0.3, 0.3),
#   laa_pars = c(0.2, 0.2, 0.5, 0.5),
#   qgain = 0.5,
#   qloss = 0.5,
#   lambda0 = 0.3,
#   M0 = matrix(
#     sample(c(0, 1), 2500, replace = TRUE),
#     ncol = 50,
#     nrow = 50
#   ),
#   transprob = 0.5
# )
#
# # set3 with 0.6 cospeciation per capita rate
# # compare with set1 with 0.1 cospeciation per capita rate
# mutualism_pars_set3 <- list(
#   lac_pars = c(0.1, 0.1),
#   mu_pars = c(0.2, 0.2, 0.5, 0.5),
#   K_pars = c(500, 500, 0.5, 0.5),
#   gam_pars = c(0.3, 0.3),
#   laa_pars = c(0.2, 0.2, 0.5, 0.5),
#   qgain = 0.5,
#   qloss = 0.5,
#   lambda0 = 0.6,
#   M0 = matrix(
#     sample(c(0, 1), 2500, replace = TRUE),
#     ncol = 50,
#     nrow = 50
#   ),
#   transprob = 0.5
# )
#
# # set4 with 0.5 immigration per capita rate for each guilds
# # compare with set1 with 0.3 immigration per capita rate
# mutualism_pars_set4 <- list(
#   lac_pars = c(0.1, 0.1),
#   mu_pars = c(0.2, 0.2, 0.5, 0.5),
#   K_pars = c(500, 500, 0.5, 0.5),
#   gam_pars = c(0.5, 0.5),
#   laa_pars = c(0.2, 0.2, 0.5, 0.5),
#   qgain = 0.5,
#   qloss = 0.5,
#   lambda0 = 0.6,
#   M0 = matrix(
#     sample(c(0, 1), 2500, replace = TRUE),
#     ncol = 50,
#     nrow = 50
#   ),
#   transprob = 0.5
# )

# set5 with Inf carring capacity for each guilds
# compare with set1 with 500
mutualism_pars_set5 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(Inf, Inf, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set6 with 50 carring capacity for each guilds
# compare with set1 with 500
mutualism_pars_set6 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set7 with 1.0 cladogenesis percapita rate for each guilds
# compare with set6 with 0.1
mutualism_pars_set7 <- list(
  lac_pars = c(1.0, 1.0),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set8 with 1.0 all mutualism-related coefficients for each guilds
# compare with set6 with 0.5
mutualism_pars_set8 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 1.0, 1.0),
  K_pars = c(50, 50, 1.0, 1.0),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 1.0, 1.0),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# # set9 with 1.0 anagenesis for each guilds
# # compare with set6 with 0.2
# mutualism_pars_set9 <- list(
#   lac_pars = c(0.1, 0.1),
#   mu_pars = c(0.2, 0.2, 0.5, 0.5),
#   K_pars = c(50, 50, 0.5, 0.5),
#   gam_pars = c(0.3, 0.3),
#   laa_pars = c(1.0, 1.0, 0.5, 0.5),
#   qgain = 0.5,
#   qloss = 0.5,
#   lambda0 = 0.3,
#   M0 = matrix(
#     sample(c(0, 1), 2500, replace = TRUE),
#     ncol = 50,
#     nrow = 50
#   ),
#   transprob = 0.5
# )

# set9 with 1.0 immigration for each guilds
# compare with set6 with 0.3
mutualism_pars_set9 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(1.0, 1.0),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set10 with 1.0 extinction for each guilds
# compare with set6 with 0.2
mutualism_pars_set10 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(1.0, 1.0, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set11 with 1.0 cladogenesis per capita rate for plant
# compare with set6 with 0.1
mutualism_pars_set11 <- list(
  lac_pars = c(1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set12 with 1.0 immigration for plant
# compare with set6 with 0.3
mutualism_pars_set12 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(1.0, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set13 with 1.0 extinction for plant
# compare with set6 with 0.2
mutualism_pars_set13 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(1.0, 0.2, 0.5, 0.5),
  K_pars = c(50, 50, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.3,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

mutualism_pars_pool <- list(
  mutualism_pars_set5,
  mutualism_pars_set6,
  mutualism_pars_set7,
  mutualism_pars_set8,
  mutualism_pars_set9,
  mutualism_pars_set10,
  mutualism_pars_set11,
  mutualism_pars_set12,
  mutualism_pars_set13
)

out_sim_mutualism <- function(param_set){
  message("Running param set: ", param_set)
  sim_pars <- mutualism_pars_pool[[param_set]]
  out <- specmutual::sim_mutualism(simtime = 5,
                                   replicates = 50,
                                   mutualism_pars = sim_pars)

  return (out)
}

out <- out_sim_mutualism(param_set = param_set)
path <- paste0("~/specmutual/result/out_", param_set + 4, ".RData")
save(out, file = path)
