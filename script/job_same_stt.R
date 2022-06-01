

mutualism_pars_plant <- list(
  lac_pars = c(0.2, 0),
  mu_pars = c(0.2, 0, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.1, 0),
  laa_pars = c(0.3, 0, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0
)

mutualism_pars_animal <- list(
  lac_pars = c(0, 0.2),
  mu_pars = c(0, 0.2, 0, 0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0, 0.1),
  laa_pars = c(0, 0.3, 0, 0),
  qgain = 0,
  qloss = 0,
  lambda0 = 0,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0
)

stt_list <- specmutual::simulation_test(
  simtime = 2,
  mutualism_pars1 = mutualism_pars_plant,
  mutualism_pars2 = mutualism_pars_animal
)

dir.create("result")
save(stt_list, file = "result/stt_list.RData")
