# load M0. it's located in script folder
#load("~/specmutual/script/M0.RData")
mutualism_pars <- create_mutualism_pars(
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

outofmem <- sim_mutualism (total_time =5,
                          replicates =500,
                          mutualism_pars = mutualism_pars,
                          sample_freq = 25,
                          verbose = TRUE)
