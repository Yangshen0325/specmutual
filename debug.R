library(specmutual)

set.seed(2)

load("script/M0.RData")

mutualism_pars <- create_mutualism_pars(
  lac_pars = c(0.6, 0.6),
  mu_pars = c(0.1, 0.1, 0, 0),
  K_pars = c(50, 50, 100, 100),
  gam_pars = c(0.05, 0.05),
  laa_pars = c(0.5, 0.5, 0, 0),
  qgain = 0.01,
  qloss = 0.01,
  lambda0 = 0.05,
  M0 = M0,
  transprob = 1.0,
  alphaa = 100
)

valid_rep <- 0
for (rep in 1:100) {
  X <- sim_core_mutualism(total_time = 5.0, mutualism_pars = mutualism_pars, max_steps = 1000)
  if (is.null(X)) {
    # sim_core_mutualism `failed`...
  }
  else {
    # great, we have a completed simulation
    valid_rep <- valid_rep + 1
    # do something with it...
  }
}
print(c("valid replicas: ", valid_rep))
