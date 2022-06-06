
## create input parameter sets
# set1 with 500 initial carrying capacity for each guilds.
mutualism_pars_set1 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(500, 500, 0.5, 0.5),
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


# set2 with 100 initial carrying capacity for each guilds.
mutualism_pars_set2 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(100, 100, 0.5, 0.5),
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

# set3 with 0.6 cospeciation per capita rate
# compare with set1 with 0.1 cospeciation per capita rate
mutualism_pars_set3 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(500, 500, 0.5, 0.5),
  gam_pars = c(0.3, 0.3),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.6,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

# set4 with 0.5 immigration per capita rate for each guilds
# compare with set1 with 0.3 immigration per capita rate
mutualism_pars_set4 <- list(
  lac_pars = c(0.1, 0.1),
  mu_pars = c(0.2, 0.2, 0.5, 0.5),
  K_pars = c(500, 500, 0.5, 0.5),
  gam_pars = c(0.5, 0.5),
  laa_pars = c(0.2, 0.2, 0.5, 0.5),
  qgain = 0.5,
  qloss = 0.5,
  lambda0 = 0.6,
  M0 = matrix(
    sample(c(0, 1), 2500, replace = TRUE),
    ncol = 50,
    nrow = 50
  ),
  transprob = 0.5
)

mutualism_pars_pool <- rbind(
  mutualism_pars_set1,
  mutualism_pars_set2,
  mutualism_pars_set3,
  mutualism_pars_set4
)

write.csv2(
  mutualism_pars_pool,
  "data/mutualism_pars_pool.csv",
  row.names = FALSE
)

