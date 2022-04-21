test_that("rates are feasible", {
  expect_true(
    are_rates(
      update_rates_mutualism(
        Mt = {set.seed(2);matrix(sample(c(0,1),20,replace = TRUE),ncol=4,nrow=5)},
        status_p = matrix(0, ncol = 1, nrow = 5),
        status_a = matrix(0, ncol = 1, nrow = 4),
        mutualism_pars = create_mutualism_pars(
          lac_pars = c(2, 2.5),
          mu_pars = c(2, 2, 0.5, 0.5),
          K_pars = c(Inf, Inf, Inf, Inf),
          gam_pars = c(1.2, 1.5),
          laa_pars = c(3, 3, 0.5, 0.5),
          qgain = 0.5,
          qloss = 0.5,
          lambda0 = 0.5,
          M0 = {set.seed(1);matrix(sample(c(0,1),20,replace = TRUE),ncol=4,nrow=5)},
          transprob = 0.5),
        island_spec = c()
      )
    )
  )
})
# I set four types of rate only for plant with lac=2, mu=2, gam=1, laa=3 and without mutualism, DD,
# and set the same only for animal species, expecting them the same output.
# Mt is random generated but have to make nrow(Mt)=ncol(Mt).
test_that("four rates for plant equals to that for animal without mutualism
          and DD", {
  expect_equal(
    unname(update_rates_mutualism(
                Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
                status_p = matrix(1, ncol = 1, nrow = 5),
                status_a = matrix(1, ncol = 1, nrow = 5),
                mutualism_pars = create_mutualism_pars(
                  lac_pars = c(2, 0),
                  mu_pars = c(2, 0, 0, 0),
                  K_pars = c(Inf, Inf, Inf, Inf),
                  gam_pars = c(1, 0),
                  laa_pars = c(3, 0, 0, 0),
                  qgain = 0,
                  qloss = 0,
                  lambda0 = 0,
                  M0 = {set.seed(1); matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5)},
                  transprob = 0),
                island_spec = c())[1:4]),
      unname(update_rates_mutualism(
                Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
                status_p = matrix(1, ncol = 1, nrow = 5),
                status_a = matrix(1, ncol = 1, nrow = 5),
                mutualism_pars = create_mutualism_pars(
                  lac_pars = c(0, 2),
                  mu_pars = c(0, 2, 0, 0),
                  K_pars = c(Inf, Inf, Inf, Inf),
                  gam_pars = c(0, 1),
                  laa_pars = c(0, 3, 0, 0),
                  qgain = 0,
                  qloss = 0,
                  lambda0 = 0,
                  M0 = {set.seed(1); matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5)},
                  transprob = 0),
                island_spec = c())[5:8])
               )
          })
