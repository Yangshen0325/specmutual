test_that("rates are feasible", {
  expect_true(
    are_rates(
      update_rates_mutualism(
        Mt = {set.seed(2);matrix(sample(c(0,1),40,replace = TRUE),ncol=5,nrow=8)},
        status_p = matrix(0, ncol = 1, nrow = 5),
        status_a = matrix(0, ncol = 1, nrow = 4),
        mutualism_pars = create_mutualism_pars(
          lac_pars = c(2, 0),
          mu_pars = c(0, 0, 0, 0),
          K_pars = c(Inf, Inf, Inf, Inf),
          gam_pars = c(1, 0),
          laa_pars = c(1, 0, 0, 0),
          qgain = 0,
          qloss = 0,
          lambda0 = 0,
          M0 = {set.seed(1);matrix(sample(c(0,1),40,replace = TRUE),ncol=5,nrow=8)},
          transprob = 1),
         island_spec = c()
      )
    )
  )
})
