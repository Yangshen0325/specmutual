test_that("mutualism_pars is feasible", {
  expect_true(
    are_mutualism_pars(
      create_mutual_pars(
        lac_pars = c(2, 0),
        mu_pars = c(0, 0, 0, 0),
        K_pars = c(Inf, Inf, Inf, Inf),
        gam_pars = c(1, 0),
        laa_pars = c(1, 0, 0, 0),
        qgain = 0,
        qloss = 0,
        lambda0 = 0,
        M0 = matrix(sample(c(0, 1), 40, replace = TRUE), ncol = 5, nrow = 8),
        transprob = 1
      )
    )
  )
})
