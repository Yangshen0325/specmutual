test_that("get_nk has the right format", {
  expect_true(
    test_format_nk(
      nk_list =  get_nk(Mt = matrix(sample(c(0,1), 20, replace = TRUE), ncol = 4, nrow = 5),
                        status_p = matrix(1, ncol = 1, nrow = 5),
                        status_a = matrix(1, ncol = 1, nrow = 4),
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
                          transprob = 0.5)),
      status_p = matrix(1, ncol = 1, nrow = 5),
      status_a = matrix(1, ncol = 1, nrow = 4)
    )
  )
})
