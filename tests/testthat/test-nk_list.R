test_that("get_nk has the right format", {
  expect_true(
    test_format_nk(
      nk_list <-  get_nk(Mt = matrix(sample(c(0,1), 20, replace = TRUE), ncol = 4, nrow = 5),
                        status_p = matrix(1, ncol = 1, nrow = 5),
                        status_a = matrix(1, ncol = 1, nrow = 4),
                          K_pars = c(Inf, Inf, Inf, Inf)),
      status_p <- matrix(1, ncol = 1, nrow = 5),
      status_a <- matrix(1, ncol = 1, nrow = 4)
    )
  )
})
