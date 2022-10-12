# without mutualism, immigration rates for plant species should be equaling to
# that of animal species (nrow(M0) = ncol(M0)).
test_that("Immigration rates for plant and animal species are identical
          without mutualism",{
  M0 <- matrix(sample(c(0, 1), 2500, replace = TRUE), ncol = 50, nrow = 50)
  K_pars <- c(500, 500, Inf, Inf)
  gam_pars <- c(1.0, 1.0)
  immig_list <- get_immig_rate(M0 = M0,
                              Mt = M0,
                              K_pars = K_pars,
                              status_a = matrix(1, ncol = 1, nrow = nrow(M0)),
                              status_p = matrix(1, ncol = 1, nrow = ncol(M0)),
                              gam_pars = gam_pars)
  expect_equal(
    immig_list$immig_p,
    immig_list$immig_a
  )
})

# Keep plants and animals have the same number of muutalistic partners
test_that("Immigration rates for plant and animal species are identical
          with mutualism",{
            M0 <- matrix(1, 2500, ncol = 50, nrow = 50)
            M0[lower.tri(M0)] <- 0
            M0 <- M0[nrow(M0):1, ]
            testit::assert(colSums(M0) == rowSums(M0))
            K_pars <- c(500, 500, 1.0, 1.0)
            gam_pars <- c(1.0, 1.0)
            immig_list <- get_immig_rate(M0 = M0,
                                         Mt = M0,
                                         K_pars = K_pars,
                                         status_a = matrix(1, ncol = 1, nrow = nrow(M0)),
                                         status_p = matrix(1, ncol = 1, nrow = ncol(M0)),
                                         gam_pars = gam_pars)

            expect_equal(
              immig_list$immig_p,
              immig_list$immig_a
            )
})
