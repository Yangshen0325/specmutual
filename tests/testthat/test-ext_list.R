# without mutualism, extinction rates for plant species should be equaling to
# that of animal species (nrow(Mt) = ncol(Mt)).
Mt <- matrix(1, 2500, ncol = 50, nrow = 50)
Mt[lower.tri(Mt)] <- 0
Mt <- Mt[nrow(Mt):1, ]
testit::assert(colSums(Mt) == rowSums(Mt)) # If they are all on the island, they
                                           # have the same amount of partners.

test_that("Extinction rates for plant and animal species are identical
          without mutualism", {
            mu_pars <- c(1.0, 1.0, 0.0, 0.0)
            ext_list <- get_ext_rate(Mt = Mt,
                                     status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
                                     status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
                                     mu_pars = mu_pars)

            expect_equal(
              ext_list$ext_p,
              ext_list$ext_a
            )
          })

test_that("Extinction rates for plant and animal species are identical
          with mutualism",{
            mu_pars <- c(1.0, 1.0, 1.0, 1.0)
            ext_list <- get_ext_rate(Mt = Mt,
                                     status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
                                     status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
                                     mu_pars = mu_pars)

            expect_equal(
              ext_list$ext_p,
              ext_list$ext_a
            )
          })
