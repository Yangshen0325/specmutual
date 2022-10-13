# without mutualism, anagenesis rates for plant species should be equaling to
# that of animal species (nrow(Mt) = ncol(Mt)).
Mt <- matrix(1, 2500, ncol = 50, nrow = 50)
Mt[lower.tri(Mt)] <- 0
Mt <- Mt[nrow(Mt):1, ]
testit::assert(colSums(Mt) == rowSums(Mt)) # If they are all on the island, they
# have the same amount of partners.
M0 <- matrix(sample(c(0,1), 2500, replace = TRUE), nrow = 50, ncol = 50)
island_spec <- matrix(0, nrow = 100, ncol = 8)
island_spec[, 4] <- "I"
island_spec[1:nrow(M0), 8] <- "plant"
island_spec[(nrow(M0) + 1):nrow(island_spec), 8] <- "animal"

test_that("Anagenesis rates for plant and animal species are identical
          without mutualism", {
            laa_pars <- c(1.0, 1.0, 0.0, 0.0)
            ana_list <- get_ana_rate(M0 = M0,
                                     Mt = Mt,
                                     status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
                                     status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
                                     laa_pars = laa_pars,
                                     island_spec = island_spec)

            expect_equal(
              ana_list$ana_p,
              ana_list$ana_a
            )
          })

test_that("Anagenesis rates for plant and animal species are identical
          with mutualism",{
            laa_pars <- c(1.0, 1.0, 1.0, 1.0)
            ana_list <- get_ana_rate(M0 = M0,
                                     Mt = Mt,
                                     status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
                                     status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
                                     laa_pars = laa_pars,
                                     island_spec = island_spec)

            expect_equal(
              ana_list$ana_p,
              ana_list$ana_a
            )
          })
