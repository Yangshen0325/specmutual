# Randomly generate Mt, make sure that all plant and animal species are all
# shown on the island. Then only those two have links between them could have
# cospeciation rates. Thus, where Mt = 0, cospeciation rate =0.
Mt <- matrix(sample(c(0, 1), 2500, replace = TRUE), nrow = 50, ncol = 50)

cospec_rate <- get_cospec_rate(Mt = Mt,
                               status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
                               status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
                               lambda0 = 2.0,
                               K_pars = c(500, 500, 1.0, 1.0))
test_that("There is no cospeciation rates if there is no link between plant and
          animal species", {
            expect_equal(which(Mt == 0),
                         which(cospec_rate == 0)
            )}
)

