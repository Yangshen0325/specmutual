# Only those shown on the island but didn’t have a link between could gain a
# new link, at which Qgain ≠ 0.
Mt <- matrix(sample(c(0, 1), 2500, replace = TRUE), nrow = 50, ncol = 50)
status_p <- matrix(sample(c(0, 1), nrow(Mt), replace = TRUE), ncol = 1, nrow = nrow(Mt))
status_a <- matrix(sample(c(0, 1), ncol(Mt), replace = TRUE), ncol = 1, nrow = ncol(Mt))
both_shown <- status_p %*% t(status_a)
gain_rate <- get_gain_rate(Mt = Mt,
                           status_a = status_a,
                           status_p = status_p,
                           qgain = 1.0)

test_that("Pairs that didn't have a link could gain a new link if they both are
          on the island", {
            expect_equal(which((both_shown * Mt) == 1),
                         which(gain_rate != 0))

          })
