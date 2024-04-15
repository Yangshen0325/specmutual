# Only those shown on the island but didn’t have a link between could gain a
# new link, at which Qgain ≠ 0.
Mt <- matrix(sample(c(0, 1), 2500, replace = TRUE), nrow = 50, ncol = 50)
status_p <- matrix(sample(c(0, 1), nrow(Mt), replace = TRUE), ncol = 1, nrow = nrow(Mt))
status_a <- matrix(sample(c(0, 1), ncol(Mt), replace = TRUE), ncol = 1, nrow = ncol(Mt))
gain_rate <- get_gain_rate(
  Mt = Mt,
  status_a = status_a,
  status_p = status_p,
  qgain = 1.0
)

test_that("Pairs that didn't have a link could gain a new link if they both are
          on the island", {
  rates_pos <- which(gain_rate != 0, arr.ind = TRUE)
  expect_true(sum(Mt[rates_pos]) == 0)
  expect_equal(
    sort(unique(rates_pos[, 1])),
    which(status_p == 1)
  )
  expect_equal(
    sort(unique(rates_pos[, 2])),
    which(status_a == 1)
  )
})
