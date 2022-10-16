# loss scenarios (they had link between in the mainland):
# [1] both plant and animal show on the island
# [2] plant present, animal absent;
# [3] plant absent, animal present;
Mt <- matrix(sample(c(0, 1), 2500, replace = TRUE), nrow = 50, ncol = 50)
status_p <- matrix(sample(c(0, 1), nrow(Mt), replace = TRUE), ncol = 1, nrow = nrow(Mt))
status_a <- matrix(sample(c(0, 1), ncol(Mt), replace = TRUE), ncol = 1, nrow = ncol(Mt))
loss_rate <- get_loss_rate(Mt = Mt,
                           status_a = status_a,
                           status_p = status_p,
                           qloss = 2.0)

test_that("Pairs lose a link between based on three scenatios",{
  expect_true(sum(!(which(loss_rate != 0) %in% which(Mt == 1))) == 0)
  expect_true(sum(loss_rate[which(status_p == 0), which(status_a == 0)]) == 0)
})
