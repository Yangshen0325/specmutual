# without mutualism, cladogenesis rates for plant species should be equaling to
# that of animal species (nrow(Mt) = ncol(Mt)).
Mt <- matrix(1, 2500, ncol = 50, nrow = 50)
Mt[lower.tri(Mt)] <- 0
Mt <- Mt[nrow(Mt):1, ]
testit::assert(colSums(Mt) == rowSums(Mt))
lac_pars <- c(1.0, 1.0)

test_that("Cladogenesis rates for plant and animal species are identical
          without mutualism", {
  K_pars <- c(500, 500, Inf, Inf)
  clado_list <- get_clado_rate(
    Mt = Mt,
    K_pars = K_pars,
    status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
    status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
    lac_pars = lac_pars
  )

  expect_equal(
    clado_list$clado_p,
    clado_list$clado_a
  )
})

# Keep plants and animals have the same number of muutalistic partners
test_that("Cladogenesis rates for plant and animal species are identical
          with mutualism", {
  K_pars <- c(500, 500, 1.0, 1.0)
  clado_list <- get_clado_rate(
    Mt = Mt,
    K_pars = K_pars,
    status_a = matrix(1, ncol = 1, nrow = nrow(Mt)),
    status_p = matrix(1, ncol = 1, nrow = ncol(Mt)),
    lac_pars = lac_pars
  )

  expect_equal(
    clado_list$clado_p,
    clado_list$clado_a
  )
})
