test_that("get_pans_cmps has the right format", {
  expect_true(
    test_format_pans_cmps(
      pans_cmps_list = get_pans_cmps(
        Mt = matrix(sample(c(0,1), 20, replace = TRUE), ncol = 4, nrow = 5),
        status_p = matrix(1, ncol = 1, nrow = 5),
        status_a = matrix(1, ncol = 1, nrow = 4)),
      status_p = matrix(1, ncol = 1, nrow = 5),
      status_a = matrix(1, ncol = 1, nrow = 4))
  )
})

test_that("mutualistic partners and compeititors are calculated right", {

  Mt <- matrix(sample(c(0,1), 20, replace = TRUE), ncol = 4, nrow = 5)
  status_p <- matrix(1, ncol = 1, nrow = 5)
  status_a <- matrix(1, ncol = 1, nrow = 4)

  competitors <- function(Mt){
    cmps <- rep(0, NROW(Mt))
    for (i in seq(NROW(Mt))){
      row_cmps <- rep(0, NROW(Mt))
      for (j in seq(NROW(Mt))){
        if (j != i){
          row_cmps[j] <- (sum(Mt[i, ] * Mt[j, ])) >= 1
        }
      }
      cmps[i] <- sum(row_cmps)
    }
    return(cmps)
  }

  expect_equal(
    get_pans_cmps(
      Mt = Mt,
      status_p = status_p,
      status_a = status_a
    ),
    list(pans_p = rowSums(Mt),
         pans_a = colSums(Mt),
         cmps_p = competitors(Mt = Mt),
         cmps_a = competitors(Mt = t(Mt))
    )
  )
})
