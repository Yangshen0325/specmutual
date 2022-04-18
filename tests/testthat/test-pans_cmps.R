test_that("mutualistic partners and compeititors", {

  Mt = {set.seed(1); matrix(sample(c(0,1), 20, replace = TRUE), ncol = 4, nrow = 5)}
  status_p = matrix(1, ncol = 1, nrow = 5)
  status_a = matrix(1, ncol = 1, nrow = 4)

  competitors <- function(Mt){
    cmps <- matrix(0, ncol =1, nrow = NROW(Mt))
    for (i in seq(NROW(Mt))){
      row_cmps <- matrix(0, ncol =1, nrow = NROW(Mt))
      for (j in seq(NROW(Mt))){
        if (j != i){
          row_cmps[j] <- (sum(Mt[i, ] * Mt[j, ])) >= 1
        }
      }
      cmps[i] <- sum(row_cmps)
    }
    return(as.numeric(cmps))
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
