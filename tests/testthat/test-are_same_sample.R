M0 <- matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
island_spec <- matrix(NA, nrow = nrow(M0) + ncol(M0), ncol = 8)
island_spec[1:nrow(M0), 8] <- "plant"
island_spec[(nrow(M0) + 1):(nrow(M0) + ncol(M0)), 8] <- "animal"
island_spec[, 4] <- "I"
island_spec[, 1] <- rep(c(1:5), 2)

rates1 <- update_rates_mutualism(
  M0 = M0,
  Mt = M0,
  status_p = matrix(1, ncol = 1, nrow = nrow(M0)),
  status_a = matrix(1, ncol = 1, nrow = ncol(M0)),
  lac_pars = c(2.0, 0.0),
  mu_pars = c(0.5, 0.0, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.05, 0.0),
  laa_pars = c(0.3, 0.0, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  transprob = 0.0,
  island_spec = island_spec
)
rates2 <- update_rates_mutualism(
  M0 = M0,
  Mt = M0,
  status_p = matrix(1, ncol = 1, nrow = nrow(M0)),
  status_a = matrix(1, ncol = 1, nrow = ncol(M0)),
  lac_pars = c(0.0, 2.0),
  mu_pars = c(0.0, 0.5, 0.0, 0.0),
  K_pars = c(Inf, Inf, Inf, Inf),
  gam_pars = c(0.0, 0.05),
  laa_pars = c(0.0, 0.3, 0.0, 0.0),
  qgain = 0.0,
  qloss = 0.0,
  lambda0 = 0.0,
  transprob = 0.0,
  island_spec = island_spec
)
test_that("sampled species with same ID for plant and animal", {
  plant_exclu <- sum(unlist(rates1[c(5:11)]))
  animal_exclu <- sum(unlist(rates2[c(1:4)])) + sum(unlist(rates2[c(9:11)]))
  if (plant_exclu != 0 | animal_exclu != 0) {
    print("incorrect")
  } else {
    rates1 <- rates1[-c(5:11)]
    rates2 <- rates2[c(5:8)]
    event <- function(rates) {
      output <- reshape2::melt(setNames(rates, seq_along(rates)))
      x <- sample(1:dim(output)[1],
        size = 1,
        replace = FALSE,
        prob = unlist(rates)
      )
      possible_event <- output[x, ]

      return(possible_event)
    }
    expect_equal(
      {
        set.seed(123)
        event(rates = rates1)
      },
      {
        set.seed(123)
        event(rates = rates2)
      }
    )
  }
})
