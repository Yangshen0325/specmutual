# without mutualism, should sample the same ID for plant and animal species
test_that("sampled species with same ID for plant and animal", {

  rates1 <- update_rates_mutualism(
    Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
    status_p = matrix(1, ncol = 1, nrow = 5),
    status_a = matrix(1, ncol = 1, nrow = 5),
    mutualism_pars = create_mutualism_pars(
      lac_pars = c(2, 0),
      mu_pars = c(2, 0, 0, 0),
      K_pars = c(Inf, Inf, Inf, Inf),
      gam_pars = c(1, 0),
      laa_pars = c(3, 0, 0, 0),
      qgain = 0,
      qloss = 0,
      lambda0 = 0,
      M0 = {set.seed(1); matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5)},
      transprob = 0),
    island_spec = c())
  rates2 <- update_rates_mutualism(
    Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
    status_p = matrix(1, ncol = 1, nrow = 5),
    status_a = matrix(1, ncol = 1, nrow = 5),
    mutualism_pars = create_mutualism_pars(
      lac_pars = c(0, 2),
      mu_pars = c(0, 2, 0, 0),
      K_pars = c(Inf, Inf, Inf, Inf),
      gam_pars = c(0, 1),
      laa_pars = c(0, 3, 0, 0),
      qgain = 0,
      qloss = 0,
      lambda0 = 0,
      M0 = {set.seed(1); matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5)},
      transprob = 0),
    island_spec = c())
  set.seed(123)
  sample_event_mutualism(rates = rates1)
  set.seed(123)
  sample_event_mutualism(rates = rates2)

})

output <- reshape2::melt(setNames(rates, seq_along(rates)))
cnames <- c("plant", "animal", "rate", "event")
colnames(output) <- cnames
output$event <-as.integer(output$event)

onlyplant <- output[which(output$event < 5)]
onlyanimal <- output[which(output$event > 5 & output$event < 9), ]
x <- sample(1:dim(output)[1],
            size = 1,
            replace = FALSE,
            prob = unlist(rates))
possible_event <- output[x,]

return(possible_event)
}

