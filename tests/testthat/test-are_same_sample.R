# without mutualism, should sample the same ID for plant and animal species
test_that("sampled species with same ID for plant and animal", {
  # I set four types of rate only for plant with lac=2, mu=2, gam=1, laa=3 and without mutualism, DD,
  # and set the same only for animal species, expecting sampled the same ID.
  # Mt is random generated but have to make nrow(Mt)=ncol(Mt).
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
      M0 = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
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
      M0 = matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5),
      transprob = 0),
    island_spec = c())

  possible_event <- function(plantrates, animalrates){
    testrates1 <- plantrates[-c(5:11)]
    testrates2 <- animalrates[c(5:8)]
    output1 <- reshape2::melt(setNames(plantrates, seq_along(plantrates)))
    output2 <- reshape2::melt(setNames(animalrates, seq_along(animalrates)))
    cnames <- c("plant", "animal", "rate", "event")
    colnames(output1) <- cnames
    colnames(output2) <- cnames
    output1$event <-as.integer(output1$event)
    output2$event <-as.integer(output2$event)
    onlyplant <- output1[which(output1$event < 5), ]
    onlyanimal <- output2[which(output2$event > 4 & output2$event < 9), ]
    onlyanimal$animal <- onlyanimal$plant
    set.seed(123)
    x <- sample(1:dim(onlyplant)[1],
                size = 1,
                replace = FALSE,
                prob = unlist(testrates1))
    possible_event1 <- onlyplant[x, ]
    set.seed(123)
    y <- sample(1:dim(onlyanimal)[1],
                size = 1,
                replace = FALSE,
                prob = unlist(testrates2))
    possible_event2 <- onlyanimal[y, ]

    return(list(possible_event1 = possible_event1,
                possible_event2 = possible_event2))
  }
  possible_event <- possible_event(plantrates = rates1, animalrates = rates2)
  possible_event1 <- possible_event[[1]]
  possible_event2 <- possible_event[[2]]
  expect_equal(
    possible_event1$plant, possible_event2$animal)
  expect_equal(
    possible_event1$event + 4, possible_event2$event)
})







