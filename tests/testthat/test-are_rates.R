test_that("rates are feasible", {
  expect_true(
    are_rates(
      update_rates_mutualism(
        Mt = {
          set.seed(2)
          matrix(sample(c(0, 1), 20, replace = TRUE), ncol = 4, nrow = 5)
        },
        status_p = matrix(0, ncol = 1, nrow = 5),
        status_a = matrix(0, ncol = 1, nrow = 4),
        mutualism_pars = create_mutual_pars(
          lac_pars = c(2, 2.5),
          mu_pars = c(2, 2, 0.5, 0.5),
          K_pars = c(Inf, Inf, Inf, Inf),
          gam_pars = c(1.2, 1.5),
          laa_pars = c(3, 3, 0.5, 0.5),
          qgain = 0.5,
          qloss = 0.5,
          lambda0 = 0.5,
          M0 = {
            set.seed(1)
            matrix(sample(c(0, 1), 20, replace = TRUE), ncol = 4, nrow = 5)
          },
          transprob = 0.5
        ),
        island_spec = c()
      )
    )
  )
})
# I set four types of rate only for plant with lac=2, mu=2, gam=1, laa=3 and without mutualism, DD,
# and set the same only for animal species, expecting them the same output.
# Mt is random generated but have to make nrow(Mt)=ncol(Mt).
test_that("four rates for plant equals to that for animal without mutualism
          and DD", {
  expect_equal(
    unname(update_rates_mutualism(
      Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5),
      status_p = matrix(1, ncol = 1, nrow = 5),
      status_a = matrix(1, ncol = 1, nrow = 5),
      mutualism_pars = create_mutual_pars(
        lac_pars = c(2, 0),
        mu_pars = c(2, 0, 0, 0),
        K_pars = c(Inf, Inf, Inf, Inf),
        gam_pars = c(1, 0),
        laa_pars = c(3, 0, 0, 0),
        qgain = 0,
        qloss = 0,
        lambda0 = 0,
        M0 = {
          set.seed(1)
          matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
        },
        transprob = 0
      ),
      island_spec = c()
    )[1:4]),
    unname(update_rates_mutualism(
      Mt = matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5),
      status_p = matrix(1, ncol = 1, nrow = 5),
      status_a = matrix(1, ncol = 1, nrow = 5),
      mutualism_pars = create_mutual_pars(
        lac_pars = c(0, 2),
        mu_pars = c(0, 2, 0, 0),
        K_pars = c(Inf, Inf, Inf, Inf),
        gam_pars = c(0, 1),
        laa_pars = c(0, 3, 0, 0),
        qgain = 0,
        qloss = 0,
        lambda0 = 0,
        M0 = {
          set.seed(1)
          matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
        },
        transprob = 0
      ),
      island_spec = c()
    )[5:8])
  )
})

# immigration rates: length(immig_p) = length(NROW(M0)), length(immig_a) = length(NCOL(M0))
# anagenesis rates: length(ana_p) = length(NROW(M0)), length(ana_a) = length(NCOL(M0))
test_that("immigration and anagenesis only happen to species from mainland", {
  Mt <- matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
  status_p <- matrix(1, ncol = 1, nrow = 5)
  status_a <- matrix(1, ncol = 1, nrow = 5)
  mutualism_pars <- create_mutual_pars(
    lac_pars = c(2, 0),
    mu_pars = c(2, 0, 0, 0),
    K_pars = c(Inf, Inf, Inf, Inf),
    gam_pars = c(1, 2),
    laa_pars = c(3, 0, 0, 0),
    qgain = 0,
    qloss = 0,
    lambda0 = 0,
    M0 = matrix(sample(c(0, 1), 20, replace = TRUE), ncol = 4, nrow = 5),
    transprob = 0
  )

  immig_rate <- get_immig_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )
  ana_rate <- get_ana_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars,
    island_spec = c()
  )

  expect_equal(
    length(immig_rate$immig_p),
    NROW(mutualism_pars$M0)
  )
  expect_equal(
    length(immig_rate$immig_a),
    NCOL(mutualism_pars$M0)
  )
  expect_equal(
    length(ana_rate$ana_p),
    NROW(mutualism_pars$M0)
  )
  expect_equal(
    length(ana_rate$ana_a),
    NCOL(mutualism_pars$M0)
  )
})

# extinction rates: length(ext_p) = length(NROW(Mt)), length(ext_a) = length(NCOL(Mt))
# cladogenesis rates: length(clado_p) = length(NROW(Mt)), length(clado_a) = length(NCOL(Mt))
test_that("extinction and cladogenesis happen to all species on island", {
  Mt <- matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
  status_p <- matrix(1, ncol = 1, nrow = 5)
  status_a <- matrix(1, ncol = 1, nrow = 5)
  mutualism_pars <- create_mutual_pars(
    lac_pars = c(2, 0),
    mu_pars = c(2, 1, 0, 0),
    K_pars = c(Inf, Inf, Inf, Inf),
    gam_pars = c(1, 2),
    laa_pars = c(3, 0, 0, 0),
    qgain = 0,
    qloss = 0,
    lambda0 = 0,
    M0 = matrix(sample(c(0, 1), 20, replace = TRUE), ncol = 4, nrow = 5),
    transprob = 0
  )

  ext_rate <- get_ext_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )
  clado_rate <- get_clado_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  expect_equal(
    length(ext_rate$ext_p),
    NROW(Mt)
  )
  expect_equal(
    length(ext_rate$ext_a),
    NCOL(Mt)
  )
  expect_equal(
    length(clado_rate$clado_p),
    NROW(Mt)
  )
  expect_equal(
    length(clado_rate$clado_a),
    NCOL(Mt)
  )
})

# cospeciation, gain and loss rates
test_that("cospeciation, gain and loss rates for each pair of species", {
  Mt <- matrix(sample(c(0, 1), 25, replace = TRUE), ncol = 5, nrow = 5)
  status_p <- matrix(1, ncol = 1, nrow = 5)
  status_a <- matrix(1, ncol = 1, nrow = 5)
  mutualism_pars <- create_mutual_pars(
    lac_pars = c(2, 0),
    mu_pars = c(2, 1, 0, 0),
    K_pars = c(Inf, Inf, Inf, Inf),
    gam_pars = c(1, 2),
    laa_pars = c(3, 0, 0, 0),
    qgain = 0,
    qloss = 0,
    lambda0 = 0,
    M0 = matrix(sample(c(0, 1), 20, replace = TRUE), ncol = 4, nrow = 5),
    transprob = 0
  )

  cospec_rate <- get_cospec_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )
  gain_rate <- get_gain_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )
  loss_rate <- get_loss_rate(
    Mt = Mt,
    status_p = status_p,
    status_a = status_a,
    mutualism_pars = mutualism_pars
  )

  expect_equal(
    prod(dim(cospec_rate)),
    prod(dim(Mt))
  )
  expect_equal(
    prod(dim(gain_rate)),
    prod(dim(Mt))
  )
  expect_equal(
    prod(dim(loss_rate)),
    prod(dim(Mt))
  )
})
