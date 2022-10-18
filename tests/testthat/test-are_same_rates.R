
M0 <- matrix(sample(c(0, 1), 25, replace = TRUE), ncol=5, nrow=5)
island_spec <- matrix(NA, nrow = nrow(M0) + ncol(M0), ncol =8)
island_spec[1:nrow(M0), 8] <- "plant"
island_spec[(nrow(M0) +1):(nrow(M0) + ncol(M0)), 8] <- "animal"
island_spec[, 4] <- "I"
island_spec[, 1] <- rep(c(1:5), 2)

rates <- update_rates_mutualism(M0 = M0,
                                Mt = M0,
                                status_p = matrix(1, ncol = 1, nrow = 5),
                                status_a = matrix(1, ncol = 1, nrow = 5),
                                lac_pars = c(2.0, 2.0),
                                mu_pars = c(0.5, 0.5, 1.0, 1.0),
                                K_pars = c(Inf, Inf, Inf, Inf),
                                gam_pars = c(0.05, 0.05),
                                laa_pars = c(0.3, 0.3, 1.5, 1.5),
                                qgain = 2.0,
                                qloss = 1.0,
                                lambda0 = 1.0,
                                transprob = 1.0,
                                island_spec = island_spec)
test_that("Plant species have the same rates with animal species in terms of immigration,
          extinction, anagenesis and cladogenesis", {
            expect_equal(rates$immig_p, rates$immig_a)
            expect_equal(rates$ext_p, rates$ext_a)
            expect_equal(rates$clado_p, rates$clado_a)
            expect_equal(rates$ana_p, rates$ana_a)
          })
