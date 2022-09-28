# job_simulation_one
# M0 produced
M0 <- {set.seed(122); specmutual::get_M0(plant = 40)}
mutualism_pars <- specmutual::create_mutualism_pars(
  lac_pars = c(0.5, 0.5), # c(lac_plant, lac_animal)
  mu_pars = c(0.2, 0.2, 1.0, 1.0), # c(mu_P0, mu_A0, mu_P1, mu_A1)
  K_pars = c(Inf, Inf, 1.0, 1.0), # c(K_P0, K_A0, K_P1, K_A1)
  gam_pars = c(0.01, 0.01), # (gam_plant, gam_animal)
  qgain = 1.0,
  qloss = 1.0,
  laa_pars = c(0.3, 0.3, 1.0, 1.0), # c(laa_P0, laa_A0, laa_P1, laa_A1)
  lambda0 = 1.0,
  M0 = M0,
  transprob = 1.0)
output_one <- specmutual::sim_mutualism(simtime = 5,
                                        replicates = 500,
                                        mutualism_pars = mutualism_pars,
                                        sample_freq = 25,
                                        plot_sims = FALSE)
#dir.create("result")
save(output_one, file = "result/output_one.RData")
