# load("D:/PhD_Yang/specmutual/result/out_5.RData")
# load("X:/YSPhD_Aca/specmutual/result/out_5.RData")

# load("X:/YSPhD_Aca/specmutual/trial/M0.RData")
# load("D:/PhD_Yang/specmutual/trial/M0.RData")
# mutualism_pars <- create_mutualism_pars(
#   lac_pars = c(0.5, 0.5), # c(lac_plant, lac_animal)
#   mu_pars = c(0.2, 0.2, 1.0, 1.0), # c(mu_P0, mu_A0, mu_P1, mu_A1)
#   K_pars = c(Inf, Inf, 1.0, 1.0), # c(K_P0, K_A0, K_P1, K_A1)
#   gam_pars = c(0.1, 0.1), # (gam_plant, gam_animal)
#   qgain = 1.0,
#   qloss = 1.0,
#   laa_pars = c(0.3, 0.3, 1.0, 1.0), # c(laa_P0, laa_A0, laa_P1, laa_A1)
#   lambda0 = 1.0,
#   M0 = M0,
#   transprob = 1.0) # the possibility to inherit links from parents
#  load("X:/YSPhD_Aca/specmutual/trial/island_replicates.RData")
#  load("D:/PhD_Yang/specmutual/trial/island_replicates.RData")

format_island_mutual <- function(island_replicates,
                                 total_time,
                                 sample_freq,
                                 mutualism_pars,
                                 verbose = TRUE) {

  M0 <- mutualism_pars$M0
  several_islands_plant <- list()
  several_islands_animal <- list()
  for (rep in 1:length(island_replicates)) {
    the_island <- island_replicates[[rep]]
    the_stt <- the_island$stt_table
    clades_info_plant <- the_island$clades_info_plant
    clades_info_animal <- the_island$clades_info_animal

    stt_all <- matrix(ncol = 7, nrow = sample_freq + 1)
    colnames(stt_all) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_all[, "Time"] <- rev(seq(from = 0,
                                 to = total_time,
                                 length.out = sample_freq + 1))
    stt_all[1, 2:7] <- c(0, 0, 0, 0, 0, 0)
    for (j in 2:nrow(stt_all)) {
      the_age <- stt_all[j, "Time"]
      stt_all[j, 2:7] <- the_stt[max(which(the_stt[, "Time"] >= the_age)), 2:7]
    }

    island_list_plant <- list()
    island_list_animal <- list()
    if(sum(the_stt[nrow(the_stt), 2:4]) == 0) {
      island_list_plant[[1]] <- list(island_age = total_time,
                                     not_present_p = nrow(M0),
                                     stt_plant = stt_all[, 1:4])
    } else {
      island_list_plant[[1]] <- list(
        island_age = total_time,
        not_present_p = nrow(M0) - length(clades_info_plant),
        stt_plant = stt_all[, 1:4])
      for (y in 1:length(clades_info_plant)) {
        island_list_plant[[y + 1]] = clades_info_plant[[y]]
      }
    }
    if (sum(the_stt[nrow(the_stt), 5:7]) == 0) {
      island_list_animal[[1]] <- list(island_age = total_time,
                                      not_present_a = ncol(M0),
                                      stt_animal = stt_all[, c(1, 5:7)])
    } else {
      island_list_animal[[1]] <- list(
        island_age = total_time,
        not_present_a = ncol(M0) - length(clades_info_animal),
        stt_animal = stt_all[, c(1, 5:7)])
      for (y in 1:length(clades_info_animal)) {
        island_list_animal[[y + 1]] = clades_info_animal[[y]]
      }
    }

    island_list_plant <- add_brt_table(island = island_list_plant)
    island_list_animal <- add_brt_table(island = island_list_animal)
    several_islands_plant[[rep]] <- island_list_plant
    several_islands_animal[[rep]] <- island_list_animal
    if (verbose == TRUE) {
      message(
        "Island being formatted: ", rep, "/", length(island_replicates)
      )
  }
  return(list(several_islands_plant = several_islands_plant,
              several_islands_animal = several_islands_animal))
  }
}


