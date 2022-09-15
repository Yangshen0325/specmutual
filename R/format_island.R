# load("D:/PhD_Yang/specmutual/result/out_5.RData")
# load("X:/YSPhD_Aca/specmutual/result/out_5.RData")

# load("X:/YSPhD_Aca/specmutual/trial/M0.RData")
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

state_list_all <- list()
island_list_all <- list()
for (i in 1:length(island_replicates)){
  state_list_all[[i]] <- island_replicates[[i]][["state_list"]]
  island_list_all[[i]] <- island_replicates[[i]][["island"]]
}

format_island <- function(island_list_all,
                       simtime,
                       sample_freq,
                       mutualism_pars){
  M0 <- mutualism_pars$M0
  Mtotal <- nrow(M0) + ncol(M0)
  several_islands <- list()
  for (rep in 1:length(island_list_all)){
    the_island <- island_list_all[[rep]]
    the_stt <- the_island$stt_table
    taxon_list <- the_island$taxon_list
    stt_all <- matrix(ncol = 7, nrow = sample_freq + 1)
    colnames(stt_all) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_all[, "Time"] <- rev(seq(from = 0,
                                   to = simtime,
                                   length.out = sample_freq + 1))
    stt_all[1, 2:7] <- c(0, 0, 0, 0, 0, 0)
    for (j in 2:nrow(stt_all)) {
      the_age <- stt_all[j, "Time"]
      stt_all[j, 2:7] <- the_stt[max(which(the_stt[, "Time"] >= the_age)), 2:7]
    }

    island_list <- list() # I didn't write `not_present`
    island_list[[1]] <- list(island_age = simtime,
                             stt_all = stt_all)

    if(sum(the_stt[nrow(the_stt),2:7]) == 0){
      island_list[[1]] <- list(island_age = simtime,
                               not_present = Mtotal,
                               stt_all = stt_all)
    } else {
      island_list[[1]] <- list(island_age = simtime,
                               not_present = Mtotal - length(taxon_list),
                               stt_all = stt_all)
      for (y in 1:length(taxon_list)){
        island_list[[y + 1]] = taxon_list[[y]]
      }
    }

    island_list <- DAISIE:::add_brt_table(island = island_list)
    several_islands[[rep]] <- island_list
  }
  return(several_islands)
}

