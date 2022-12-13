# # calculate the difference of stt_table.
#
# ####  Please don't run outs_DAISIE below, because the outputs already saved in
# ####  "D:/YSPhD_Aca/specmutual/results/outs_DAISIE.RData"
# ## outs_DAISIE <- list()
# ## for (i in 1:7) {
# ##   set.seed(i)
# ##   message("Running param set: ", i)
# ##   out_DAISIE <- DAISIE::DAISIE_sim_cr(
# ##     time = 5,
# ##     M = 200,
# ##     pars = pars_pool[[i]],
# ##     replicates = 1000,
# ##     divdepmodel = "IW",
# ##     plot_sims = FALSE,
# ##     verbose = FALSE)
# ##   outs_DAISIE[[i]] <- out_DAISIE
# ## }
# ## save(outs_DAISIE, file = "D:/YSPhD_Aca/specmutual/results/outs_DAISIE.RData")
#
# # mutualism_pars_set1 <- list(
# #   lac_pars = c(0.2, 0.2),
# #   mu_pars = c(0.5, 0.5, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.05, 0.05),
# #   laa_pars = c(1.0, 1.0, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set2 <- list(
# #   lac_pars = c(0.2, 0.2),
# #   mu_pars = c(0.5, 0.5, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.1, 0.1),
# #   laa_pars = c(1.0, 1.0, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set3 <- list(
# #   lac_pars = c(0.2, 0.2),
# #   mu_pars = c(0.5, 0.5, 0.0, 0.0),
# #   K_pars = c(100, 100, Inf, Inf),
# #   gam_pars = c(0.05, 0.05),
# #   laa_pars = c(1.0, 1.0, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set4 <- list(
# #   lac_pars = c(0.4, 0.4),
# #   mu_pars = c(0.5, 0.5, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.05, 0.05),
# #   laa_pars = c(1.0, 1.0, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set5 <- list(
# #   lac_pars = c(0.2, 0.2),
# #   mu_pars = c(0.8, 0.8, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.05, 0.05),
# #   laa_pars = c(1.0, 1.0, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set6 <- list(
# #   lac_pars = c(0.2, 0.2),
# #   mu_pars = c(0.5, 0.5, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.05, 0.05),
# #   laa_pars = c(0.5, 0.5, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
# #
# # mutualism_pars_set7 <- list(
# #   lac_pars = c(1.2, 1.2),
# #   mu_pars = c(0.6, 0.6, 0.0, 0.0),
# #   K_pars = c(Inf, Inf, Inf, Inf),
# #   gam_pars = c(0.02, 0.02),
# #   laa_pars = c(1.2, 1.2, 0.0, 0.0),
# #   qgain = 0.0,
# #   qloss = 0.0,
# #   lambda0 = 0.0,
# #   M0 = matrix(
# #     sample(c(0, 1), 10000, replace = TRUE),
# #     ncol = 100,
# #     nrow = 100
# #   ),
# #   transprob = 1.0
# # )
#
# # load("D:/YSPhD_Aca/specmutual/results/outs_parpool1.RData")
# # load("D:/YSPhD_Aca/specmutual/results/outs_DAISIE.RData")
# out_DAISIE <- outs_DAISIE[[1]]
# island_total <- format_island_mutual_total(island_replicates = outs_parpool,
#                                            total_time = 5,
#                                            sample_freq = 25,
#                                            mutualism_pars = mutualism_pars_set1)
#
# rep <- length(island_total)
# complete_arr_mutual <- array(dim = c(26, 6, rep))
# complete_arr_DAISIE <- array(dim = c(26, 6, rep))
# for (x in 1:rep) {
#   #mutualism
#   sum_endemics_mutual <- island_total[[x]][[1]]$stt_all[, "nAp"] +
#     island_total[[x]][[1]]$stt_all[, "nCp"] +
#     island_total[[x]][[1]]$stt_all[, "nAa"] +
#     island_total[[x]][[1]]$stt_all[, "nCa"]
#   total_mutual <- island_total[[x]][[1]]$stt_all[, "nIp"] +
#     island_total[[x]][[1]]$stt_all[, "nAp"] +
#     island_total[[x]][[1]]$stt_all[, "nCp"] +
#     island_total[[x]][[1]]$stt_all[, "nIa"] +
#     island_total[[x]][[1]]$stt_all[, "nAa"] +
#     island_total[[x]][[1]]$stt_all[, "nCa"]
#   nI_mutual <- island_total[[x]][[1]]$stt_all[, "nIp"] +
#     island_total[[x]][[1]]$stt_all[, "nIa"]
#   nA_mutual <- island_total[[x]][[1]]$stt_all[, "nAp"] +
#     island_total[[x]][[1]]$stt_all[, "nAa"]
#   nC_mutual <- island_total[[x]][[1]]$stt_all[, "nCp"] +
#     island_total[[x]][[1]]$stt_all[, "nCa"]
#   complete_arr_mutual[, , x] <- cbind(island_total[[x]][[1]]$stt_all[, 'Time'],
#                                       nI_mutual,
#                                       nA_mutual,
#                                       nC_mutual,
#                                       sum_endemics_mutual,
#                                       total_mutual)
#   #DAISIE
#   sum_endemics_DAISIE <- out_DAISIE[[x]][[1]]$stt_all[, "nA"] +
#     out_DAISIE[[x]][[1]]$stt_all[, "nC"]
#   total_DAISIE <- out_DAISIE[[x]][[1]]$stt_all[, "nI"] +
#     out_DAISIE[[x]][[1]]$stt_all[, "nA"] +
#     out_DAISIE[[x]][[1]]$stt_all[, "nC"]
#   complete_arr_DAISIE[, , x] <- cbind(out_DAISIE[[x]][[1]]$stt_all[, 'Time'],
#                                       out_DAISIE[[x]][[1]]$stt_all[, 'nI'],
#                                       out_DAISIE[[x]][[1]]$stt_all[, 'nA'],
#                                       out_DAISIE[[x]][[1]]$stt_all[, 'nC'],
#                                       sum_endemics_DAISIE,
#                                       total_DAISIE)
# }
# ###  Comparation ###
# stt_median_mutual <- apply(complete_arr_mutual, c(1, 2), stats::median)
# stt_average_mutual <- apply(complete_arr_mutual, c(1, 2), mean)
#
# stt_median_DAISIE <- apply(complete_arr_DAISIE, c(1, 2), stats::median)
# stt_average_DAISIE <- apply(complete_arr_DAISIE, c(1, 2), mean)
#
# diff_meadin <- stt_median_DAISIE - stt_median_mutual
# # colMeans(diff_meadin)
# diff_aver <- stt_average_DAISIE - stt_average_mutual
# # colMeans(diff_aver)
#
## plot the difference in terms of median
# par(mfrow = c(1, 5))
# plot(1:26, diff_meadin[, 2], ylim = c(-5, 5), col="black", pch = 16,
#      xaxt = "n", xlab ="",
#      ylab = "Difference in nI", cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2)
# plot(1:26, diff_meadin[, 3], ylim = c(-5, 5), col="black", pch = 16,
#      xaxt = "n",xlab ="",
#      ylab = "Difference in nA", cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2)
# plot(1:26, diff_meadin[, 4], ylim = c(-5, 5), col="black", pch = 16,
#      xaxt = "n",xlab ="",
#      ylab = "Difference in nC", cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2)
# plot(1:26, diff_meadin[, 5], ylim = c(-5, 5), col="black", pch = 16,
#      xaxt = "n",xlab ="",
#      ylab = "Difference in endemic", cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2)
# plot(1:26, diff_meadin[, 6], ylim = c(-5, 5), col="black", pch = 16,
#      xaxt = "n",xlab ="",
#      ylab = "Difference in total", cex.lab = 1.2, cex.main = 1.2, cex.axis = 1.2)
