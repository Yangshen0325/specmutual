# calculate the difference of stt_table.

island_total <- out_mutual$island_total
rep <- length(island_total)
complete_arr_mutual <- array(dim = c(26, 6, rep))
complete_arr_DAISIE <- array(dim = c(26, 6, rep))
for (x in 1:rep) {
  #mutualism
  sum_endemics_mutual <- island_total[[x]][[1]]$stt_all[, "nAp"] +
    island_total[[x]][[1]]$stt_all[, "nCp"] +
    island_total[[x]][[1]]$stt_all[, "nAa"] +
    island_total[[x]][[1]]$stt_all[, "nCa"]
  total_mutual <- island_total[[x]][[1]]$stt_all[, "nIp"] +
    island_total[[x]][[1]]$stt_all[, "nAp"] +
    island_total[[x]][[1]]$stt_all[, "nCp"] +
    island_total[[x]][[1]]$stt_all[, "nIa"] +
    island_total[[x]][[1]]$stt_all[, "nAa"] +
    island_total[[x]][[1]]$stt_all[, "nCa"]
  nI_mutual <- island_total[[x]][[1]]$stt_all[, "nIp"] +
    island_total[[x]][[1]]$stt_all[, "nIa"]
  nA_mutual <- island_total[[x]][[1]]$stt_all[, "nAp"] +
    island_total[[x]][[1]]$stt_all[, "nAa"]
  nC_mutual <- island_total[[x]][[1]]$stt_all[, "nCp"] +
    island_total[[x]][[1]]$stt_all[, "nCa"]
  complete_arr_mutual[, , x] <- cbind(island_total[[x]][[1]]$stt_all[, 'Time'],
                                   nI_mutual,
                                   nA_mutual,
                                   nC_mutual,
                                   sum_endemics_mutual,
                                   total_mutual)
  #DAISIE
  sum_endemics_DAISIE <- out_DAISIE[[x]][[1]]$stt_all[, "nA"] +
    out_DAISIE[[x]][[1]]$stt_all[, "nC"]
  total_DAISIE <- out_DAISIE[[x]][[1]]$stt_all[, "nI"] +
    out_DAISIE[[x]][[1]]$stt_all[, "nA"] +
    out_DAISIE[[x]][[1]]$stt_all[, "nC"]
  complete_arr_DAISIE[, , x] <- cbind(out_DAISIE[[x]][[1]]$stt_all[, 'Time'],
                                      out_DAISIE[[x]][[1]]$stt_all[, 'nI'],
                                      out_DAISIE[[x]][[1]]$stt_all[, 'nA'],
                                      out_DAISIE[[x]][[1]]$stt_all[, 'nC'],
                                      sum_endemics_DAISIE,
                                      total_DAISIE)
}
###  Comparation ###
stt_median_mutual <- apply(complete_arr_mutual, c(1, 2), stats::median)
stt_average_mutual <- apply(complete_arr_mutual, c(1, 2), mean)

stt_median_DAISIE <- apply(complete_arr_DAISIE, c(1, 2), stats::median)
stt_average_DAISIE <- apply(complete_arr_DAISIE, c(1, 2), mean)

diff_meadin <- stt_median_DAISIE - stt_median_mutual
# colMeans(diff_meadin)
diff_aver <- stt_average_DAISIE - stt_average_mutual
# colMeans(diff_aver)

