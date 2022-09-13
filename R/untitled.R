stt_table <- island_replicates[[rep]]$stt_table
taxon_list_size <- length(island_replicates[[rep]]$taxon_list)
island_list <- list()

if (taxon_list_size != 0) {
  island_list[[1]] <- list(
    island_age = total_time,
    stt_table = stt_table)
  for (i in 1: length(taxon_list_size)){
    island_list[[i + 1]] <- island_replicates[[rep]]$taxon_list[[i]]
  }
} else {
  island_list[[1]] <- list(
    island_age = total_time,
    stt_table = stt_table,
    brts_table = c(total_time, 0, 0, NA, NA))# can be combined
}
# internal function: island_list <-
