Nnes <- list()
Qmod <- list()
for (i in 1:rep) {
  if (is.matrix(M_true_list[[i]]) && min(dim(M_true_list[[i]])) > 1) {
    Nnesofthis <- bipartite::nest.smdm(M_true_list[[i]])
    Nnes[[i]] <- nesofthis$NODFmatrix
    Qmodofthis <- bipartite::computeModules(M_true_list[[i]])
    Qmod[[i]] <- Qmodofthis@likelihood
  } else {
    nes[[i]] <- -1
    Qmod[[i]] <- -1
  }
}

Nnes <- c(-1, unlist(nes), -1)
Qmod <- c(-1, unlist(Qmod), -1)
stt_table <- outs_parpool[[1]][["island"]][["stt_table"]]
timeval <- stt_table[, 1]
plot(timeval, Nnes)
plot(timeval, Qmod)
