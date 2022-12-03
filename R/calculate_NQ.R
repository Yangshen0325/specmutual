

format_Mt <- function(outs_parpool) {
  rep <- length(outs_parpool)
  pos <- round(seq(1, 100, length.out = 70))
  N_table <- c()
  Q_table <- c()
  for (i in 1:rep) {
    M_true_list <- outs_parpool[[i]][["M_true_list"]]
    Nnes <- list()
    Qmod <- list()
    for (j in 1:length(M_true_list)) {
      if (is.matrix(M_true_list[[j]]) && min(dim(M_true_list[[j]])) > 1) {
        Nnesofthis <- bipartite::nest.smdm(M_true_list[[j]])
        Nnes[[j]] <- Nnesofthis$NODFmatrix
        Qgraph <- igraph::graph_from_incidence_matrix(M_true_list[[j]])
        Qmod[[j]] <- igraph::modularity(igraph::cluster_walktrap(Qgraph))
      } else {
        Nnes[[j]] <- -1
        Qmod[[j]] <- -1
      }
    }
    Nnes <- as.matrix(Nnes[pos])
    Qmod <- as.matrix(Qmod[pos])
    N_table <- cbind(N_table, Nnes)
    Q_table <- cbind(Q_table, Qmod)
  }

}
