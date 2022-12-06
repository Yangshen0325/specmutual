
## 1st: dynamics
dynamic_N <- function(x, outputs) {
  M_true_list <- outputs[[x]][["M_true_list"]]
  Nnes <- list()
  for (i in 1: length(M_true_list)) {
    temp <- bipartite::nested(M_true_list[[i]], method = "binmatnest")
    Nnes[[i]] <- temp
  }
  Nnes <- Nnes[!is.na(Nnes)]
  plot(1:length(Nnes), Nnes,
       pch = 21,
       bg = "red",   # Fill color
       col = "blue", # Border color
       cex = 1,      # Symbol size
       lwd = 3,      # Border width
       ylab = "Nestedness",
       xlab = "",
       ylim = c(0, 20))
}









