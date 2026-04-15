###############################################################################
# proj3/02_run_local_example.R
#
# A small, quick example you can run locally to verify the pipeline works.
# Uses a single mid-range parameter set with reduced simulation time (2 MY)
# and 1 replicate, so it should complete in a few minutes.
#
# Usage (from the specmutual root directory):
#   Rscript proj3/02_run_local_example.R
#
# Or source it in an interactive R session.
###############################################################################

library(specmutual)

# ---------------------------------------------------------------------------
# Load M0 — adjust path if needed
# ---------------------------------------------------------------------------
# Try relative path first (assumes working directory is specmutual/)
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
m0_candidates <- c(
  "script/M0.RData",
  "../script/M0.RData",
  "~/specmutual/script/M0.RData",
  file.path(script_dir, "..", "script", "M0.RData")
)
m0_loaded <- FALSE
for (mp in m0_candidates) {
  mp <- normalizePath(mp, mustWork = FALSE)
  if (file.exists(mp)) {
    load(mp)
    cat("Loaded M0 from:", mp, "\n")
    m0_loaded <- TRUE
    break
  }
}
if (!m0_loaded) stop("Cannot find M0.RData. Please adjust the path.")

cat("Mainland network size:", nrow(M0), "plants x", ncol(M0), "animals\n\n")

# ---------------------------------------------------------------------------
# Mid-range parameter set (close to manuscript medium-mutualism scenario)
# ---------------------------------------------------------------------------
mutualism_pars <- create_mutual_pars(
  lac_pars  = c(0.5, 0.5),                       # cladogenesis
  mu_pars   = c(0.1, 0.1, 0.005, 0.005),         # extinction (intrinsic + mutualism)
  K_pars    = c(50, 50, 50, 50),                  # carrying capacity
  gam_pars  = c(0.06, 0.06),                      # immigration
  laa_pars  = c(0.5, 0.5, 0.005, 0.005),         # anagenesis
  qgain     = 0.001,
  qloss     = 0.001,
  lambda0   = 0.25,                               # cospeciation
  M0        = M0,
  transprob = 1.0,
  alpha     = 100
)

set.seed(123)

# ---------------------------------------------------------------------------
# Run a short simulation
# ---------------------------------------------------------------------------
cat("Running local example (total_time = 2, replicates = 1) ...\n")
t1 <- Sys.time()

sim_output <- peregrine_sim(
  total_time     = 2,
  replicates     = 1,
  mutualism_pars = mutualism_pars,
  return_parts   = "island_parts",
  verbose        = TRUE
)

t2 <- Sys.time()
cat("\nDone! Elapsed time:", format(t2 - t1), "\n\n")

# ---------------------------------------------------------------------------
# Quick summary
# ---------------------------------------------------------------------------
rep1     <- sim_output[[1]]
n_plant  <- sum(rep1$status_p)
n_animal <- sum(rep1$status_a)
cat("Plant species on island :", n_plant, "\n")
cat("Animal species on island:", n_animal, "\n")

Mt      <- rep1$Mt
true_Mt <- Mt[rep1$status_p == 1, rep1$status_a == 1, drop = FALSE]
cat("Island network size     :", nrow(true_Mt), "x", ncol(true_Mt), "\n")

if (nrow(true_Mt) > 0 && ncol(true_Mt) > 0) {
  conn <- sum(true_Mt > 0) / (nrow(true_Mt) * ncol(true_Mt))
  cat("Network connectance     :", round(conn, 4), "\n")
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
outfile <- "proj3/local_example_output.rds"
if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive = TRUE)
saveRDS(sim_output, file = outfile)
cat("\nOutput saved to:", outfile, "\n")
cat("You can inspect it with: readRDS('", outfile, "')\n", sep = "")
