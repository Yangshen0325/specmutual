

# 1000 combinations

# Run this script ONCE before submitting the cluster array job:
#   Rscript proj3/part2_newSim/00_generate_params.R

# Output: proj3/part2_newSim/param_table.csv
###############################################################################

# --- Install lhs if needed ---------------------------------------------------
if (!requireNamespace("lhs", quietly = TRUE)) {
  install.packages("lhs", repos = "https://cloud.r-project.org")
}
library(lhs)

set.seed(3245478)

# --- Settings ----------------------------------------------------------------
n_combos <- 1000   # number of parameter combinations

# --- Generate LHS matrix in [0, 1]^9 ----------------------------------------
lhs_raw <- randomLHS(n = n_combos, k = 9)

# --- Define parameter ranges -------------------------------------------------
#
# Parameter   | Description                       | Range       | Manuscript
# ------------|-----------------------------------|-------------|------------
# lac_0       | Intrinsic cladogenesis rate        | [0.1, 1.0]  | {0.3, 0.6}
# mu_0        | Intrinsic extinction rate          | [0.01, 0.5] | {0.05, 0.2}
# gam_0       | Intrinsic immigration rate         | [0.01, 0.2] | {0.04, 0.08}
# laa_0       | Intrinsic anagenesis rate          | [0.1, 2.0]  | {0.5, 1.0}
# K_0         | Intrinsic carrying capacity        | [20, 100]   | 50 (fixed)
# K_1         | Mutualism carrying-capacity coeff  | [0, 150]    | {0, 50, 100}
# mu_1        | Mutualism extinction coefficient   | [0, 0.02]   | {0, 0.005, 0.01}
# laa_1       | Mutualism anagenesis coefficient   | [0, 0.02]   | {0, 0.005, 0.01}
# lambda0     | Cospeciation rate                  | [0, 1.0]    | {0, 0.25, 0.5}
#
# Fixed parameters (not in the sweep):
#   M0        : Fixed mainland network (135 plants x 612 animals)
#   K_0       : Now also varied (manuscript held at 50)
#   qgain     : 0.001
#   qloss     : 0.001
#   alpha     : 100
#   transprob : 1.0

# --- Scale LHS to parameter ranges ------------------------------------------
params <- data.frame(
  combo_id = 1:n_combos,
  lac_0    = lhs_raw[, 1] * (1.0  - 0.1)  + 0.1,
  mu_0     = lhs_raw[, 2] * (0.5  - 0.01) + 0.01,
  gam_0    = lhs_raw[, 3] * (0.2  - 0.01) + 0.01,
  laa_0    = lhs_raw[, 4] * (2.0  - 0.1)  + 0.1,
  K_0      = round(lhs_raw[, 5] * (100 - 20) + 20),
  K_1      = lhs_raw[, 6] * 150,
  mu_1     = lhs_raw[, 7] * 0.02,
  laa_1    = lhs_raw[, 8] * 0.02,
  lambda0  = lhs_raw[, 9] * 1.0
)

# --- Save to CSV -------------------------------------------------------------
# Determine script directory robustly (works across R versions)
script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) NULL
)
if (is.null(script_dir) || !nzchar(script_dir)) {
  # Fallback: assume working directory is specmutual/
  outfile <- "proj3/part2_newSim/param_table.csv"
} else {
  outfile <- file.path(script_dir, "param_table.csv")
}
write.csv(params, file = outfile, row.names = FALSE)

cat("Generated", n_combos, "parameter combinations via Latin Hypercube Sampling.\n")
cat("Saved to:", outfile, "\n\n")
cat("Parameter ranges:\n")
print(summary(params[, -1]))
cat("\nNext step: submit the cluster job with  sbatch proj3/part2_newSim/submit_proj3.sh\n")
