###############################################################################
#
# Print a SLURM array specification for combinations that are not complete.
#
# Usage:
#   Rscript proj3/part2_newSim/02_failed_array.R
#
# Then resubmit with larger resources, for example:
#   FAILED=$(Rscript proj3/part2_newSim/02_failed_array.R)
#   if [ -n "${FAILED}" ]; then
#     sbatch --array="${FAILED}%20" --time=3-00:00:00 --mem=20G \
#       proj3/part2_newSim/submit_proj3.sh
#   fi
#
###############################################################################

base_dir <- Sys.getenv("SPEC_MUTUAL_BASE", unset = "~/specmutual")
base_dir <- path.expand(base_dir)
run_dir <- file.path(base_dir, "proj3", "part2_newSim")
param_csv <- file.path(run_dir, "param_table.csv")
out_dir <- file.path(run_dir, "results")

target_reps <- as.integer(Sys.getenv("PROJ3_TARGET_REPS", unset = "10"))

params <- read.csv(param_csv)
combo_ids <- params$combo_id

is_complete <- function(combo_id) {
  outfile <- file.path(out_dir, sprintf("combo_%03d.rds", combo_id))
  if (!file.exists(outfile)) return(FALSE)

  result <- tryCatch(readRDS(outfile), error = function(e) NULL)
  if (is.null(result)) return(FALSE)

  has_all_reps <- isTRUE(result$n_reps_ok >= target_reps) &&
    length(result$sim_output) >= target_reps

  has_all_reps && !isFALSE(result$complete)
}

failed <- combo_ids[!vapply(combo_ids, is_complete, logical(1))]

compress_ids <- function(ids) {
  ids <- sort(unique(as.integer(ids)))
  if (length(ids) == 0) return("")

  runs <- list()
  start <- ids[1]
  previous <- ids[1]

  if (length(ids) > 1) {
    for (id in ids[-1]) {
      if (id == previous + 1) {
        previous <- id
      } else {
        runs[[length(runs) + 1]] <- c(start, previous)
        start <- id
        previous <- id
      }
    }
  }
  runs[[length(runs) + 1]] <- c(start, previous)

  paste(
    vapply(
      runs,
      function(x) if (x[1] == x[2]) as.character(x[1]) else paste0(x[1], "-", x[2]),
      character(1)
    ),
    collapse = ","
  )
}

array_spec <- compress_ids(failed)
if (!nzchar(array_spec)) {
  message("All combinations are complete; nothing to resubmit.")
} else {
  cat(array_spec, "\n", sep = "")
}
