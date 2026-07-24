###############################################################################
# Shared helpers for the proj3_second pilot.
###############################################################################

proj3_second_base_dir <- function() {
  configured <- Sys.getenv("SPEC_MUTUAL_BASE", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(path.expand(configured), mustWork = TRUE))
  }
  normalizePath(".", mustWork = TRUE)
}

proj3_second_load_model <- function(base_dir) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(base_dir, quiet = TRUE)
  } else {
    suppressPackageStartupMessages(library(specmutual))
    sys.source(
      file.path(base_dir, "R", "sim_core_mutualism_proj3.R"),
      envir = .GlobalEnv
    )
  }
  invisible(TRUE)
}

proj3_second_atomic_save_rds <- function(object, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(file, ".tmp.", Sys.getpid())
  saveRDS(object, temporary, compress = "xz")
  if (!file.rename(temporary, file)) {
    unlink(temporary)
    stop("Could not atomically move result into place: ", file)
  }
  invisible(file)
}

proj3_second_quantile <- function(x, probability) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  unname(stats::quantile(x, probability, names = FALSE, type = 8))
}

proj3_second_end_ltt <- function(clades_info) {
  if (is.null(clades_info) || length(clades_info) == 0) {
    return(list(
      nonend_ltt = data.frame(nonend_brt = 0, n_nonend = 0),
      singleton_ltt = data.frame(singleton_brt = 0, n_singleton = 0),
      multi_ltt = data.frame(multi_brt = 0, n_multi = 0)
    ))
  }

  stac <- unlist(lapply(clades_info, "[[", "stac"))
  branching_times <- lapply(clades_info, "[[", "branching_times")
  branching_lengths <- vapply(branching_times, length, integer(1))

  make_ltt <- function(indices, time_name, number_name) {
    times <- c(
      unique(sort(unlist(branching_times[indices]), decreasing = TRUE)),
      0
    )
    if (length(times) == 1) {
      times <- 0
      numbers <- 0
    } else {
      numbers <- c(seq(0, length(times) - 2), length(times) - 2)
    }
    out <- data.frame(times, numbers)
    names(out) <- c(time_name, number_name)
    out
  }

  list(
    nonend_ltt = make_ltt(
      which(stac == 4), "nonend_brt", "n_nonend"
    ),
    singleton_ltt = make_ltt(
      which(stac == 2 & branching_lengths == 2),
      "singleton_brt",
      "n_singleton"
    ),
    multi_ltt = make_ltt(
      which((stac == 2 & branching_lengths > 2) | stac == 3),
      "multi_brt",
      "n_multi"
    )
  )
}

proj3_second_safe_nltt <- function(ltt_data, type) {
  if (nrow(ltt_data) == 0 || ltt_data[1, 1] == 0) {
    return(0)
  }
  nLTT::nltt_diff_exact_extinct(
    event_times = ltt_data[[paste0(type, "_brt")]],
    species_number = ltt_data[[paste0("n_", type)]],
    event_times2 = 0,
    species_number2 = 0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )
}

proj3_second_final_mismatch <- function(state, M0) {
  island_spec <- state$island_spec
  empty <- list(plant = numeric(0), animal = numeric(0))
  if (length(island_spec) == 0) {
    return(empty)
  }

  immigrant_plants <- as.numeric(island_spec[
    island_spec[, "Species type"] == "I" &
      island_spec[, "Species state"] == "plant",
    "Species"
  ])
  immigrant_animals <- as.numeric(island_spec[
    island_spec[, "Species type"] == "I" &
      island_spec[, "Species state"] == "animal",
    "Species"
  ])
  immigrant_plants <- immigrant_plants[
    immigrant_plants >= 1 & immigrant_plants <= nrow(M0)
  ]
  immigrant_animals <- immigrant_animals[
    immigrant_animals >= 1 & immigrant_animals <= ncol(M0)
  ]

  mismatch <- abs(
    state$Mt[seq_len(nrow(M0)), seq_len(ncol(M0)), drop = FALSE] - M0
  )
  plant <- if (length(immigrant_plants) == 0) {
    numeric(0)
  } else {
    as.numeric(
      mismatch %*% state$status_a[seq_len(ncol(M0)), , drop = FALSE]
    )[immigrant_plants]
  }
  animal <- if (length(immigrant_animals) == 0) {
    numeric(0)
  } else {
    as.numeric(
      t(mismatch) %*% state$status_p[seq_len(nrow(M0)), , drop = FALSE]
    )[immigrant_animals]
  }
  list(plant = plant, animal = animal)
}

proj3_second_summarize_state <- function(state, M0, completed) {
  status_p <- as.numeric(state$status_p)
  status_a <- as.numeric(state$status_a)
  present_p <- which(status_p == 1)
  present_a <- which(status_a == 1)
  true_Mt <- state$Mt[present_p, present_a, drop = FALSE]

  island_p <- length(present_p)
  island_a <- length(present_a)
  connectance <- if (nrow(true_Mt) >= 2 && ncol(true_Mt) >= 2) {
    sum(true_Mt) / length(true_Mt)
  } else {
    NA_real_
  }
  disconnect_p <- if (nrow(true_Mt) > 0) {
    sum(rowSums(true_Mt) == 0)
  } else {
    NA_real_
  }
  disconnect_a <- if (ncol(true_Mt) > 0) {
    sum(colSums(true_Mt) == 0)
  } else {
    NA_real_
  }

  plant_degree_values <- if (nrow(true_Mt) > 0) {
    rowSums(true_Mt)
  } else {
    numeric(0)
  }
  animal_degree_values <- if (ncol(true_Mt) > 0) {
    colSums(true_Mt)
  } else {
    numeric(0)
  }

  if (nrow(true_Mt) > 0 && ncol(true_Mt) > 0) {
    graph <- igraph::graph_from_biadjacency_matrix(true_Mt)
    component_info <- igraph::components(graph)
    largest_component <- max(component_info$csize)
    n_components <- component_info$no
  } else {
    largest_component <- NA_real_
    n_components <- NA_real_
  }

  island_spec <- state$island_spec
  type_count <- function(guild, type) {
    if (length(island_spec) == 0) {
      return(0)
    }
    sum(
      island_spec[, "Species state"] == guild &
        island_spec[, "Species type"] == type
    )
  }
  nonendemic_p <- type_count("plant", "I")
  endemic_p <- type_count("plant", "A") + type_count("plant", "C")
  nonendemic_a <- type_count("animal", "I")
  endemic_a <- type_count("animal", "A") + type_count("animal", "C")

  nltt_values <- setNames(rep(NA_real_, 6), c(
    "nonend_nltt_p", "singleton_nltt_p", "multi_nltt_p",
    "nonend_nltt_a", "singleton_nltt_a", "multi_nltt_a"
  ))
  if (isTRUE(completed) && !is.null(state$island)) {
    plant_ltt <- proj3_second_end_ltt(state$island$clades_info_plant)
    animal_ltt <- proj3_second_end_ltt(state$island$clades_info_animal)
    nltt_values <- c(
      nonend_nltt_p = proj3_second_safe_nltt(plant_ltt$nonend_ltt, "nonend"),
      singleton_nltt_p = proj3_second_safe_nltt(
        plant_ltt$singleton_ltt, "singleton"
      ),
      multi_nltt_p = proj3_second_safe_nltt(plant_ltt$multi_ltt, "multi"),
      nonend_nltt_a = proj3_second_safe_nltt(animal_ltt$nonend_ltt, "nonend"),
      singleton_nltt_a = proj3_second_safe_nltt(
        animal_ltt$singleton_ltt, "singleton"
      ),
      multi_nltt_a = proj3_second_safe_nltt(animal_ltt$multi_ltt, "multi")
    )
  }

  mismatch <- proj3_second_final_mismatch(state, M0)
  all_degree <- c(plant_degree_values, animal_degree_values)
  all_mismatch <- c(mismatch$plant, mismatch$animal)

  c(
    island_p = island_p,
    island_a = island_a,
    island_endemic_p = endemic_p,
    island_nonendemic_p = nonendemic_p,
    island_endemic_a = endemic_a,
    island_nonendemic_a = nonendemic_a,
    connectance = connectance,
    disconnect_p = disconnect_p,
    disconnect_a = disconnect_a,
    largest_component = largest_component,
    n_components = n_components,
    plant_degree = if (length(plant_degree_values)) {
      mean(plant_degree_values)
    } else {
      NA_real_
    },
    animal_degree = if (length(animal_degree_values)) {
      mean(animal_degree_values)
    } else {
      NA_real_
    },
    nltt_values,
    final_degree_median = proj3_second_quantile(all_degree, 0.5),
    final_degree_p90 = proj3_second_quantile(all_degree, 0.9),
    final_degree_p99 = proj3_second_quantile(all_degree, 0.99),
    final_degree_max = if (length(all_degree)) max(all_degree) else NA_real_,
    final_D_median = proj3_second_quantile(all_mismatch, 0.5),
    final_D_p90 = proj3_second_quantile(all_mismatch, 0.9),
    final_D_max = if (length(all_mismatch)) max(all_mismatch) else NA_real_,
    final_D_nonzero_fraction = if (length(all_mismatch)) {
      mean(all_mismatch > 0)
    } else {
      NA_real_
    }
  )
}

proj3_second_flatten_result <- function(result) {
  values <- c(
    list(
      combo_id = result$combo_id,
      seed = result$seed,
      design_group = result$design_group,
      run_started = result$run_started,
      run_finished = result$run_finished,
      total_time = result$total_time
    ),
    result$params[setdiff(names(result$params), "design_group")],
    as.list(setNames(
      unlist(result$fixed_parameters),
      paste0("fixed_", names(result$fixed_parameters))
    )),
    as.list(setNames(
      unlist(result$safety_caps),
      paste0("cap_", names(result$safety_caps))
    )),
    result$diagnostics,
    as.list(result$community_stats)
  )
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}
