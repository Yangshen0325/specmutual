# Lightweight, diagnostic simulation core for the proj3_second pilot.
#
# This file deliberately leaves the published simulation functions unchanged.
# It uses the same rate and state-transition functions, but replaces per-event
# storage of full rate matrices, network snapshots, evo_table, and stt_table
# with online scalar summaries.

proj3_internal_function <- function(name) {
  namespace <- tryCatch(asNamespace("specmutual"), error = function(e) NULL)
  if (!is.null(namespace) && exists(name, envir = namespace, inherits = FALSE)) {
    return(get(name, envir = namespace, inherits = FALSE))
  }
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE))
  }
  stop("Cannot resolve specmutual internal function: ", name)
}

proj3_new_exposure <- function(lower, upper) {
  list(
    lower = lower,
    upper = upper,
    weight = 0,
    weighted_sum = 0,
    weighted_sum_sq = 0,
    min = Inf,
    max = -Inf,
    inactive_weight = 0,
    informative_weight = 0,
    strong_weight = 0
  )
}

proj3_update_exposure <- function(accumulator, values, duration) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0 || !is.finite(duration) || duration <= 0) {
    return(accumulator)
  }

  total_weight <- duration * length(values)
  accumulator$weight <- accumulator$weight + total_weight
  accumulator$weighted_sum <- accumulator$weighted_sum + duration * sum(values)
  accumulator$weighted_sum_sq <- accumulator$weighted_sum_sq +
    duration * sum(values^2)
  accumulator$min <- min(accumulator$min, min(values))
  accumulator$max <- max(accumulator$max, max(values))
  accumulator$inactive_weight <- accumulator$inactive_weight +
    duration * sum(values < accumulator$lower)
  accumulator$informative_weight <- accumulator$informative_weight +
    duration * sum(values >= accumulator$lower & values < accumulator$upper)
  accumulator$strong_weight <- accumulator$strong_weight +
    duration * sum(values >= accumulator$upper)
  accumulator
}

proj3_finalize_exposure <- function(accumulator, prefix) {
  if (accumulator$weight <= 0) {
    values <- c(
      weight = 0,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      max = NA_real_,
      frac_inactive = NA_real_,
      frac_informative = NA_real_,
      frac_strong = NA_real_
    )
  } else {
    mean_value <- accumulator$weighted_sum / accumulator$weight
    variance <- max(
      0,
      accumulator$weighted_sum_sq / accumulator$weight - mean_value^2
    )
    values <- c(
      weight = accumulator$weight,
      mean = mean_value,
      sd = sqrt(variance),
      min = accumulator$min,
      max = accumulator$max,
      frac_inactive = accumulator$inactive_weight / accumulator$weight,
      frac_informative = accumulator$informative_weight / accumulator$weight,
      frac_strong = accumulator$strong_weight / accumulator$weight
    )
  }
  names(values) <- paste0(prefix, "_", names(values))
  values
}

proj3_anagenesis_mismatch <- function(M0, Mt, status_p, status_a, island_spec) {
  empty <- list(plant = numeric(0), animal = numeric(0))
  if (length(island_spec) == 0) {
    return(empty)
  }

  immigrant_plants <- as.numeric(island_spec[
    island_spec[, 4] == "I" & island_spec[, 8] == "plant",
    1
  ])
  immigrant_animals <- as.numeric(island_spec[
    island_spec[, 4] == "I" & island_spec[, 8] == "animal",
    1
  ])

  immigrant_plants <- immigrant_plants[
    is.finite(immigrant_plants) &
      immigrant_plants >= 1 &
      immigrant_plants <= nrow(M0)
  ]
  immigrant_animals <- immigrant_animals[
    is.finite(immigrant_animals) &
      immigrant_animals >= 1 &
      immigrant_animals <= ncol(M0)
  ]
  if (length(immigrant_plants) == 0 && length(immigrant_animals) == 0) {
    return(empty)
  }

  mismatch <- abs(Mt[seq_len(nrow(M0)), seq_len(ncol(M0)), drop = FALSE] - M0)
  plant_values <- if (length(immigrant_plants) == 0) {
    numeric(0)
  } else {
    as.numeric(mismatch %*% status_a[seq_len(ncol(M0)), , drop = FALSE])[
      immigrant_plants
    ]
  }
  animal_values <- if (length(immigrant_animals) == 0) {
    numeric(0)
  } else {
    as.numeric(t(mismatch) %*% status_p[seq_len(nrow(M0)), , drop = FALSE])[
      immigrant_animals
    ]
  }

  list(plant = plant_values, animal = animal_values)
}

proj3_species_type_counts <- function(island_spec) {
  counts <- c(nIp = 0, nAp = 0, nCp = 0, nIa = 0, nAa = 0, nCa = 0)
  if (length(island_spec) == 0) {
    return(counts)
  }

  counts[c("nIp", "nAp", "nCp")] <- vapply(
    c("I", "A", "C"),
    function(type) sum(island_spec[, 4] == type & island_spec[, 8] == "plant"),
    numeric(1)
  )
  counts[c("nIa", "nAa", "nCa")] <- vapply(
    c("I", "A", "C"),
    function(type) sum(island_spec[, 4] == type & island_spec[, 8] == "animal"),
    numeric(1)
  )
  counts
}

#' Memory-efficient diagnostic simulation for the proj3 pilot
#'
#' The stochastic process and state transitions are the same as in
#' `sim_core_mutualism()`. Full rate matrices and event-by-event state tables
#' are not retained. Instead, event counts, cumulative rate hazards, and
#' time-weighted summaries of the mutualism response terms are updated online.
#'
#' @param total_time Island age.
#' @param mutualism_pars Model parameter list from `create_mutual_pars()`.
#' @param max_events Safety cap for pathological simulations.
#' @param max_matrix_cells Safety cap on `length(Mt)`.
#' @param max_species_per_guild Safety cap on present richness in either guild.
#' @param max_runtime_s Per-simulation elapsed-time cap.
#' @param finalize_island Whether to construct DAISIE-like island output after
#'   a completed simulation.
#'
#' @return A compact diagnostic object plus the final state. The caller should
#'   extract summaries and discard the state before saving pilot results.
sim_core_mutualism_proj3 <- function(
    total_time,
    mutualism_pars,
    max_events = 20000L,
    max_matrix_cells = 4000000L,
    max_species_per_guild = 1500L,
    max_runtime_s = 7200,
    finalize_island = TRUE) {
  are_mutualism_pars <- proj3_internal_function("are_mutualism_pars")
  get_partners <- proj3_internal_function("get_partners")
  update_rates_mutual <- proj3_internal_function("update_rates_mutual")
  sample_time_mutual <- proj3_internal_function("sample_time_mutual")
  sample_event_mutual <- proj3_internal_function("sample_event_mutual")
  update_states_mutual <- proj3_internal_function("update_states_mutual")
  create_island_mutual <- proj3_internal_function("create_island_mutual")

  testit::assert(are_mutualism_pars(mutualism_pars))

  event_names <- c(
    "immigration_plant", "extinction_plant", "cladogenesis_plant",
    "anagenesis_plant", "immigration_animal", "extinction_animal",
    "cladogenesis_animal", "anagenesis_animal", "cospeciation",
    "link_gain", "link_loss"
  )

  timeval <- 0
  M0 <- mutualism_pars$M0
  Mt <- M0
  status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
  status_a <- matrix(0, nrow = ncol(M0), ncol = 1)
  maxplantID <- nrow(M0)
  maxanimalID <- ncol(M0)
  island_spec <- c()

  lac_pars <- mutualism_pars$lac_pars
  mu_pars <- mutualism_pars$mu_pars
  K_pars <- mutualism_pars$K_pars
  gam_pars <- mutualism_pars$gam_pars
  laa_pars <- mutualism_pars$laa_pars
  qgain <- mutualism_pars$qgain
  qloss <- mutualism_pars$qloss
  lambda0 <- mutualism_pars$lambda0
  transprob <- mutualism_pars$transprob
  alpha <- mutualism_pars$alpha

  if (sum(gam_pars) == 0) {
    stop("Island cannot be colonised when both immigration rates are zero.")
  }

  event_counts <- setNames(integer(length(event_names)), event_names)
  rate_integrals <- setNames(numeric(length(event_names)), event_names)
  exposures <- list(
    mu1d_all = proj3_new_exposure(0.1, 3),
    mu1d_plant = proj3_new_exposure(0.1, 3),
    mu1d_animal = proj3_new_exposure(0.1, 3),
    laa1D_all = proj3_new_exposure(0.05, 1),
    laa1D_over_laa0_all = proj3_new_exposure(0.1, 2),
    K1d_over_K0_all = proj3_new_exposure(0.1, 4),
    K1d_over_K0_immigration = proj3_new_exposure(0.1, 4)
  )

  completed <- TRUE
  stop_reason <- "completed"
  n_events <- 0L
  max_richness_p <- 0L
  max_richness_a <- 0L
  max_rows_Mt <- nrow(Mt)
  max_cols_Mt <- ncol(Mt)
  start_elapsed <- proc.time()[["elapsed"]]

  while (timeval < total_time) {
    richness_p <- sum(status_p)
    richness_a <- sum(status_a)
    max_richness_p <- max(max_richness_p, richness_p)
    max_richness_a <- max(max_richness_a, richness_a)
    max_rows_Mt <- max(max_rows_Mt, nrow(Mt))
    max_cols_Mt <- max(max_cols_Mt, ncol(Mt))

    if (n_events >= max_events) {
      completed <- FALSE
      stop_reason <- "max_events"
      break
    }
    if (length(Mt) > max_matrix_cells) {
      completed <- FALSE
      stop_reason <- "max_matrix_cells"
      break
    }
    if (max(richness_p, richness_a) > max_species_per_guild) {
      completed <- FALSE
      stop_reason <- "max_species_per_guild"
      break
    }
    if ((proc.time()[["elapsed"]] - start_elapsed) > max_runtime_s) {
      completed <- FALSE
      stop_reason <- "max_runtime_s"
      break
    }

    partners_list <- get_partners(
      Mt = Mt,
      status_p = status_p,
      status_a = status_a
    )
    rates <- update_rates_mutual(
      M0 = M0,
      Mt = Mt,
      alpha = alpha,
      status_p = status_p,
      status_a = status_a,
      lac_pars = lac_pars,
      mu_pars = mu_pars,
      K_pars = K_pars,
      gam_pars = gam_pars,
      laa_pars = laa_pars,
      qgain = qgain,
      qloss = qloss,
      lambda0 = lambda0,
      transprob = transprob,
      partners_list = partners_list,
      island_spec = island_spec
    )

    total_rate <- sum(vapply(rates, sum, numeric(1)))
    if (!is.finite(total_rate) || total_rate <= 0) {
      completed <- FALSE
      stop_reason <- "invalid_or_zero_total_rate"
      break
    }

    timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
    next_time <- timeval_and_dt$timeval
    duration <- min(timeval_and_dt$dt, total_time - timeval)

    rate_sums <- c(
      sum(rates$immig_p), sum(rates$ext_p), sum(rates$clado_p),
      sum(rates$ana_p), sum(rates$immig_a), sum(rates$ext_a),
      sum(rates$clado_a), sum(rates$ana_a), sum(rates$cospec_rate),
      sum(rates$gain_rate), sum(rates$loss_rate)
    )
    rate_integrals <- rate_integrals + rate_sums * duration

    present_p <- which(status_p == 1)
    present_a <- which(status_a == 1)
    degree_p <- as.numeric(partners_list[[1]])
    degree_a <- as.numeric(partners_list[[2]])
    mu1d_p <- mu_pars[3] * degree_p[present_p]
    mu1d_a <- mu_pars[4] * degree_a[present_a]
    exposures$mu1d_plant <- proj3_update_exposure(
      exposures$mu1d_plant, mu1d_p, duration
    )
    exposures$mu1d_animal <- proj3_update_exposure(
      exposures$mu1d_animal, mu1d_a, duration
    )
    exposures$mu1d_all <- proj3_update_exposure(
      exposures$mu1d_all, c(mu1d_p, mu1d_a), duration
    )

    mismatch <- proj3_anagenesis_mismatch(
      M0 = M0,
      Mt = Mt,
      status_p = status_p,
      status_a = status_a,
      island_spec = island_spec
    )
    laa1D <- c(laa_pars[3] * mismatch$plant, laa_pars[4] * mismatch$animal)
    laa1D_ratio <- c(
      laa_pars[3] * mismatch$plant / laa_pars[1],
      laa_pars[4] * mismatch$animal / laa_pars[2]
    )
    exposures$laa1D_all <- proj3_update_exposure(
      exposures$laa1D_all, laa1D, duration
    )
    exposures$laa1D_over_laa0_all <- proj3_update_exposure(
      exposures$laa1D_over_laa0_all, laa1D_ratio, duration
    )

    K_ratio <- c(
      K_pars[3] * degree_p[present_p] / K_pars[1],
      K_pars[4] * degree_a[present_a] / K_pars[2]
    )
    exposures$K1d_over_K0_all <- proj3_update_exposure(
      exposures$K1d_over_K0_all, K_ratio, duration
    )
    K_ratio_immigration <- c(
      K_pars[3] * degree_p[seq_len(nrow(M0))] / K_pars[1],
      K_pars[4] * degree_a[seq_len(ncol(M0))] / K_pars[2]
    )
    exposures$K1d_over_K0_immigration <- proj3_update_exposure(
      exposures$K1d_over_K0_immigration,
      K_ratio_immigration,
      duration
    )

    timeval <- next_time
    if (!is.finite(timeval) || timeval > total_time) {
      timeval <- total_time
      break
    }

    possible_event <- sample_event_mutual(rates = rates)
    event_counts[possible_event] <- event_counts[possible_event] + 1L
    n_events <- n_events + 1L

    stt_stub <- matrix(
      c(total_time - timeval, rep(0, 6)),
      nrow = 1,
      dimnames = list(
        NULL,
        c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
      )
    )
    updated_states <- update_states_mutual(
      M0 = M0,
      Mt = Mt,
      status_p = status_p,
      status_a = status_a,
      maxplantID = maxplantID,
      maxanimalID = maxanimalID,
      timeval = timeval,
      total_time = total_time,
      rates = rates,
      possible_event = possible_event,
      island_spec = island_spec,
      stt_table = stt_stub,
      transprob = transprob
    )
    Mt <- updated_states$Mt
    status_p <- updated_states$status_p
    status_a <- updated_states$status_a
    maxplantID <- updated_states$maxplantID
    maxanimalID <- updated_states$maxanimalID
    island_spec <- updated_states$island_spec
  }

  elapsed_s <- proc.time()[["elapsed"]] - start_elapsed
  max_richness_p <- max(max_richness_p, sum(status_p))
  max_richness_a <- max(max_richness_a, sum(status_a))
  max_rows_Mt <- max(max_rows_Mt, nrow(Mt))
  max_cols_Mt <- max(max_cols_Mt, ncol(Mt))

  type_counts <- proj3_species_type_counts(island_spec)
  stt_table <- rbind(
    c(total_time, 0, 0, 0, 0, 0, 0),
    c(max(0, total_time - timeval), type_counts)
  )
  colnames(stt_table) <- c("Time", names(type_counts))

  final_island_spec <- island_spec
  island <- NULL
  if (length(final_island_spec) != 0) {
    colnames(final_island_spec) <- c(
      "Species", "Mainland Ancestor", "Colonisation time (BP)",
      "Species type", "branch_code", "branching time (BP)",
      "Anagenetic_origin", "Species state"
    )
    final_island_spec[, "branching time (BP)"] <- total_time -
      as.numeric(final_island_spec[, "branching time (BP)"])
    final_island_spec[, "Colonisation time (BP)"] <- total_time -
      as.numeric(final_island_spec[, "Colonisation time (BP)"])
  }

  if (isTRUE(finalize_island) && completed) {
    if (length(final_island_spec) == 0) {
      island <- list(
        stt_table = stt_table,
        clades_info_plant = NULL,
        clades_info_animal = NULL
      )
    } else {
      island <- create_island_mutual(
        stt_table = stt_table,
        total_time = total_time,
        island_spec = final_island_spec
      )
    }
  }

  exposure_summary <- unlist(Map(
    proj3_finalize_exposure,
    exposures,
    names(exposures)
  ))
  names(exposure_summary) <- sub("^[^.]+\\.", "", names(exposure_summary))

  diagnostics <- c(
    list(
      completed = completed,
      stop_reason = stop_reason,
      simulated_time = timeval,
      elapsed_s = elapsed_s,
      n_events = n_events,
      max_richness_p = max_richness_p,
      max_richness_a = max_richness_a,
      max_rows_Mt = max_rows_Mt,
      max_cols_Mt = max_cols_Mt,
      max_matrix_cells_observed = max_rows_Mt * max_cols_Mt
    ),
    as.list(setNames(event_counts, paste0("events_", names(event_counts)))),
    as.list(setNames(rate_integrals, paste0("hazard_", names(rate_integrals)))),
    as.list(exposure_summary)
  )

  list(
    state = list(
      Mt = Mt,
      status_p = status_p,
      status_a = status_a,
      island_spec = final_island_spec,
      island = island
    ),
    diagnostics = diagnostics
  )
}
