#' Internal function of simulation
#' @title sim_core_mutualism
#'
#' @description
#' This internal function runs a Monte Carlo simulation to model the dynamics of mutualistic relationships
#' between plant and animal species on the island. The simulation tracks species interactions over time
#' based on defined parameters and generates an island state table reflecting species composition,
#' colonization events, and other relevant metrics.
#' The function requires a set of parameters (`mutualism_pars`) that define the initial conditions and rules
#' governing interactions, such as colonization rates and species dynamics. It returns a comprehensive
#' list containing the final state of the island, including species status, interaction matrices,
#' and an evolution table documenting significant events throughout the simulation.
#'
#' @param total_time Numeric. Total time for the simulation.
#' @param mutualism_pars List. Parameters used in the simulation.
#' @param return_parts Character. Specifies which part of the result to return. Options are:
#' - `"island_parts"` (default): Returns only the core simulation results, including the following elements:
#'   - `Mt`: A matrix representing the "false" matrix on the island, including extinct and non-immigrated species occupying
#'            their original indices from the mainland.
#'   - `M_true_list`: A list of "true" matrices left on the island (every certain ages).
#'   - `status_p`: A one column matrix indicating the presence of the plant species
#'   on the island (Presence (1) or absence (0)).
#'   - `status_a`: A one column matrix indicating the presence of the animal species
#'   on the island (Presence (1) or absence (0)).
#'   - `island_spec`: DAISIE-alike data frame. A table showing the evolutionary trajectory of species on the island, including their colonization
#'                    and branching details.
#'   - `island`: DAISIE-alike island information. A list containing three components: `stt_table`, `clades_info_plant`, and `clades_info_animal`.
#'   - `evo_table`: A data frame summarizing the evolutionary history of the species, including branching times and other evolutionary metrics.
#' - `"additional_parts"`: Returns only additional information,for checking the rates and species richness. It contains:
#'   - `rates_list`: A list containing all types of rates
#'   - `timeval_list`: A list containing all values of time
#'   - `richness_p_list`: A list containing all plant richness on the island across simulation time
#'   - `richness_a_list`: A list containing all animal richness on the island across simulation time
#'
#' @return A list

sim_core_mutualism <- function(total_time, mutualism_pars, return_parts) {

  if (return_parts == "island_parts") {
    #### Initialization ####

    # Validate the structure of `mutualism_pars`
    testit::assert(are_mutualism_pars(mutualism_pars))

    # Initialize varaibles
    timeval <- 0
    M0 <- mutualism_pars$M0
    Mt <- M0
    alpha <- mutualism_pars$alpha
    maxplantID <- nrow(M0)
    maxanimalID <- ncol(M0)
    status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
    status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

    island_spec <- c()
    stt_table <- matrix(ncol = 7)
    # species through time table, `~p` stands for plant species, `~a` animal species
    colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)

    # Extract parameters
    lac_pars <- mutualism_pars$lac_pars
    mu_pars <- mutualism_pars$mu_pars
    K_pars <- mutualism_pars$K_pars
    gam_pars <- mutualism_pars$gam_pars
    laa_pars <- mutualism_pars$laa_pars
    qgain <- mutualism_pars$qgain
    qloss <- mutualism_pars$qloss
    lambda0 <- mutualism_pars$lambda0
    transprob <- mutualism_pars$transprob

    M_true_list <- list()
    measure_interval <- 0.5
    measure_time <- measure_interval

    if (sum(gam_pars) == 0) {
      stop("Island has no species and the rate of
    colonisation is zero. Island cannot be colonised.")
    }

    # Initialize evolution table
    evo_table <- data.frame(Time = numeric(0), Event_id = numeric(0))

    #### Start Monte Carlo iterations ####

    while (timeval < total_time) {

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

      testit::assert(are_rates(rates))

      # Determine next time step
      timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
      timeval <- timeval_and_dt$timeval

      # Store matrix on island every 0.5 time step
      if (timeval > measure_time &&
          timeval - timeval_and_dt$dt < measure_time) {

        M_true <- Mt[which(status_p == 1), which(status_a == 1), drop = FALSE]
        store_index <- floor(timeval / measure_interval)
        M_true_list[[store_index]] <- M_true
        measure_time <- (store_index + 1) * measure_interval

      }

      if (timeval <= total_time) {

        # Select next event
        possible_event <- sample_event_mutual(rates = rates)
        evo_table <- rbind(evo_table, c(total_time - timeval, possible_event))

        # Update states based on the selected event
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
          stt_table = stt_table,
          transprob = transprob
        )
        Mt <- updated_states$Mt
        status_p <- updated_states$status_p
        status_a <- updated_states$status_a
        maxplantID <- updated_states$maxplantID
        maxanimalID <- updated_states$maxanimalID
        island_spec <- updated_states$island_spec
        stt_table <- updated_states$stt_table
      }
    }

    #### Finalize STT ####
    stt_table <- rbind(
      stt_table,
      c(0, stt_table[nrow(stt_table), 2:7])
    )

    #### Finalize island_spec ####
    if (length(island_spec) != 0) {
      cnames <- c(
        "Species",
        "Mainland Ancestor",
        "Colonisation time (BP)",
        "Species type",
        "branch_code",
        "branching time (BP)",
        "Anagenetic_origin",
        "Species state"
      )
      colnames(island_spec) <- cnames

      # Adjust ages counting backward from present
      island_spec[, "branching time (BP)"] <- total_time -
        as.numeric(island_spec[, "branching time (BP)"])
      island_spec[, "Colonisation time (BP)"] <- total_time -
        as.numeric(island_spec[, "Colonisation time (BP)"])
    }

    island <- create_island_mutual(
      stt_table = stt_table,
      total_time = total_time,
      island_spec = island_spec
    )

    return(list(
      Mt = Mt,
      M_true_list = M_true_list,
      status_p = status_p,
      status_a = status_a,
      island_spec = island_spec,
      island = island,
      evo_table = evo_table
    ))

  }

  if(return_parts == "additional_parts") {
    # Initialize varaibles
    timeval <- 0
    M0 <- mutualism_pars$M0
    Mt <- M0
    alpha <- mutualism_pars$alpha
    maxplantID <- nrow(M0)
    maxanimalID <- ncol(M0)
    status_p <- matrix(0, nrow = nrow(M0), ncol = 1)
    status_a <- matrix(0, nrow = ncol(M0), ncol = 1)

    island_spec <- c()
    stt_table <- matrix(ncol = 7)
    # species through time table, `~p` stands for plant species, `~a` animal species
    colnames(stt_table) <- c("Time", "nIp", "nAp", "nCp", "nIa", "nAa", "nCa")
    stt_table[1, ] <- c(total_time, 0, 0, 0, 0, 0, 0)

    # Extract parameters
    lac_pars <- mutualism_pars$lac_pars
    mu_pars <- mutualism_pars$mu_pars
    K_pars <- mutualism_pars$K_pars
    gam_pars <- mutualism_pars$gam_pars
    laa_pars <- mutualism_pars$laa_pars
    qgain <- mutualism_pars$qgain
    qloss <- mutualism_pars$qloss
    lambda0 <- mutualism_pars$lambda0
    transprob <- mutualism_pars$transprob

    rates_list <- list()
    timeval_list <- list()
    richness_p_list <- list()
    richness_a_list <- list()

    #sum_partners_p <- list()
    #sum_partners_a <- list()

    # Start Monte Carlo iterations
    while (timeval < total_time) {

      partners_list <- get_partners(
        Mt = Mt,
        status_p = status_p,
        status_a = status_a
      )

      #sum_partners_p[[length(sum_partners_p) + 1]] <- sum(partners_list$partners_p)
      #sum_partners_a[[length(sum_partners_a) + 1]] <- sum(partners_list$partners_a)

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

      # Save rates list
      rates_list[[length(rates_list) + 1 ]] <- rates

      # Determine next time step
      timeval_and_dt <- sample_time_mutual(rates = rates, timeval = timeval)
      timeval <- timeval_and_dt$timeval

      # Save time values
      timeval_list[[length(timeval_list) + 1]] <- timeval

      if (timeval <= total_time){
        # Select next event
        possible_event <- sample_event_mutual(rates = rates)

        # Update states based on the selected event
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
          stt_table = stt_table,
          transprob = transprob
        )

        Mt <- updated_states$Mt
        status_p <- updated_states$status_p
        status_a <- updated_states$status_a
        maxplantID <- updated_states$maxplantID
        maxanimalID <- updated_states$maxanimalID
        island_spec <- updated_states$island_spec
        stt_table <- updated_states$stt_table

        # Save richness for plants and animals
        richness_p_list[[length(richness_p_list) + 1]] <- sum(status_p)
        richness_a_list[[length(richness_a_list) + 1]] <- sum(status_a)
      }
    }

    return(list(rates_list = rates_list,
                timeval_list = timeval_list,
                richness_p_list = richness_p_list,
                richness_a_list = richness_a_list))
                #sum_partners_p = sum_partners_p,
                #sum_partners_a = sum_partners_a))
  }

  }


