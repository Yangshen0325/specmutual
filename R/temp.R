# simtime <- 1.5 timeval <- 0 M0 <- mutualism_pars$M0 Mt <- M0 maxplantID <-
# NROW(M0) maxanimalID <- NCOL(M0) status_p <- matrix(0, nrow = NROW(M0), ncol
# = 1) status_a <- matrix(0, nrow = NCOL(M0), ncol = 1) island_spec <- c()
# stt_table <- matrix(ncol = 7) colnames(stt_table) <- c('Time', 'nIp', 'nAp',
# 'nCp', 'nIa', 'nAa', 'nCa') stt_table[1, ] <- c(simtime, 0, 0, 0, 0, 0, 0)
# while (timeval < simtime){ rates <- update_rates_mutualism(Mt = Mt, status_p
# = status_p, status_a = status_a, mutualism_pars = mutualism_pars,
# island_spec = island_spec) # next time timeval_and_dt <-
# calc_next_timeval_mutualism(rates = rates, timeval = timeval) timeval <-
# timeval_and_dt$timeval # next event possible_event <-
# sample_event_mutualism(rates = rates) # next state based on event
# updated_state <- sim_update_state_mutualism(timeval = timeval, simtime =
# simtime, possible_event = possible_event, Mt = Mt, status_p = status_p,
# status_a = status_a, maxplantID = maxplantID, maxanimalID = maxanimalID,
# island_spec = island_spec, stt_table = stt_table, mutualism_pars =
# mutualism_pars) Mt <- updated_state$Mt status_p <- updated_state$status_p
# status_a <- updated_state$status_a maxplantID <- updated_state$maxplantID
# maxanimalID <- updated_state$maxanimalID island_spec <-
# updated_state$island_spec stt_table <- updated_state$stt_table } ####
# Finalize STT #### stt_table <- rbind(stt_table, c(0,
# stt_table[nrow(stt_table), 2], stt_table[nrow(stt_table), 3],
# stt_table[nrow(stt_table), 4], stt_table[nrow(stt_table), 5],
# stt_table[nrow(stt_table), 6], stt_table[nrow(stt_table), 7]))
# #return(stt_table) island <- create_island_mutualism(stt_table = stt_table,
# simtime = simtime, island_spec = island_spec, M0 = M0)
