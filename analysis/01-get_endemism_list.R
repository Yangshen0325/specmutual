
#' Get the median, 25th percentile (Q0.25), and 75th percentile (Q0.75) data list for either plant or animal data or
#' all (plant + animal)
#'
#' This function processes clade information to calculate the median and quantile statistics
#' for non-endemic, endemic, and total species counts over a specified sampling frequency.
#' The function supports both plant and animal data, adjusting automatically based on
#' the specified species type.
#'
#' @param cladeinfo List. A list containing clade information data for multiple islands.
#'   It should include data for both "several_islands_plant" and "several_islands_animal"
#' @param species_type Character. Specifies the type of species data to process, either `"plant"`
#'   or `"animal"` or `"all`. Based on this argument, the function will select the appropriate dataset
#'   and columns for analysis.
#' @param sample_freq Numeric. Defines the frequency of sampling for the simulation.
#'
#' @return A list.
#' @export

get_endemism_list <- function(cladeinfo, species_type, sample_freq) {

  # Select the appropriate cladeinfo for plants or animals
  if (species_type == "plant") {
    cladeinfo_data <- cladeinfo[["several_islands_plant"]]
    stt_column <- "stt_plant"
    col_nonendemic <- "nIp"
    col_endemic_1 <- "nAp"
    col_endemic_2 <- "nCp"
  } else if (species_type == "animal") {
    cladeinfo_data <- cladeinfo[["several_islands_animal"]]
    stt_column <- "stt_animal"
    col_nonendemic <- "nIa"
    col_endemic_1 <- "nAa"
    col_endemic_2 <- "nCa"
  } else if (species_type == "all") {
    cladeinfo_data <- cladeinfo
    stt_column <- "stt_all"
  }  else {
    stop("Invalid species_type. Choose either 'plant' or 'animal' or 'all'.")
  }

  rep <- length(cladeinfo_data)

  # Initialize space for saving data
  non_endemic_arr <- array(dim = c(sample_freq + 1, 2, rep))
  endemic_arr <- array(dim = c(sample_freq + 1, 2, rep))
  total_arr <- array(dim = c(sample_freq + 1, 2, rep))

  for (x in 1:rep) {

    if (species_type == "plant" | species_type == "animal") {
      # Extract data for non-endemic, endemic, and total species counts based on species type
      sum_nonendemic_all <- cladeinfo_data[[x]][[1]][[stt_column]][, col_nonendemic]
      sum_endemic_all <- cladeinfo_data[[x]][[1]][[stt_column]][, col_endemic_1] +
        cladeinfo_data[[x]][[1]][[stt_column]][, col_endemic_2]
      total_all <- cladeinfo_data[[x]][[1]][[stt_column]][, col_nonendemic] +
        cladeinfo_data[[x]][[1]][[stt_column]][, col_endemic_1] +
        cladeinfo_data[[x]][[1]][[stt_column]][, col_endemic_2]

      # Populate arrays with Time and calculated values
      non_endemic_arr[, , x] <- cbind(cladeinfo_data[[x]][[1]][[stt_column]][, 'Time'],
                                      sum_nonendemic_all)
      endemic_arr[, , x] <- cbind(cladeinfo_data[[x]][[1]][[stt_column]][, 'Time'],
                                  sum_endemic_all)
      total_arr[, , x] <- cbind(cladeinfo_data[[x]][[1]][[stt_column]][, 'Time'],
                                total_all)
    }
    if (species_type == "all") {
      # Extract data for non-endemic, endemic, and total species counts
      sum_nonendemic_all <- cladeinfo[[x]][[1]][[stt_column]][, "nIp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nIa"]

      sum_endemic_all <- cladeinfo[[x]][[1]][[stt_column]][, "nAp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nCp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nAa"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nCa"]

      total_all <- cladeinfo[[x]][[1]][[stt_column]][, "nIp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nAp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nCp"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nIa"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nAa"] +
        cladeinfo[[x]][[1]][[stt_column]][, "nCa"]

      non_endemic_arr[, , x] <- cbind(cladeinfo[[x]][[1]][[stt_column]][, 'Time'],
                                      sum_nonendemic_all)

      endemic_arr[, , x] <- cbind(cladeinfo[[x]][[1]][[stt_column]][, 'Time'],
                                  sum_endemic_all)

      total_arr[, , x] <- cbind(cladeinfo[[x]][[1]][[stt_column]][, 'Time'],
                                total_all)
    }

  }

  # Calculate median and quantiles for non-endemic data
  non_endemic_average <- apply(non_endemic_arr, c(1, 2), stats::median)
  non_endemic_q0.25 <- apply(non_endemic_arr, c(1, 2), stats::quantile, 0.25)
  non_endemic_q0.75 <- apply(non_endemic_arr, c(1, 2), stats::quantile, 0.75)
  non_endemic_dat <- cbind(non_endemic_average, non_endemic_q0.25[, -1], non_endemic_q0.75[, -1])
  colnames(non_endemic_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  non_endemic_dat <- as.data.frame(non_endemic_dat)

  # Calculate median and quantiles for endemic data
  endemic_average <- apply(endemic_arr, c(1, 2), stats::median)
  endemic_q0.25 <- apply(endemic_arr, c(1, 2), stats::quantile, 0.25)
  endemic_q0.75 <- apply(endemic_arr, c(1, 2), stats::quantile, 0.75)
  # Combine them together, remove the first column of `non_endemic_q0.25` and `non_endemic_q0.75`
  # coz that's "Time"
  endemic_dat <- cbind(endemic_average, endemic_q0.25[, -1], endemic_q0.75[, -1])
  colnames(endemic_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  endemic_dat <- as.data.frame(endemic_dat)

  # Calculate median and quantiles for total data
  total_average <- apply(total_arr, c(1, 2), stats::median)
  total_q0.25 <- apply(total_arr, c(1, 2), stats::quantile, 0.25)
  total_q0.75 <- apply(total_arr, c(1, 2), stats::quantile, 0.75)
  total_dat <- cbind(total_average, total_q0.25[, -1], total_q0.75[, -1])
  colnames(total_dat) <- c("Time", "Median", "Q0.25", "Q0.75")
  total_dat <- as.data.frame(total_dat)

  # Compile and return all data as a list
  infolist <- list(
    non_endemic_dat = non_endemic_dat,
    endemic_dat = endemic_dat,
    total_dat = total_dat
  )

  return(infolist)
}





