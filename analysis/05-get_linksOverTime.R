

# Function to get total links over time. For a single mutualism effecrts case
# Over time means every 0.5 steps until `total_time` (island age)

get_linksOverTime <- function(data, total_time, box_data) {

  # Extract data. Network on islands every 0.5 time steps.
  allTrueMt <- lapply(data, function(rep_outputs) rep_outputs$M_true_list)

  # Deal with NULL and more the 20 lists.
  # If it's NULL, it should keep the network one step before it.
  # If it has the length over 20, only need 20.
  for (i in seq_along(allTrueMt)) {

    temV <- allTrueMt[[i]]
    temV <- temV[1:20]
    sublist <- vector("list", length(temV))
    null_value <- NULL

    for (j in seq_along(temV)) {
      if (!is.null(temV[[j]])) {
        null_value <- temV[[j]]
        sublist[[j]] <- temV[[j]]
      } else {
        sublist[[j]] <- null_value
      }
    }

    allTrueMt[[i]] <- sublist
  }

  # If the first immigration happened at the time longer than 1.0, for example, 1.2,
  # this can make the matrix at 0.5 time NULL. Deal with it and get networks links:
  sum_links <- lapply(allTrueMt, function(sublist) {
    sapply(sublist, function(x) if (is.null(x)) 0 else sum(x))
  })

  # If plot the boxplot of links over time across replicates
  if(box_data) {
    # Get the matrix of links over time
    links_matrix <- do.call(rbind, sum_links)

    # Add column names (Time)
    links_matrix <- cbind(0, links_matrix)
    column_names <- seq(total_time, 0, by = -0.5)
    colnames(links_matrix) <- column_names
    links_matrix <- as.data.frame(links_matrix)

    return(links_matrix)
  }

  # If it's not for the box plot
  # The Median, 25th, 75th percentiles
  links_matrix <- do.call(rbind, sum_links)
  quantiles <- apply(links_matrix, 2, quantile, probs = c(0.25, 0.5, 0.75))
  links_q25 <- quantiles[1, ]
  links_median <- quantiles[2, ]
  links_q75 <- quantiles[3, ]

  # Format
  linksOverTime <- data.frame("Time" = seq(total_time, 0, by = -0.5), # Time to present
                              "Links_median" = c(0, links_median),
                             "Links_q25" = c(0, links_q25),
                             "Links_q75" = c(0, links_q75))

  return(linksOverTime)

}

