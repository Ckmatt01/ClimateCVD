###############################################################################################
# BEST MODEL FINDER ----
#
# Generate R^2 sensitivity analysis -> generates list of variables that are the
# largest independent set below a certain R^2 threshold with the lowest correlation
# between those variables. Then, generates models based on those variables.
# Performs this for 0.8, 0.6, 0.4, and 0.2 
###############################################################################################

setwd(working_directory_output)

# Generate correlation matrix from model variables

cor_matrix <- cor(subset_data[, c("HI_C_Change70_79_13_22", "Change_Temp_C_70_79_13_22", "Change_RH_70_79_13_22",
                                  "windU_diff70_79_13_22", "windV_diff70_79_13_22", "latent_diff70_79_13_22",
                                  "solar_diff70_79_13_22", "thermal_diff70_79_13_22", "sensible_diff70_79_13_22", "evap_diff70_79_13_22",
                                  "pressure_diff70_79_13_22", 
                                  "precip_diff70_79_13_22", "transpir_diff70_79_13_22", "downwards_solar_diff70_79_13_22"), drop = FALSE], method = "pearson", use = "complete.obs")
cor_matrix <- cor_matrix^2

# Create lists to store results

results_list <- list()
cor_thresholds <- c(0.80000, 0.60000, 0.40000, 0.20000)

# For each threshold
for (i in seq_along(cor_thresholds)) {
  current_threshold <- cor_thresholds[i]
  
  # Create boolean of cor_matrix where edges represent strong correlations (>= threshold)
  cor_adj <- abs(cor_matrix) >= current_threshold
  
  # Removes self-loops
  diag(cor_adj) <- FALSE
  
  # Create adjacency matrix where edges represent weak correlations
  g <- graph_from_adjacency_matrix(cor_adj, mode = "undirected")
  
  cor_all_largest_sets <- largest_ivs(g)
  
  ####
  #' Function to compute mean correlation for each set
  #' 
  #' @param nodes Character vector of node/variable names to include in correlation calculation.
  #' These should correspond to row/column names in the precomputed correlation matrix.
  #' @return Numeric value representing the mean absolute correlation between all unique 
  #' pairs of variables in the node set
  ####
  mean_correlation <- function(nodes) {
    mean(cor_matrix[nodes, nodes][upper.tri(cor_matrix[nodes, nodes])])
  }
  
  # Calculate mean correlation for each set
  cor_set_means <- sapply(cor_all_largest_sets, function(set) {
    nodes <- V(g)$name[set]
    mean_correlation(nodes)
  })
  
  #### Find the largest independent set that includes heat index WITH the lowest mean correlation ####
  
  cor_HI_include <- "HI_C_Change70_79_13_22"
  sets_with_HI <- which(sapply(cor_all_largest_sets, function(set) {
    cor_HI_include %in% V(g)$name[set]
  }))
  
  if (length(sets_with_HI) > 0) {
    # Select the set with the lowest mean correlation among those that include HI_C_Change70_79_13_22
    cor_lowest_avg_set_HI <- cor_all_largest_sets[[sets_with_HI[which.min(abs(cor_set_means[sets_with_HI]))]]]
    cor_selected_HI <- V(g)$name[cor_lowest_avg_set_HI]
    
    # Generate correlation matrix for this selection
    cor_matrix_HI <- cor_matrix[cor_selected_HI, cor_selected_HI]
  } else {
    cor_selected_HI <- NULL
    cor_matrix_HI <- NULL
    warning(paste("No independent sets found that include 'HI_C_Change70_79_13_22' for threshold", current_threshold))
  }
  
  #### Find the largest independent set regardless of "HI_C_Change70_79_13_22" WITH the lowest mean correlation (DEPRECATED) ####
  # Select the set with the lowest mean correlation
  cor_lowest_avg_set_global <- cor_all_largest_sets[[which.min(abs(cor_set_means))]]
  cor_selected_global <- V(g)$name[cor_lowest_avg_set_global]
  
  # Generate correlation matrix for this selection
  cor_matrix_global <- cor_matrix[cor_selected_global, cor_selected_global]
  
  # Store results for this threshold
  results_list[[i]] <- list(
    threshold = current_threshold,
    cor_selected_HI = cor_selected_HI,
    cor_matrix_HI = cor_matrix_HI,
    cor_selected_global = cor_selected_global,
    cor_matrix_global = cor_matrix_global,
    mean_corr_HI = if (!is.null(cor_selected_HI)) mean_correlation(cor_selected_HI) else NA,
    mean_corr_global = mean_correlation(cor_selected_global)
  )
  
  # Print results for current threshold
  cat("\nResults for threshold:", current_threshold, "\n")
  cat("Selected variables including HI:", if (!is.null(cor_selected_HI)) paste(cor_selected_HI, collapse = ", ") else "None", "\n")
  cat("Mean correlation (HI set):", if (!is.null(cor_selected_HI)) mean_correlation(cor_selected_HI) else NA, "\n")
  cat("Selected variables (global):", paste(cor_selected_global, collapse=", "), "\n")
  cat("Mean correlation (global set):", mean_correlation(cor_selected_global), "\n")
}