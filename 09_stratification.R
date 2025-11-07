#### load RData ####
setwd(working_directory_data)
load("commondata.Rdata")
ruca_data <- readxl::read_xlsx("ruca2010revised.xlsx")
names(ruca_data) <- c("CountyFIPS_Ruca","State","County","TractFIPS","Primary_Ruca_Code","Secondary_Ruca_Code","Tract_Population","Land_Area","Population_Density")
ruca_data$TractFIPS <- as.numeric(ruca_data$TractFIPS)
ruca_data$Primary_Ruca_Code <- ifelse(ruca_data$Primary_Ruca_Code == 1, 1, 0)
setwd(working_directory_output)
# Perform the left join (keeping all rows from subset_data)
subset_data <- merge(subset_data, ruca_data, 
                     by.x = "TractFIPS.1", 
                     by.y = "TractFIPS", 
                     all.x = TRUE)

# Count how many TractFIPS.1 didn't get a match
unmatched_count <- sum(is.na(subset_data$Primary_Ruca_Code))
cat("Number of TractFIPS.1 without matching data:", unmatched_count, "\n")

# Alternatively, to see which specific TractFIPS.1 didn't match:
unmatched_tracts <- subset_data$TractFIPS.1[!subset_data$TractFIPS.1 %in% ruca_data$TractFIPS]

figure_2_filterer <- function(lowhigh_vars, data) {
  # Initialize a list to store all results for each variable in lowhigh_vars
  all_results <- list()
  
  for (vars in lowhigh_vars) {
    # Split data into low and high groups based on median
    highlow <- quantile(data[, vars], probs = seq(0, 1, by = 1/2), na.rm = TRUE)
    filtered_low <- data[data[vars] < highlow[[2]], ]
    filtered_high <- data[data[vars] >= highlow[[2]], ]
    
    # Initialize lists to store model results for this variable
    results_list <- list(
      low = list(
        coefficients_chd = list(),
        se_chd = list(),
        lower_bound_chd = list(),
        upper_bound_chd = list(),
        coefficients_stroke = list(),
        se_stroke = list(),
        lower_bound_stroke = list(),
        upper_bound_stroke = list()
      ),
      high = list(
        coefficients_chd = list(),
        se_chd = list(),
        lower_bound_chd = list(),
        upper_bound_chd = list(),
        coefficients_stroke = list(),
        se_stroke = list(),
        lower_bound_stroke = list(),
        upper_bound_stroke = list()
      )
    )
    
    # Process both filtered_low and filtered_high
    for (filter_type in c("low", "high")) {
      current_data <- if (filter_type == "low") filtered_low else filtered_high
      
      # Calculate average prevalence values
      avg_CHD <- mean(current_data$CHD_CrudePrev, na.rm = TRUE)
      avg_STROKE <- mean(current_data$STROKE_CrudePrev, na.rm = TRUE)
      
      # Calculate average anomaly sizes for climate variables
      climate_vars <- c(
        "HI_C_Change70_79_13_22", "Change_Temp_C_70_79_13_22", "Change_RH_70_79_13_22",
        "windU_diff70_79_13_22", "windV_diff70_79_13_22", "latent_diff70_79_13_22_P_Mill",
        "solar_diff70_79_13_22_P_Mill", "thermal_diff70_79_13_22_P_Mill",
        "sensible_diff70_79_13_22_P_Mill", "evap_diff70_79_13_22_P_x10",
        "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
        "transpir_diff70_79_13_22_P_x1000", "downwards_solar_diff70_79_13_22_P_Mill"
      )
      
      avg_values <- sapply(climate_vars, function(var) mean(current_data[[var]], na.rm = TRUE))
      
      # Define common control variables for models
      control_vars <- c(
        "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
        "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
        "Temp_C_2013_2022", "LCchangeMEAN"
      )
      
      # Function to run mixed-effects model and extract results
      run_model <- function(dependent_var, independent_var, data, avg_value) {
        formula <- as.formula(paste(
          dependent_var, "~", independent_var, "+",
          paste(control_vars, collapse = " + "), "+ (1 | CountyFIPS)"
        ))
        
        model <- lmer(formula, data = data)
        model_summary <- summary(model)
        
        # Extract and scale coefficients
        estimate <- coef(model_summary)[independent_var, "Estimate"] * avg_value
        se <- coef(model_summary)[independent_var, "Std. Error"]
        
        return(list(
          estimate = estimate,
          se = se,
          lower = estimate - 1.96 * se,
          upper = estimate + 1.96 * se
        ))
      }
      
      # Run models for CHD
      for (var in climate_vars) {
        simple_name <- var
        results <- run_model("CHD_CrudePrev", var, current_data, avg_values[simple_name])
        
        results_list[[filter_type]]$coefficients_chd[[paste0("chd_", simple_name)]] <- results$estimate
        results_list[[filter_type]]$se_chd[[paste0("chd_", simple_name)]] <- results$se
        results_list[[filter_type]]$lower_bound_chd[[paste0("chd_", simple_name)]] <- results$lower
        results_list[[filter_type]]$upper_bound_chd[[paste0("chd_", simple_name)]] <- results$upper
      }
      
      # Run models for Stroke
      for (var in climate_vars) {
        simple_name <- var
        results <- run_model("STROKE_CrudePrev", var, current_data, avg_values[simple_name])
        
        results_list[[filter_type]]$coefficients_stroke[[paste0("stroke_", simple_name)]] <- results$estimate
        results_list[[filter_type]]$se_stroke[[paste0("stroke_", simple_name)]] <- results$se
        results_list[[filter_type]]$lower_bound_stroke[[paste0("stroke_", simple_name)]] <- results$lower
        results_list[[filter_type]]$upper_bound_stroke[[paste0("stroke_", simple_name)]] <- results$upper
      }
      
      # Create results data frames
      variable_names <- c(
        "Heat Index", "Air Temperature", "Humidity", "Eastward Wind", "Northward Wind",
        "Latent Heat Flux", "Surface-Absorbed Sunlight", "Thermal Radiation",
        "Sensible Heat Flux", "Evaporation", "Surface Pressure", "Precipitation",
        "Transpiration", "Sunlight"
      )
      
      chd_fig2 <- data.frame(
        Variable = variable_names,
        SE = unlist(results_list[[filter_type]]$se_chd),
        Effect_Anomaly_Size = unlist(results_list[[filter_type]]$coefficients_chd),
        Lower_CI = unlist(results_list[[filter_type]]$lower_bound_chd),
        Upper_CI = unlist(results_list[[filter_type]]$upper_bound_chd),
        stringsAsFactors = FALSE
      )
      
      stroke_fig2 <- data.frame(
        Variable = variable_names,
        SE = unlist(results_list[[filter_type]]$se_stroke),
        Effect_Anomaly_Size = unlist(results_list[[filter_type]]$coefficients_stroke),
        Lower_CI = unlist(results_list[[filter_type]]$lower_bound_stroke),
        Upper_CI = unlist(results_list[[filter_type]]$upper_bound_stroke),
        stringsAsFactors = FALSE
      )
      
      # Format confidence intervals
      chd_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", chd_fig2$Lower_CI, chd_fig2$Upper_CI)
      stroke_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", stroke_fig2$Lower_CI, stroke_fig2$Upper_CI)
      
      # Store the results for this filter type
      results_list[[filter_type]]$chd_results <- chd_fig2
      results_list[[filter_type]]$stroke_results <- stroke_fig2
    }
    
    # Store all results for this variable
    all_results[[vars]] <- results_list
  }
  
  return(all_results)
}

results <- figure_2_filterer(c("SPL_THEME1","Temp_C_2013_2022","Primary_Ruca_Code"), subset_data)

combined_results_chd_SPL <- as.data.frame(cbind(
  results$SPL_THEME1$low$chd_results$Variable,
  results$SPL_THEME1$low$chd_results$Effect_Anomaly_Size,
  results$SPL_THEME1$low$chd_results$Combined_CI,
  results$SPL_THEME1$high$chd_results$Effect_Anomaly_Size,
  results$SPL_THEME1$high$chd_results$Combined_CI
))
combined_results_stroke_SPL <- as.data.frame(cbind(
  results$SPL_THEME1$low$stroke_results$Variable,
  results$SPL_THEME1$low$stroke_results$Effect_Anomaly_Size,
  results$SPL_THEME1$low$stroke_results$Combined_CI,
  results$SPL_THEME1$high$stroke_results$Effect_Anomaly_Size,
  results$SPL_THEME1$high$stroke_results$Combined_CI
))

combined_results_chd_Temp <- as.data.frame(cbind(
  results$Temp_C_2013_2022$low$chd_results$Variable,
  results$Temp_C_2013_2022$low$chd_results$Effect_Anomaly_Size,
  results$Temp_C_2013_2022$low$chd_results$Combined_CI,
  results$Temp_C_2013_2022$high$chd_results$Effect_Anomaly_Size,
  results$Temp_C_2013_2022$high$chd_results$Combined_CI
))
combined_results_stroke_Temp <- as.data.frame(cbind(
  results$Temp_C_2013_2022$low$stroke_results$Variable,
  results$Temp_C_2013_2022$low$stroke_results$Effect_Anomaly_Size,
  results$Temp_C_2013_2022$low$stroke_results$Combined_CI,
  results$Temp_C_2013_2022$high$stroke_results$Effect_Anomaly_Size,
  results$Temp_C_2013_2022$high$stroke_results$Combined_CI
))

combined_results_chd_Ruca <- as.data.frame(cbind(
  results$Primary_Ruca_Code$low$chd_results$Variable,
  results$Primary_Ruca_Code$low$chd_results$Effect_Anomaly_Size,
  results$Primary_Ruca_Code$low$chd_results$Combined_CI,
  results$Primary_Ruca_Code$high$chd_results$Effect_Anomaly_Size,
  results$Primary_Ruca_Code$high$chd_results$Combined_CI
))

combined_results_stroke_Ruca <- as.data.frame(cbind(
  results$Primary_Ruca_Code$low$stroke_results$Variable,
  results$Primary_Ruca_Code$low$stroke_results$Effect_Anomaly_Size,
  results$Primary_Ruca_Code$low$stroke_results$Combined_CI,
  results$Primary_Ruca_Code$high$stroke_results$Effect_Anomaly_Size,
  results$Primary_Ruca_Code$high$stroke_results$Combined_CI
))

names(combined_results_chd_Ruca) <- c("Variable","Low","Low_CI","High","High_CI")
names(combined_results_stroke_Ruca) <- c("Variable","Low","Low_CI","High","High_CI")
names(combined_results_chd_SPL) <- c("Variable","Low","Low_CI","High","High_CI")
names(combined_results_stroke_SPL) <- c("Variable","Low","Low_CI","High","High_CI")
names(combined_results_chd_Temp) <- c("Variable","Low","Low_CI","High","High_CI")
names(combined_results_stroke_Temp) <- c("Variable","Low","Low_CI","High","High_CI")

fig_strat_low_Ruca_data <- reformat_forest_plot(
  table_1 = combined_results_chd_Ruca,
  table_2 = combined_results_stroke_Ruca,
  value_col = "Low",
  ci_col = "Low_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_low_Ruca_data)

fig_strat_high_Ruca_data <- reformat_forest_plot(
  table_1 = combined_results_chd_Ruca,
  table_2 = combined_results_stroke_Ruca,
  value_col = "High",
  ci_col = "High_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_high_Ruca_data)

fig_strat_low_Temp_data <- reformat_forest_plot(
  table_1 = combined_results_chd_Temp,
  table_2 = combined_results_stroke_Temp,
  value_col = "Low",
  ci_col = "Low_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_low_Temp_data)

fig_strat_high_Temp_data <- reformat_forest_plot(
  table_1 = combined_results_chd_Temp,
  table_2 = combined_results_stroke_Temp,
  value_col = "High",
  ci_col = "High_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_high_Temp_data)

fig_strat_low_SPL_data <- reformat_forest_plot(
  table_1 = combined_results_chd_SPL,
  table_2 = combined_results_stroke_SPL,
  value_col = "Low",
  ci_col = "Low_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_low_SPL_data)

fig_strat_high_SPL_data <- reformat_forest_plot(
  table_1 = combined_results_chd_SPL,
  table_2 = combined_results_stroke_SPL,
  value_col = "High",
  ci_col = "High_CI",
  x_label = "Prevalence",
  reference_lines = c(-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
)
print(fig_strat_high_SPL_data)

ggsave("TableS8_forestplot_low_SPL.png", fig_strat_low_SPL_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS8_forestplot_high_SPL.png", fig_strat_high_SPL_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS8_forestplot_low_Temp.png", fig_strat_low_Temp_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS8_forestplot_high_Temp.png", fig_strat_high_Temp_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS8_forestplot_low_Ruca.png", fig_strat_low_Ruca_data$plot, width = 2, height = 6, dpi = 300)
ggsave("TableS8_forestplot_high_Ruca.png", fig_strat_high_Ruca_data$plot, width = 2, height = 6, dpi = 300)


write.csv(combined_results_chd_SPL, "TableS8_combined_results_chd_SPL.csv", row.names = FALSE)
write.csv(combined_results_stroke_SPL, "TableS8_combined_results_stroke_SPL.csv", row.names = FALSE)
write.csv(combined_results_chd_Temp, "TableS8_combined_results_chd_Temp.csv", row.names = FALSE)
write.csv(combined_results_stroke_Temp, "TableS8_combined_results_stroke_Temp.csv", row.names = FALSE)
write.csv(combined_results_chd_Ruca, "TableS8_combined_results_chd_Ruca.csv", row.names = FALSE)
write.csv(combined_results_stroke_Ruca, "TableS8_combined_results_stroke_Ruca.csv", row.names = FALSE)
