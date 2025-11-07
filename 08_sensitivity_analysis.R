###############################################################################################
# MODEL GENERATOR (SENSITIVITY) ----
#
# 
###############################################################################################

setwd(working_directory_output)

scaled_model_9e <- c(
  "HI_C_Change70_89_03_22",
  "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
  "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
  "Temp_C_2013_2022.x", "LCchangeMEAN", "windU_diff70_89_03_22",
  "windV_diff70_89_03_22", "evap_diff70_89_03_22_P_Ten", "pressure_diff70_89_03_22_scale",
  "transpir_diff70_89_03_22_scale", "downwards_solar_diff70_89_03_22_P_Mill"
)
scaled_model_old_sens <- c(
  "HI_C_Change70_89_03_22",
  "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
  "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
  "Temp_C_2013_2022.x", "LCchangeMEAN", "windU_diff70_89_03_22",
  "windV_diff70_89_03_22","solar_diff70_89_03_22_scale", "pressure_diff70_89_03_22_scale",
  "precip_diff70_89_03_22_scale", "transpir_diff70_89_03_22_scale"
)

scaled_model_HI <- c("HI_C_Change70_89_03_22",
                     "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
                     "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
                     "Temp_C_2013_2022.x", "LCchangeMEAN"
)

model_specifications <- list(
  model_9e = scaled_model_9e,
  model_old_sens = scaled_model_old_sens,
  model_HI = scaled_model_HI
)

#### Find mean crude prevalence of CHD and stroke ####
avg_stroke <- mean(sensitivity70_89_03_22_merged$STROKE_CrudePrev, na.rm = TRUE)
avg_CHD <- mean(sensitivity70_89_03_22_merged$CHD_CrudePrev, na.rm = TRUE)


####
#' Run mixed-effects model and extract results
#'
#' @param model_name Name for the model (used in output files)
#' @param predictors predictor variables (e.x. c("HI_C_Change70_79_13_22", "OBESITY_CrudePrev_P_div10",...))
#' @param dependent_var Name of dependent variable (e.x. CHD_CrudePrev)
#' @param dependent_formula Formula fragment for lmer (e.x. CHD_CrudePrev ~)
#' @param data Dataset containing all variables (e.x. subset data)
#' @return List containing model object and results dataframe
####

run_model <- function(model_name, predictors, dependent_var, dependent_formula) {
  # Create formula
  formula_str <- paste(dependent_formula, paste(predictors, collapse = " + "), 
                       "+ (1 | CountyFIPS)")
  formula_obj <- as.formula(formula_str)
  
  # Run model with error handling
  model <- tryCatch({
    lmer(formula_obj, data = sensitivity70_89_03_22_merged)
  }, error = function(e) {
    message(paste("Error in model", model_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(model)) {
    return(NULL)
  }
  
  # Store model in global environment
  # assign(model_name, model, envir = .GlobalEnv) ## CAN POTENTIALLY REMOVE
  
  # Extract summary statistics
  model_summary <- summary(model)
  model_r2 <- r.squaredGLMM(model)
  model_aic <- AIC(model)
  model_bic <- BIC(model)
  model_rmse <- rmse(model)
  
  # Prepare results for all variables
  model_results_list <- list()
  
  for (var in predictors) {
    # Check if variable exists in coefficients
    if (!var %in% rownames(model_summary$coefficients)) {
      warning(paste("Variable", var, "not found in model coefficients for", model_name))
      next
    }
    
    # Extract coefficients
    coefficients <- model_summary$coefficients[, "Estimate"]
    se <- model_summary$coefficients[, "Std. Error"]
    tvalue <- model_summary$coefficients[, "t value"]
    lower_bound <- coefficients - 1.96 * se
    upper_bound <- coefficients + 1.96 * se
    
    # Calculate percent contribution if variable exists in data
    if (var %in% names(sensitivity70_89_03_22_merged)) {
      avg_var <- mean(sensitivity70_89_03_22_merged[[var]], na.rm = TRUE) # Ex: mean(sensitivity70_89_03_22_merged[["HI_C_Change70_79_13_22"]] na.arm = TRUE)
      avg_dependent <- mean(sensitivity70_89_03_22_merged[[dependent_var]], na.rm = TRUE) # Ex: mean(sensitivity70_89_03_22_merged[[CHD_CrudePrev]])
      pct_contribution <- avg_var * coefficients[var] / avg_dependent * 100 # Ex: mean of "HI_C_Change70_79_13_22" * HI_C_Change70_79_13_22 effect size (giving anomaly effect size) / avg_CHD * 100
    } else {
      pct_contribution <- NA
    }
    
    # Create results data frame for this variable
    model_results_list[[var]] <- data.frame(
      Model = model_name,
      Dependent = dependent_var,
      Variable = var,
      Effect_Size = coefficients[var],
      T_Value = tvalue[var],
      CI = paste0("(", round(lower_bound[var], 2), ", ", round(upper_bound[var], 2), ")"),
      Percent_Contribution = pct_contribution,
      R2m = model_r2[1],
      R2c = model_r2[2],
      AIC = model_aic,
      BIC = model_bic,
      RMSE = model_rmse,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all variables' results
  results <- do.call(rbind, model_results_list)
  
  # Assign each variable's results to global environment
  for (var in predictors) {
    # Create a unique name for each result (e.g., "chd_9a_HI_C_Change_results")
    result_name <- paste0(model_name, "_", gsub("[^[:alnum:]]", "_", var), "_results")
    if (var %in% names(model_results_list)) {
      assign(result_name, model_results_list[[var]], envir = .GlobalEnv)
    }
  }
  
  # Assign the complete model results to global environment
  assign(paste0(model_name, "_full_results"), results, envir = .GlobalEnv)
  
  # Round numeric columns
  numeric_cols <- c("Effect_Size", "T_Value", "Percent_Contribution", "R2m", "R2c", "AIC", "BIC")
  results[numeric_cols] <- lapply(results[numeric_cols], function(x) round(x, 2))
  
  # Save results to CSV
  write.csv(results, paste0("TableS4_fitstats_", model_name, "_results.csv"), row.names = FALSE)
  
  return(list(model = model, results = results))
}

# Run all models and store results
all_results_sens <- list()
for (i in seq_along(model_dependents)) {
  dependent_var <- model_dependents[i]
  dependent_formula <- model_dependents_formula[i]
  
  for (model_name in names(model_specifications)) {
    cat("\nRunning model:", model_name, "for dependent variable:", dependent_var, "\n")
    
    model_result <- run_model(
      model_name = paste0(model_name, "_", dependent_var),
      predictors = model_specifications[[model_name]],
      dependent_var = dependent_var,
      dependent_formula = dependent_formula
    )
    
    if (!is.null(model_result)) {
      all_results_sens[[paste0(model_name, "_", dependent_var)]] <- model_result
      # Print summary of the model
      print(summary(model_result$model))
    }
  }
}

###############################################################################################
# TABLE 1 GENERATOR (SENSITIVITY) ----
#
# This script generates table 1
###############################################################################################

####
#' Create results table for a given model
#' @param model Model object (e.x. model_9a_CHD_CrudePrev)
#' @param model_name Name for the model (used in output files) (e.x. "model_9a_CHD_table")
#' @param data Dataset containing all variables (e.x. subset_data)
#' @param avg_model Average value of the dependent variable (e.x. avg_CHD)
#' @param time_period Time period label for output files (e.x. 70_89_03_22)
#' @return Formatted results table

####

create_results_table <- function(model, model_name, raw_input, avg_model, time_period) {
  # load in values from model summary
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients[, "Estimate"]
  se <- model_summary$coefficients[, "Std. Error"]
  tvalue <- model_summary$coefficients[, "t value"]
  lower_bound <- coefficients - 1.96 * se
  upper_bound <- coefficients + 1.96 * se
  # Make list that aligns variable names and their 'pretty' names
  vars <- list(
    HI = list(name = "HI_C_Change70_89_03_22", label = "Heat Index Anomaly"),
    temp = list(name = "Change_Temp_C_70_89_03_22", label = "Air Temperature Anomaly"),
    humidity = list(name = "Change_RH_70_89_03_22", label = "Humidity Anomaly"),
    windU = list(name = "windU_diff70_89_03_22", label = "Eastward Wind Anomaly"),
    windV = list(name = "windV_diff70_89_03_22", label = "Northward Wind Anomaly"),
    latent = list(name = "latent_diff70_89_03_22_scale", label = "Latent Heat Flux Anomaly"),
    solar = list(name = "solar_diff70_89_03_22_scale", label = "Absorbed Sunlight Anomaly"),
    thermal = list(name = "thermal_diff70_89_03_22_scale", label = "Thermal Radiation Anomaly"),
    sensible = list(name = "sensible_diff70_89_03_22_scale", label = "Sensible Heat Flux Anomaly"),
    evaporation = list(name = "evap_diff70_89_03_22_P_Ten", label = "Evaporation Anomaly"),
    pressure = list(name = "pressure_diff70_89_03_22_scale", label = "Surface Pressure Anomaly"),
    precipitation = list(name = "precip_diff70_89_03_22_scale", label = "Precipitation Anomaly"),
    transpir = list(name = "transpir_diff70_89_03_22_scale", label = "Transpiration Anomaly"),
    sunlight = list(name = "downwards_solar_diff70_89_03_22_P_Mill", label = "Sunlight Anomaly")
  )
  
  # Fix names
  vars <- vars[sapply(vars, function(x) x$name %in% names(coefficients))]
  # calculate results
  results <- lapply(vars, function(var) {
    avg <- mean(raw_input[[var$name]], na.rm = TRUE)
    rate <- avg * coefficients[var$name]
    lower_CI <- rate - 1.96 * se[var$name]
    upper_CI <- rate + 1.96 * se[var$name]
    pct <- rate / avg_model * 100
    # generate list with results
    list(
      Variable = var$label,
      avg = avg,
      rate = rate,
      lower_CI = lower_CI,
      upper_CI = upper_CI,
      pct = pct
    )
  })
  
  new_data <- raw_input
  for (var in vars) {
    new_data[[paste0("exposure_", var$name)]] <- (new_data[[var$name]]) * new_data$Pop18Over
  }
  # generate more results
  pop_effects <- lapply(vars, function(var) {
    av_natl_expos <- sum(new_data[[paste0("exposure_", var$name)]], na.rm = TRUE) / sum(new_data$Pop18Over, na.rm = TRUE)
    popweight_effectsize <- av_natl_expos * coefficients[var$name]
    lower_CI_natl <- popweight_effectsize - 1.96 * se[var$name]
    upper_CI_natl <- popweight_effectsize + 1.96 * se[var$name]
    
    list(
      av_natl_expos = av_natl_expos,
      popweight_effectsize = popweight_effectsize,
      lower_CI_natl = lower_CI_natl,
      upper_CI_natl = upper_CI_natl
    )
  })
  
  # Determine which prevalence measure to use based on the avg_model parameter
  if(identical(avg_model, avg_stroke)) {
    new_data$pop_prev <- new_data$STROKE_CrudePrev * new_data$Pop18Over
    prevalence_label <- "stroke"
  } else if(identical(avg_model, avg_CHD)) {
    new_data$pop_prev <- new_data$CHD_CrudePrev * new_data$Pop18Over
    prevalence_label <- "chd"
  } else {
    stop("avg_model must be either avg_stroke or avg_CHD")
  }
  
  av_natl_prev <- sum(new_data$pop_prev, na.rm = TRUE) / sum(new_data$Pop18Over, na.rm = TRUE)
  
  pct_popweight <- sapply(pop_effects, function(x) x$popweight_effectsize / av_natl_prev * 100)
  # Generate dataframe that calculates values of interest
  results_df <- data.frame(
    Variable = sapply(results, function(x) x$Variable),
    Effect_Size = sapply(vars, function(x) coefficients[x$name]),
    CI_1 = paste0("(", round(lower_bound[sapply(vars, function(x) x$name)], 2), 
                  ", ", round(upper_bound[sapply(vars, function(x) x$name)], 2), ")"),
    T_Value = round(sapply(vars, function(x) tvalue[x$name]), 2),
    Anomaly_Size = round(sapply(results, function(x) x$avg), 2),
    Effect_Size_Anomaly = round(sapply(results, function(x) x$rate), 2),
    CI_2 = paste0("(", round(sapply(results, function(x) x$lower_CI), 2), 
                  ", ", round(sapply(results, function(x) x$upper_CI), 2), ")"),
    Percent_Contribution = round(sapply(results, function(x) x$pct), 2),
    Average_National_Exposure = round(sapply(pop_effects, function(x) x$av_natl_expos), 2),
    Pop_Weighted_Effect_Size = round(sapply(pop_effects, function(x) x$popweight_effectsize), 2),
    CI_3 = paste0("(", round(sapply(pop_effects, function(x) x$lower_CI_natl), 2), 
                  ", ", round(sapply(pop_effects, function(x) x$upper_CI_natl), 2), ")"),
    Pop_Weight_Percent_Contribution = round(pct_popweight, 2)
  )
  # generate final dataframe that includes all values of interest
  final_table <- results_df %>%
    mutate(
      Effect_Size_Per_Unit = as.character(round(Effect_Size, 2)),
      Effect_Size_Per_Unit_CI = CI_1,
      Effect_Size_Anomaly = as.character(round(Effect_Size_Anomaly, 2)),
      Effect_Size_Anomaly_CI = CI_2,
      Pop_Weight_Effect_Size_Anomaly = as.character(Pop_Weighted_Effect_Size),
      Pop_Weight_Effect_Size_Anomaly_CI = CI_3
    ) %>%
    select(
      Variable,
      Effect_Size_Per_Unit,
      Effect_Size_Per_Unit_CI,
      T_Value,
      Anomaly_Size,
      Effect_Size_Anomaly,
      Effect_Size_Anomaly_CI,
      Percent_Contribution,
      Average_National_Exposure,
      Pop_Weight_Effect_Size_Anomaly,
      Pop_Weight_Effect_Size_Anomaly_CI,
      Pop_Weight_Percent_Contribution
    )
  
  write.csv(final_table, paste0("TableS4_",model_name, "_Table1", ".csv"), row.names = FALSE)
  
  return(final_table)
}

#### 
#' Table 1 Formatter (SENSITIVITY) ---
#' 
#' 
####

format_table1 <- function(CHD_table, stroke_table) {
  formatted_CHD_table <- CHD_table[, c("Variable", "Average_National_Exposure", "Effect_Size_Per_Unit", "Effect_Size_Per_Unit_CI", "Pop_Weight_Effect_Size_Anomaly", "Pop_Weight_Effect_Size_Anomaly_CI", "Pop_Weight_Percent_Contribution")]
  names(formatted_CHD_table) <- c("Anomaly_Metric", "CHD_Anomaly_Size", "CHD_Effect_Size_Per_Unit_Change","CHD_Effect_Size_Per_Unit_Change_CI", "CHD_Anomaly_Effect_Size", "CHD_Anomaly_Effect_Size_CI", "CHD_Relative_Prevalence")
  formatted_stroke_table <- stroke_table[, c("Average_National_Exposure", "Effect_Size_Per_Unit", "Effect_Size_Per_Unit_CI", "Pop_Weight_Effect_Size_Anomaly", "Pop_Weight_Effect_Size_Anomaly_CI", "Pop_Weight_Percent_Contribution")]
  names(formatted_stroke_table) <- c("Stroke_Anomaly_Size", "Stroke_Effect_Size_Per_Unit_Change","Stroke_Effect_Size_Per_Unit_Change_CI", "Stroke_Anomaly_Effect_Size", "Stroke_Anomaly_Effect_Size_CI", "Stroke_Relative_Prevalence")
  formatted_table <- cbind(formatted_CHD_table, formatted_stroke_table)
  return(formatted_table)
}

model_9e_CHD_table1 <- create_results_table(all_results_sens$model_9e_CHD_CrudePrev$model, "model_9e_CHD", sensitivity70_89_03_22_merged, avg_CHD, "70_89_03_22")
model_9e_STROKE_table1 <- create_results_table(all_results_sens$model_9e_STROKE_CrudePrev$model, "model_9e_Stroke", sensitivity70_89_03_22_merged, avg_stroke, "70_89_03_22")
formatted_table1_model_9e <- format_table1 (model_9e_CHD_table1, model_9e_STROKE_table1)

model_old_sens_CHD_table1 <- create_results_table(all_results_sens$model_old_sens_CHD_CrudePrev$model, "model_old_sens_CHD", sensitivity70_89_03_22_merged, avg_CHD, "70_89_03_22")
model_old_sens_STROKE_table1 <- create_results_table(all_results_sens$model_old_sens_STROKE_CrudePrev$model, "model_old_sens_Stroke", sensitivity70_89_03_22_merged, avg_stroke, "70_89_03_22")
formatted_table1_model_old_sens <- format_table1 (model_old_sens_CHD_table1, model_old_sens_STROKE_table1)

model_HI_CHD_table1 <- create_results_table(all_results_sens$model_HI_CHD_CrudePrev$model, "model_HI_CHD", sensitivity70_89_03_22_merged, avg_CHD, "70_89_03_22")
model_HI_STROKE_table1 <- create_results_table(all_results_sens$model_HI_STROKE_CrudePrev$model, "model_HI_Stroke", sensitivity70_89_03_22_merged, avg_stroke, "70_89_03_22")
formatted_table1_model_HI <- format_table1 (model_HI_CHD_table1, model_HI_STROKE_table1)

write.csv(formatted_table1_model_9e,"TableS4_model_9e_formatted.csv", row.names = FALSE)
write.csv(all_results_sens$model_9e_CHD_CrudePrev$results,"TableS4_model_9e_modelfit_CHD.csv", row.names = FALSE)
write.csv(all_results_sens$model_9e_STROKE_CrudePrev$results,"TableS4_model_9e_modelfit_STROKE.csv", row.names = FALSE)

################################################################################
# FIGURE 2 GENERATOR (SENSITIVITY) --
# This script performs mixed-effects modeling for CHD and Stroke prevalence
# against various climate variables and generates Figure 2 outputs
#
#
################################################################################

# Initialize empty lists to store model results
coefficients_chd <- list()
se_chd <- list()
lower_bound_chd <- list()
upper_bound_chd <- list()

coefficients_stroke <- list()
se_stroke <- list()
lower_bound_stroke <- list()
upper_bound_stroke <- list()

# Calculate average prevalence values
avg_CHD <- mean(sensitivity70_89_03_22_merged$CHD_CrudePrev, na.rm = TRUE)
avg_STROKE <- mean(sensitivity70_89_03_22_merged$STROKE_CrudePrev, na.rm = TRUE)

# Calculate average anomaly sizes for climate variables
climate_vars <- c(
  "HI_C_Change70_89_03_22", "Change_Temp_C_70_89_03_22", "Change_RH_70_89_03_22",
  "windU_diff70_89_03_22", "windV_diff70_89_03_22",
  "solar_diff70_89_03_22_scale", "evap_diff70_89_03_22_P_Ten",
  "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
  "transpir_diff70_89_03_22_scale", "downwards_solar_diff70_89_03_22_P_Mill"
)

avg_values <- sapply(climate_vars, function(var) mean(sensitivity70_89_03_22_merged[[var]], na.rm = TRUE))

# Define common control variables for models
control_vars <- c(
  "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
  "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
  "Temp_C_2013_2022.x", "LCchangeMEAN"
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
  results <- run_model("CHD_CrudePrev", var, sensitivity70_89_03_22_merged, avg_values[simple_name])
  
  coefficients_chd[[paste0("chd_", simple_name)]] <- results$estimate
  se_chd[[paste0("chd_", simple_name)]] <- results$se
  lower_bound_chd[[paste0("chd_", simple_name)]] <- results$lower
  upper_bound_chd[[paste0("chd_", simple_name)]] <- results$upper
}

# Run models for Stroke
for (var in climate_vars) {
  simple_name <- var
  results <- run_model("STROKE_CrudePrev", var, sensitivity70_89_03_22_merged, avg_values[simple_name])
  
  coefficients_stroke[[paste0("stroke_", simple_name)]] <- results$estimate
  se_stroke[[paste0("stroke_", simple_name)]] <- results$se
  lower_bound_stroke[[paste0("stroke_", simple_name)]] <- results$lower
  upper_bound_stroke[[paste0("stroke_", simple_name)]] <- results$upper
}

# Create results data frames
variable_names <- c(
  "Heat Index", "Air Temperature", "Humidity", "Eastward Wind", "Northward Wind",
  "Surface-Absorbed Sunlight", "Evaporation", "Surface Pressure", "Precipitation",
  "Transpiration", "Sunlight"
)

chd_fig2 <- data.frame(
  Variable = variable_names,
  SE = unlist(se_chd),
  Effect_Anomaly_Size = unlist(coefficients_chd),
  Lower_CI = unlist(lower_bound_chd),
  Upper_CI = unlist(upper_bound_chd),
  stringsAsFactors = FALSE
)

stroke_fig2 <- data.frame(
  Variable = variable_names,
  SE = unlist(se_stroke),
  Effect_Anomaly_Size = unlist(coefficients_stroke),
  Lower_CI = unlist(lower_bound_stroke),
  Upper_CI = unlist(upper_bound_stroke),
  stringsAsFactors = FALSE
)

# Format confidence intervals
chd_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", chd_fig2$Lower_CI, chd_fig2$Upper_CI)
stroke_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", stroke_fig2$Lower_CI, stroke_fig2$Upper_CI)

# Combine results for output
combined_results <- cbind(
  chd_fig2$Variable,
  chd_fig2$Effect_Anomaly_Size,
  chd_fig2$Combined_CI,
  stroke_fig2$Effect_Anomaly_Size,
  stroke_fig2$Combined_CI
)

write.csv(combined_results,"TableS4_figure2_verify.csv", row.names = FALSE)