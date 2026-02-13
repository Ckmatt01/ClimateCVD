###############################################################################################
# MODEL GENERATOR ----
#
# This script runs mixed-effects models for CHD and stroke prevalence analysis,
# generates results tables, and performs sensitivity analysis on anomaly effects.
###############################################################################################

setwd(working_directory_output)

#### Generate character vectors of model inputs

hardcoded_vars <- c("OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
                    "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1", 
                    "Temp_C_2013_2022", "LCchangeMEAN")

dynamic_vars <- results_list[[1]][[2]]
model_9a <- c(dynamic_vars[1], hardcoded_vars, dynamic_vars[-1])

dynamic_vars <- results_list[[2]][[2]]
model_9b <- c(dynamic_vars[1], hardcoded_vars, dynamic_vars[-1])

dynamic_vars <- results_list[[3]][[2]]
model_9c <- c(dynamic_vars[1], hardcoded_vars, dynamic_vars[-1])

dynamic_vars <- results_list[[4]][[2]]
model_9d <- c(dynamic_vars[1], hardcoded_vars, dynamic_vars[-1])

# Original strings
original <- c("latent_diff70_79_13_22", "solar_diff70_79_13_22", "thermal_diff70_79_13_22", 
              "sensible_diff70_79_13_22", "downwards_solar_diff70_79_13_22", 
              "pressure_diff70_79_13_22", "evap_diff70_79_13_22", 
              "precip_diff70_79_13_22", "transpir_diff70_79_13_22")

# Scaled strings
replacement <- c("latent_diff70_79_13_22_P_Mill", "solar_diff70_79_13_22_P_Mill", 
                 "thermal_diff70_79_13_22_P_Mill", "sensible_diff70_79_13_22_P_Mill", 
                 "downwards_solar_diff70_79_13_22_P_Mill", "pressure_diff70_79_13_22_P_div10", 
                 "evap_diff70_79_13_22_P_x10", "precip_diff70_79_13_22_P_x10", 
                 "transpir_diff70_79_13_22_P_x1000")

#### ,"latent_diff70_89_03_22", "solar_diff70_89_03_22", "thermal_diff70_89_03_22", "sensible_diff70_89_03_22", "pressure_diff70_89_03_22", "precip_diff70_89_03_22", "transpir_diff70_89_03_22", "downwards_solar_diff70_89_03_22"
#' Function to replace unscaled variables with scaled variables in a character vector ---
#' 
#' 
#### "pressure_diff70_89_03_22_scale", "solar_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale", "transpir_diff70_89_03_22_scale", "downwards_solar_diff70_89_03_22_P_Mill"
scaled_strings <- function(char_vector) {
  # Create a named vector for replacement
  replacement_map <- setNames(replacement, original)
  
  # Replace matches in the input vector
  replaced <- ifelse(char_vector %in% original, 
                     replacement_map[char_vector], 
                     char_vector)
  
  # Return the result (unname if you don't want names)
  return(unname(replaced))
}

scaled_model_9a <- scaled_strings(model_9a)
scaled_model_9b <- scaled_strings(model_9b)
scaled_model_9c <- scaled_strings(model_9c)
scaled_model_9d <- scaled_strings(model_9d)
scaled_model_old <- c(
  "HI_C_Change70_79_13_22",
  "Temp_C_2013_2022", 
  "PCT_ImperviousSurfaces",
  "SPL_THEME1",
  "EP_AGE65", 
  "CSMOKING_CrudePrev", 
  "CHECKUP_CrudePrev",
  "OBESITY_CrudePrev_P_div10", 
  "windU_diff70_79_13_22", 
  "windV_diff70_79_13_22", 
  "solar_diff70_79_13_22_P_Mill",
  "pressure_diff70_79_13_22_P_div10",
  "precip_diff70_79_13_22_P_x10", 
  "transpir_diff70_79_13_22_P_x1000", 
  "LCchangeMEAN"
)

# Define each model's variables as a named list
model_dependents <- c("CHD_CrudePrev", "STROKE_CrudePrev")
model_dependents_formula <- c("CHD_CrudePrev ~", "STROKE_CrudePrev ~")
model_dependents_name <- c("CHD", "stroke")

model_specifications <- list(
  model_9a = scaled_model_9a,
  model_9b = scaled_model_9b,
  model_9c = scaled_model_9c,
  model_9d = scaled_model_9d,
  model_old = scaled_model_old
)

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
    lmer(formula_obj, data = subset_data)
  }, error = function(e) {
    message(paste("Error in model", model_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(model)) {
    return(NULL)
  }
  
  # Store model in global environment
  assign(model_name, model, envir = .GlobalEnv) ## CAN POTENTIALLY REMOVE
  
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
    if (var %in% names(subset_data)) {
      avg_var <- mean(subset_data[[var]], na.rm = TRUE) # Ex: mean(subset_data[["HI_C_Change70_79_13_22"]] na.arm = TRUE)
      avg_dependent <- mean(subset_data[[dependent_var]], na.rm = TRUE) # Ex: mean(subset_data[[CHD_CrudePrev]])
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
  numeric_cols <- c("Effect_Size", "T_Value", "Percent_Contribution", "R2m", "R2c", "AIC", "BIC", "RMSE")
  results[numeric_cols] <- lapply(results[numeric_cols], function(x) round(x, 2))
  
  # Save results to CSV
  write.csv(results, paste0("TableS5_fitstats_", model_name, "_results.csv"), row.names = FALSE)
  
  return(list(model = model, results = results))
}

# Run all models and store results
all_results <- list()
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
      all_results[[paste0(model_name, "_", dependent_var)]] <- model_result
      # Print summary of the model
      print(summary(model_result$model))
    }
  }
}

################################################################################
# FIGURE 2 GENERATOR --
# This script performs mixed-effects modeling for CHD and Stroke prevalence
# against various climate variables and generates Figure 2 outputs
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
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)
avg_STROKE <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

# Calculate average anomaly sizes for climate variables
climate_vars <- c(
  "HI_C_Change70_79_13_22", "Change_Temp_C_70_79_13_22", "Change_RH_70_79_13_22",
  "windU_diff70_79_13_22", "windV_diff70_79_13_22", "latent_diff70_79_13_22_P_Mill",
  "solar_diff70_79_13_22_P_Mill", "thermal_diff70_79_13_22_P_Mill",
  "sensible_diff70_79_13_22_P_Mill", "evap_diff70_79_13_22_P_x10",
  "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
  "transpir_diff70_79_13_22_P_x1000", "downwards_solar_diff70_79_13_22_P_Mill"
)

avg_values <- sapply(climate_vars, function(var) mean(subset_data[[var]], na.rm = TRUE))

# Define common control variables for models
control_vars <- c(
  "OBESITY_CrudePrev_P_div10", "EP_AGE65", "PCT_ImperviousSurfaces",
  "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "SPL_THEME1",
  "Temp_C_2013_2022", "LCchangeMEAN"
)

# Function to run mixed-effects model and extract results for Figure 2
run_model_figure2 <- function(dependent_var, independent_var, data, avg_value) {
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

#number of rows for figure 2
nrow(subset_data)

# Run models for CHD
for (var in climate_vars) {
  simple_name <- var
  results <- run_model_figure2("CHD_CrudePrev", var, subset_data, avg_values[simple_name])
  
  coefficients_chd[[paste0("chd_", simple_name)]] <- results$estimate
  se_chd[[paste0("chd_", simple_name)]] <- results$se
  lower_bound_chd[[paste0("chd_", simple_name)]] <- results$lower
  upper_bound_chd[[paste0("chd_", simple_name)]] <- results$upper
}

# Run models for Stroke
for (var in climate_vars) {
  simple_name <- var
  results <- run_model_figure2("STROKE_CrudePrev", var, subset_data, avg_values[simple_name])
  
  coefficients_stroke[[paste0("stroke_", simple_name)]] <- results$estimate
  se_stroke[[paste0("stroke_", simple_name)]] <- results$se
  lower_bound_stroke[[paste0("stroke_", simple_name)]] <- results$lower
  upper_bound_stroke[[paste0("stroke_", simple_name)]] <- results$upper
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

write.csv(combined_results, "TableS5_Figure2.csv", row.names = FALSE)

# Prepare data for forest plot
combined_fig2 <- rbind(
  cbind(chd_fig2, Outcome = "CHD"),
  cbind(stroke_fig2, Outcome = "Stroke")
)

# Define variable ordering
custom_order <- c(
  "Heat Index", "Air Temperature", "Humidity", "Surface-Absorbed Sunlight",
  "Transpiration", "Eastward Wind", "Sunlight", "Sensible Heat Flux",
  "Precipitation", "Northward Wind", "Thermal Radiation", "Latent Heat Flux",
  "Evaporation", "Surface Pressure"
)

combined_fig2$Variable_Group <- factor(combined_fig2$Variable, levels = rev(custom_order))
combined_fig2$Outcome <- factor(combined_fig2$Outcome, levels = c("Stroke", "CHD"))

# Sort data for plotting
combined_fig2 <- combined_fig2[order(combined_fig2$Variable_Group, combined_fig2$Outcome), ]
combined_fig2$Variable_Outcome <- paste(combined_fig2$Variable, combined_fig2$Outcome, sep = " - ")
combined_fig2$Variable_Outcome <- factor(combined_fig2$Variable_Outcome, levels = unique(combined_fig2$Variable_Outcome))

# Save data for figure
write.csv(combined_fig2, "Figure2_Raw_Verify.csv")

# Create forest plot
fig2 <- ggplot(combined_fig2, aes(x = Effect_Anomaly_Size, y = Variable_Outcome, 
                                  color = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = seq(-2, 2, by = 0.5), linetype = "dashed", color = "gray") +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.7) +
  scale_x_continuous(limits = c(min(combined_fig2$Lower_CI), max(combined_fig2$Upper_CI))) +
  labs(x = "Prevalence") +
  scale_color_manual(values = c("CHD" = "black", "Stroke" = "red"), 
                     guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c("CHD" = 16, "Stroke" = 17), 
                     guide = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10, b = 10),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave("Figure2.png", plot = fig2, width = 3.6, height = 5, dpi = 600)

################################################################################
# CUSTOM MODEL 9C VARIANTS WITH DIFFERENT RANDOM EFFECTS ----
# This section creates model 9c variants with State.x, CLIMDIV, and CensusDivision random effects
################################################################################

# Function to run custom model with different random effects
run_custom_model <- function(model_name, predictors, dependent_var, dependent_formula, random_effect_var) {
  formula_str <- paste(dependent_formula, paste(predictors, collapse = " + "), 
                       paste("+ (1 |", random_effect_var, ")"))
  formula_obj <- as.formula(formula_str)
  model <- tryCatch({
    lmer(formula_obj, data = subset_data)
  }, error = function(e) {
    message(paste("Error in model", model_name, ":", e$message))
    return(NULL)
  })
  if (is.null(model)) return(NULL)
  assign(model_name, model, envir = .GlobalEnv)
  model_summary <- summary(model)
  model_r2 <- r.squaredGLMM(model)
  model_aic <- AIC(model)
  model_bic <- BIC(model)
  model_rmse <- rmse(model)
  model_results_list <- list()
  for (var in predictors) {
    if (!var %in% rownames(model_summary$coefficients)) {
      warning(paste("Variable", var, "not found in model coefficients for", model_name))
      next
    }
    coefficients <- model_summary$coefficients[, "Estimate"]
    se <- model_summary$coefficients[, "Std. Error"]
    tvalue <- model_summary$coefficients[, "t value"]
    lower_bound <- coefficients - 1.96 * se
    upper_bound <- coefficients + 1.96 * se
    if (var %in% names(subset_data)) {
      avg_var <- mean(subset_data[[var]], na.rm = TRUE)
      avg_dependent <- mean(subset_data[[dependent_var]], na.rm = TRUE)
      pct_contribution <- avg_var * coefficients[var] / avg_dependent * 100
    } else {
      pct_contribution <- NA
    }
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
  results <- do.call(rbind, model_results_list)
  numeric_cols <- c("Effect_Size", "T_Value", "Percent_Contribution", "R2m", "R2c", "AIC", "BIC", "RMSE")
  results[numeric_cols] <- lapply(results[numeric_cols], function(x) round(x, 2))
  write.csv(results, paste0("TableS5_fitstats_", model_name, "_results.csv"), row.names = FALSE)
  return(list(model = model, results = results))
}

# Function to run custom model with multiple random effects
run_custom_model_multi <- function(model_name, predictors, dependent_var, dependent_formula, random_effect_vars) {
  random_terms <- paste0("(1 | ", random_effect_vars, ")", collapse = " + ")
  formula_str <- paste(dependent_formula, paste(predictors, collapse = " + "), 
                       paste("+", random_terms))
  formula_obj <- as.formula(formula_str)
  model <- tryCatch({
    lmer(formula_obj, data = subset_data)
  }, error = function(e) {
    message(paste("Error in model", model_name, ":", e$message))
    return(NULL)
  })
  if (is.null(model)) return(NULL)
  assign(model_name, model, envir = .GlobalEnv)
  model_summary <- summary(model)
  model_r2 <- r.squaredGLMM(model)
  model_aic <- AIC(model)
  model_bic <- BIC(model)
  model_rmse <- rmse(model)
  model_results_list <- list()
  for (var in predictors) {
    if (!var %in% rownames(model_summary$coefficients)) {
      warning(paste("Variable", var, "not found in model coefficients for", model_name))
      next
    }
    coefficients <- model_summary$coefficients[, "Estimate"]
    se <- model_summary$coefficients[, "Std. Error"]
    tvalue <- model_summary$coefficients[, "t value"]
    lower_bound <- coefficients - 1.96 * se
    upper_bound <- coefficients + 1.96 * se
    if (var %in% names(subset_data)) {
      avg_var <- mean(subset_data[[var]], na.rm = TRUE)
      avg_dependent <- mean(subset_data[[dependent_var]], na.rm = TRUE)
      pct_contribution <- avg_var * coefficients[var] / avg_dependent * 100
    } else {
      pct_contribution <- NA
    }
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
  results <- do.call(rbind, model_results_list)
  numeric_cols <- c("Effect_Size", "T_Value", "Percent_Contribution", "R2m", "R2c", "AIC", "BIC", "RMSE")
  results[numeric_cols] <- lapply(results[numeric_cols], function(x) round(x, 2))
  write.csv(results, paste0("TableS5_fitstats_", model_name, "_results.csv"), row.names = FALSE)
  return(list(model = model, results = results))
}

cat("\nRunning Model 9c variants with different random effects...\n")

# Model 9c CHD
model_9c_CHD_State_result <- run_custom_model(
  model_name = "model_9c_CHD_CrudePrev_State",
  predictors = scaled_model_9c,
  dependent_var = "CHD_CrudePrev",
  dependent_formula = "CHD_CrudePrev ~",
  random_effect_var = "STATE.x"
)
model_9c_CHD_CLIMDIV_result <- run_custom_model(
  model_name = "model_9c_CHD_CrudePrev_CLIMDIV",
  predictors = scaled_model_9c,
  dependent_var = "CHD_CrudePrev",
  dependent_formula = "CHD_CrudePrev ~",
  random_effect_var = "CLIMDIV"
)
model_9c_CHD_CensusDivision_result <- run_custom_model(
  model_name = "model_9c_CHD_CrudePrev_CensusDivision",
  predictors = scaled_model_9c,
  dependent_var = "CHD_CrudePrev",
  dependent_formula = "CHD_CrudePrev ~",
  random_effect_var = "CensusDivision"
)

# Model 9c Stroke
model_9c_STROKE_State_result <- run_custom_model(
  model_name = "model_9c_STROKE_CrudePrev_State",
  predictors = scaled_model_9c,
  dependent_var = "STROKE_CrudePrev",
  dependent_formula = "STROKE_CrudePrev ~",
  random_effect_var = "STATE.x"
)
model_9c_STROKE_CLIMDIV_result <- run_custom_model(
  model_name = "model_9c_STROKE_CrudePrev_CLIMDIV",
  predictors = scaled_model_9c,
  dependent_var = "STROKE_CrudePrev",
  dependent_formula = "STROKE_CrudePrev ~",
  random_effect_var = "CLIMDIV"
)
model_9c_STROKE_CensusDivision_result <- run_custom_model(
  model_name = "model_9c_STROKE_CrudePrev_CensusDivision",
  predictors = scaled_model_9c,
  dependent_var = "STROKE_CrudePrev",
  dependent_formula = "STROKE_CrudePrev ~",
  random_effect_var = "CensusDivision"
)

# Model 9c with CountyFIPS + CensusDivision random effects
model_9c_CHD_County_CensusDivision_result <- run_custom_model_multi(
  model_name = "model_9c_CHD_CrudePrev_County_CensusDivision",
  predictors = scaled_model_9c,
  dependent_var = "CHD_CrudePrev",
  dependent_formula = "CHD_CrudePrev ~",
  random_effect_vars = c("CountyFIPS", "CensusDivision")
)
model_9c_STROKE_County_CensusDivision_result <- run_custom_model_multi(
  model_name = "model_9c_STROKE_CrudePrev_County_CensusDivision",
  predictors = scaled_model_9c,
  dependent_var = "STROKE_CrudePrev",
  dependent_formula = "STROKE_CrudePrev ~",
  random_effect_vars = c("CountyFIPS", "CensusDivision")
)

model_9c_stroke_temp_70_79 <- run_custom_model(
  model_name = "model_9c_STROKE_CrudePrev_Temp70_79",
  predictors = scaled_model_9c,
  dependent_var = "STROKE_CrudePrev",
  dependent_formula = "STROKE_CrudePrev ~",
  random_effect_var = "CLIMDIV"
)

cat("\nCompleted Model 9c variants with different random effects.\n")

################################################################################
# COMPARISON TABLE FOR MODEL 9C VARIANTS ----
################################################################################

extract_model_metrics <- function(model_result, model_label) {
  if (is.null(model_result) || is.null(model_result$model)) {
    return(data.frame(
      Model = model_label,
      Effect_size_per_unit_change = NA,
      CI = NA,
      T_Value = NA,
      Percent_HI_contribution = NA,
      R2 = NA,
      RMSE = NA,
      AIC = NA,
      BIC = NA
    ))
  }
  model <- model_result$model
  model_summary <- summary(model)
  hi_var <- "HI_C_Change70_79_13_22"
  if (hi_var %in% rownames(model_summary$coefficients)) {
    coefficients <- model_summary$coefficients[, "Estimate"]
    se <- model_summary$coefficients[, "Std. Error"]
    tvalue <- model_summary$coefficients[, "t value"]
    hi_coef <- coefficients[hi_var]
    hi_se <- se[hi_var]
    hi_tvalue <- tvalue[hi_var]
    lower_ci <- hi_coef - 1.96 * hi_se
    upper_ci <- hi_coef + 1.96 * hi_se
    ci_string <- paste0("(", round(lower_ci, 2), ", ", round(upper_ci, 2), ")")
    avg_hi <- mean(subset_data[[hi_var]], na.rm = TRUE)
    avg_dependent <- ifelse(grepl("CHD", model_result$results$Dependent[1]), 
                            mean(subset_data$CHD_CrudePrev, na.rm = TRUE),
                            mean(subset_data$STROKE_CrudePrev, na.rm = TRUE))
    pct_contribution <- avg_hi * hi_coef / avg_dependent * 100
  } else {
    hi_coef <- NA
    ci_string <- NA
    hi_tvalue <- NA
    pct_contribution <- NA
  }
  model_r2 <- r.squaredGLMM(model)
  model_aic <- AIC(model)
  model_bic <- BIC(model)
  model_rmse <- rmse(model)
  return(data.frame(
    Model = model_label,
    Effect_size_per_unit_change = round(hi_coef, 2),
    CI = ci_string,
    T_Value = round(hi_tvalue, 2),
    Percent_HI_contribution = round(pct_contribution, 2),
    R2 = round(model_r2[2], 2),
    RMSE = round(model_rmse, 2),
    AIC = round(model_aic, 1),
    BIC = round(model_bic, 1)
  ))
}

cat("\n=== CHD Model 9c Variants Comparison ===\n")
chd_county <- extract_model_metrics(all_results[["model_9c_CHD_CrudePrev"]], "County FIPS")
chd_state <- extract_model_metrics(model_9c_CHD_State_result, "State")
chd_climdiv <- extract_model_metrics(model_9c_CHD_CLIMDIV_result, "CLIMDIV")
chd_censusdivision <- extract_model_metrics(model_9c_CHD_CensusDivision_result, "CensusDivision")
chd_comparison <- rbind(chd_county, chd_state, chd_climdiv, chd_censusdivision)
chd_comparison$Random_Effect <- c("County FIPS", "State", "CLIMDIV", "CensusDivision")
print("CHD Model 9c Variants:")
print(chd_comparison)

cat("\n=== Stroke Model 9c Variants Comparison ===\n")
stroke_county <- extract_model_metrics(all_results[["model_9c_STROKE_CrudePrev"]], "County FIPS")
stroke_state <- extract_model_metrics(model_9c_STROKE_State_result, "State")
stroke_climdiv <- extract_model_metrics(model_9c_STROKE_CLIMDIV_result, "CLIMDIV")
stroke_censusdivision <- extract_model_metrics(model_9c_STROKE_CensusDivision_result, "CensusDivision")
stroke_comparison <- rbind(stroke_county, stroke_state, stroke_climdiv, stroke_censusdivision)
stroke_comparison$Random_Effect <- c("County FIPS", "State", "CLIMDIV", "CensusDivision")
print("Stroke Model 9c Variants:")
print(stroke_comparison)

cat("\n=== Model 9c County + CensusDivision Random Effects ===\n")
chd_county_censusdivision <- extract_model_metrics(model_9c_CHD_County_CensusDivision_result, "County + CensusDivision")
stroke_county_censusdivision <- extract_model_metrics(model_9c_STROKE_County_CensusDivision_result, "County + CensusDivision")
county_census_comparison <- rbind(chd_county_censusdivision, stroke_county_censusdivision)
county_census_comparison$Outcome <- c("CHD", "Stroke")
print("Model 9c County + CensusDivision:")
print(county_census_comparison)
write.csv(county_census_comparison, "Model_9c_County_CensusDivision_Comparison.csv", row.names = FALSE)

# Detailed summary metrics for County + CensusDivision models
extract_summary_metrics <- function(model_obj, model_label, dependent_var) {
  model_summary <- summary(model_obj)
  hi_var <- "HI_C_Change70_79_13_22"
  if (!hi_var %in% rownames(model_summary$coefficients)) {
    return(data.frame(
      Model = model_label,
      Effect_size_per_unit_change_CI = NA,
      T_Value = NA,
      Percent_HI_contribution = NA,
      R2 = NA,
      RMSE = NA,
      AIC = NA,
      BIC = NA,
      stringsAsFactors = FALSE
    ))
  }
  est <- model_summary$coefficients[hi_var, "Estimate"]
  se <- model_summary$coefficients[hi_var, "Std. Error"]
  tvalue <- model_summary$coefficients[hi_var, "t value"]
  lower_ci <- est - 1.96 * se
  upper_ci <- est + 1.96 * se
  avg_hi <- mean(subset_data[[hi_var]], na.rm = TRUE)
  avg_dependent <- mean(subset_data[[dependent_var]], na.rm = TRUE)
  pct_contribution <- avg_hi * est / avg_dependent * 100
  model_r2 <- r.squaredGLMM(model_obj)
  data.frame(
    Model = model_label,
    Effect_size_per_unit_change_CI = paste0(round(est, 2), " (", round(lower_ci, 2), ", ", round(upper_ci, 2), ")"),
    T_Value = round(tvalue, 2),
    Percent_HI_contribution = round(pct_contribution, 2),
    R2 = round(model_r2[2], 2),
    RMSE = round(rmse(model_obj), 2),
    AIC = round(AIC(model_obj), 2),
    BIC = round(BIC(model_obj), 2),
    stringsAsFactors = FALSE
  )
}

chd_metrics <- extract_summary_metrics(
  model_9c_CHD_County_CensusDivision_result$model,
  "CHD County + CensusDivision",
  "CHD_CrudePrev"
)
stroke_metrics <- extract_summary_metrics(
  model_9c_STROKE_County_CensusDivision_result$model,
  "Stroke County + CensusDivision",
  "STROKE_CrudePrev"
)
summary_table <- rbind(chd_metrics, stroke_metrics)
print(summary_table)
write.csv(summary_table, "Model_9c_Summary_Metrics.csv", row.names = FALSE)

write.csv(chd_comparison, "Model_9c_CHD_RandomEffects_Comparison.csv", row.names = FALSE)
write.csv(stroke_comparison, "Model_9c_Stroke_RandomEffects_Comparison.csv", row.names = FALSE)

cat("\n=== Combined Model 9c Variants Comparison ===\n")
chd_comparison$Outcome <- "CHD"
stroke_comparison$Outcome <- "Stroke"
combined_comparison <- rbind(chd_comparison, stroke_comparison)
combined_comparison <- combined_comparison[, c("Outcome", "Random_Effect", "Effect_size_per_unit_change", 
                                               "CI", "T_Value", "Percent_HI_contribution", 
                                               "R2", "RMSE", "AIC", "BIC")]
print("Combined Model 9c Variants:")
print(combined_comparison)
write.csv(combined_comparison, "Model_9c_Combined_RandomEffects_Comparison.csv", row.names = FALSE)
cat("\nComparison tables saved to CSV files.\n")

# CHD: Only Heat Index + CensusDivision random effect
model_CHD_HI_CensusDivision <- lmer(
  CHD_CrudePrev ~ HI_C_Change70_79_13_22 + (1 | CensusDivision),
  data = subset_data
)
summary(model_CHD_HI_CensusDivision)

# Stroke: Only Heat Index + CensusDivision random effect
model_STROKE_HI_CensusDivision <- lmer(
  STROKE_CrudePrev ~ HI_C_Change70_79_13_22 + (1 | CensusDivision),
  data = subset_data
)
summary(model_STROKE_HI_CensusDivision)

#output both summaries as a csv

chd_hi_cd_summary <- summary(model_CHD_HI_CensusDivision)
stroke_hi_cd_summary <- summary(model_STROKE_HI_CensusDivision)
capture.output(chd_hi_cd_summary, file = "CHD_HI_CensusDivision_Summary.txt")
capture.output(stroke_hi_cd_summary, file = "Stroke_HI_CensusDivision_Summary.txt")
cat("\nSummaries for CHD and Stroke HI + CensusDivision models saved to text files.\n")
################################################################################
# END OF SCRIPT ----

# After fitting your model:
predictors <- model_specifications$model_9c
predictors <- c("Temp_C_70_79", "HI_C_Change70_79_13_22")
formula_str <- paste("STROKE_CrudePrev ~", paste(predictors, collapse = " + "), "+ (1 | CountyFIPS)")
model <- lmer(as.formula(formula_str), data = subset_data)
model_summary <- summary(model)

# Extract metrics for HI_C_Change70_79_13_22
hi_var <- "HI_C_Change70_79_13_22"
if (hi_var %in% rownames(model_summary$coefficients)) {
  coefficients <- model_summary$coefficients[, "Estimate"]
  se <- model_summary$coefficients[, "Std. Error"]
  tvalue <- model_summary$coefficients[, "t value"]
  hi_coef <- coefficients[hi_var]
  hi_se <- se[hi_var]
  hi_tvalue <- tvalue[hi_var]
  lower_ci <- hi_coef - 1.96 * hi_se
  upper_ci <- hi_coef + 1.96 * hi_se
  ci_string <- paste0("(", round(lower_ci, 2), ", ", round(upper_ci, 2), ")")
  avg_hi <- mean(subset_data[[hi_var]], na.rm = TRUE)
  avg_dependent <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)
  pct_contribution <- avg_hi * hi_coef / avg_dependent * 100
} else {
  hi_coef <- NA
  ci_string <- NA
  hi_tvalue <- NA
  pct_contribution <- NA
}
model_r2 <- r.squaredGLMM(model)
model_aic <- AIC(model)
model_bic <- BIC(model)
model_rmse <- rmse(model)

# Create a data frame with the results
result <- data.frame(
  Model = "model_9c_CHD_CrudePrev",
  Effect_size_per_unit_change = round(hi_coef, 2),
  CI = ci_string,
  T_Value = round(hi_tvalue, 2),
  Percent_HI_contribution = round(pct_contribution, 2),
  R2 = round(model_r2[2], 2),
  RMSE = round(model_rmse, 2),
  AIC = round(model_aic, 1),
  BIC = round(model_bic, 1)
)
print(result)