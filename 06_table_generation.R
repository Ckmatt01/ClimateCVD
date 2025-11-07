###############################################################################################
# TABLE 1 GENERATOR ----
#
# This script generates table 1
###############################################################################################

setwd(working_directory_output)

#### Find mean crude prevalence of CHD and stroke ####
avg_stroke <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)

####
#' Create results table for a given model
#' @param model Model object (e.x. model_9a_CHD_CrudePrev)
#' @param model_name Name for the model (used in output files) (e.x. "model_9a_CHD_table")
#' @param data Dataset containing all variables (e.x. subset_data)
#' @param avg_model Average value of the dependent variable (e.x. avg_CHD)
#' @param time_period Time period label for output files (e.x. 70_79_13_22)
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
    HI = list(name = "HI_C_Change70_79_13_22", label = "Heat Index Anomaly"),
    temp = list(name = "Change_Temp_C_70_79_13_22", label = "Air Temperature Anomaly"),
    humidity = list(name = "Change_RH_70_79_13_22", label = "Humidity Anomaly"),
    windU = list(name = "windU_diff70_79_13_22", label = "Eastward Wind Anomaly"),
    windV = list(name = "windV_diff70_79_13_22", label = "Northward Wind Anomaly"),
    latent = list(name = "latent_diff70_79_13_22_P_Mill", label = "Latent Heat Flux Anomaly"),
    solar = list(name = "solar_diff70_79_13_22_P_Mill", label = "Absorbed Sunlight Anomaly"),
    thermal = list(name = "thermal_diff70_79_13_22_P_Mill", label = "Thermal Radiation Anomaly"),
    sensible = list(name = "sensible_diff70_79_13_22_P_Mill", label = "Sensible Heat Flux Anomaly"),
    evaporation = list(name = "evap_diff70_79_13_22_P_x10", label = "Evaporation Anomaly"),
    pressure = list(name = "pressure_diff70_79_13_22_P_div10", label = "Surface Pressure Anomaly"),
    precipitation = list(name = "precip_diff70_79_13_22_P_x10", label = "Precipitation Anomaly"),
    transpir = list(name = "transpir_diff70_79_13_22_P_x1000", label = "Transpiration Anomaly"),
    sunlight = list(name = "downwards_solar_diff70_79_13_22_P_Mill", label = "Sunlight Anomaly")
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
  
  write.csv(final_table, paste0("TableS5_",model_name, "_Table1", ".csv"), row.names = FALSE)
  
  return(final_table)
}

# Use function to generate models for, 9a (0.8), 9b (0.6), 9c (0.4 -> our current model), and 9d (0.2)
model_9a_CHD_table1 <- create_results_table(model_9a_CHD_CrudePrev, "model_9a_CHD_table", subset_data, avg_CHD, "70_79_13_22")
model_9b_CHD_table1 <- create_results_table(model_9b_CHD_CrudePrev, "model_9b_CHD_table", subset_data, avg_CHD, "70_79_13_22")
model_9c_CHD_table1 <- create_results_table(model_9c_CHD_CrudePrev, "model_9c_CHD_table", subset_data, avg_CHD, "70_79_13_22")
model_9d_CHD_table1 <- create_results_table(model_9d_CHD_CrudePrev, "model_9d_CHD_table", subset_data, avg_CHD, "70_79_13_22")
model_old_CHD_table1 <- create_results_table(model_old_CHD_CrudePrev, "model_old_CHD_table", subset_data, avg_CHD, "70_79_13_22")

model_9a_STROKE_table1 <- create_results_table(model_9a_STROKE_CrudePrev, "model_9a_Stroke_table", subset_data, avg_stroke, "70_79_13_22")
model_9b_STROKE_table1 <- create_results_table(model_9b_STROKE_CrudePrev, "model_9b_Stroke_table", subset_data, avg_stroke, "70_79_13_22")
model_9c_STROKE_table1 <- create_results_table(model_9c_STROKE_CrudePrev, "model_9c_Stroke_table", subset_data, avg_stroke, "70_79_13_22")
model_9d_STROKE_table1 <- create_results_table(model_9d_STROKE_CrudePrev, "model_9d_Stroke_table", subset_data, avg_stroke, "70_79_13_22")
model_old_STROKE_table1 <- create_results_table(model_old_STROKE_CrudePrev, "model_old_Stroke_table", subset_data, avg_stroke, "70_79_13_22")

#### 
#' Table 1 Formatter ---
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

# Use function to format tables
formatted_table1_model_9a <- format_table1 (model_9a_CHD_table1, model_9a_STROKE_table1)
formatted_table1_model_9b <- format_table1 (model_9b_CHD_table1, model_9b_STROKE_table1)
formatted_table1_model_9c <- format_table1 (model_9c_CHD_table1, model_9c_STROKE_table1)
formatted_table1_model_9d <- format_table1 (model_9d_CHD_table1, model_9d_STROKE_table1)
formatted_table1_model_old <- format_table1 (model_old_CHD_table1, model_old_STROKE_table1)

write.csv(formatted_table1_model_9a, "TableS5_single_formatted_08_9a.csv", row.names = FALSE)
write.csv(formatted_table1_model_9b, "TableS5_single_formatted_06_9b.csv", row.names = FALSE)
write.csv(formatted_table1_model_9c, "TableS5_single_formatted_04_9c.csv", row.names = FALSE)
write.csv(formatted_table1_model_9d, "TableS5_single_formatted_02_9d.csv", row.names = FALSE)
write.csv(formatted_table1_model_old, "TableS5_single_formatted_old.csv", row.names = FALSE)


###############################################################################################
# ANOMALY EFFECT SIZE SENSITIVITY ANALYSIS ----
#
# This script combines anomaly effect size, associated CI, and percent
# prevalence contribution from models 9a, 9b, and 9c, as one table
###############################################################################################

all_variables <- unique(c(
  model_9a_CHD_table1$Variable,
  model_9b_CHD_table1$Variable,
  model_9c_CHD_table1$Variable,
  model_9d_CHD_table1$Variable,
  model_9a_STROKE_table1$Variable,
  model_9b_STROKE_table1$Variable,
  model_9d_STROKE_table1$Variable
))

####
#' Extract metrics from model table 1 (see create_results_table())
#'
#' @param model_table Results table from create_results_table
#' @param variables Vector of variable names to extract
#' @return List with effect sizes, CIs, and percent contributions
####
get_effect_size_ci_pct <- function(model_table, variables) {
  # Create lookup tables for effect size, CI, and percent contribution
  effect_size_lookup <- setNames(model_table$Pop_Weight_Effect_Size_Anomaly, model_table$Variable)
  ci_lookup <- setNames(model_table$Pop_Weight_Effect_Size_Anomaly_CI, model_table$Variable)
  pct_lookup <- setNames(model_table$Pop_Weight_Percent_Contribution, model_table$Variable)
  
  # Get values in order of requested variables, with NA where not present
  effect_sizes <- ifelse(variables %in% names(effect_size_lookup), effect_size_lookup[variables], "-")
  cis <- ifelse(variables %in% names(ci_lookup), ci_lookup[variables], "-")
  pcts <- ifelse(variables %in% names(pct_lookup), pct_lookup[variables], "-")
  
  return(list(effect_size = effect_sizes, ci = cis, pct = pcts))
}

# Create a data frame with all variables
combined_table <- data.frame(
  Variable = all_variables,
  stringsAsFactors = FALSE
)

# Add CHD model columns
chd_9a <- get_effect_size_ci_pct(model_9a_CHD_table1, all_variables)
chd_9b <- get_effect_size_ci_pct(model_9b_CHD_table1, all_variables)
chd_9c <- get_effect_size_ci_pct(model_9c_CHD_table1, all_variables)
chd_9d <- get_effect_size_ci_pct(model_9d_CHD_table1, all_variables)

# Add Stroke model columns
stroke_9a <- get_effect_size_ci_pct(model_9a_STROKE_table1, all_variables)
stroke_9b <- get_effect_size_ci_pct(model_9b_STROKE_table1, all_variables)
stroke_9c <- get_effect_size_ci_pct(model_9c_STROKE_table1, all_variables)
stroke_9d <- get_effect_size_ci_pct(model_9d_STROKE_table1, all_variables)

# Combine all columns
combined_table <- cbind(
  combined_table,
  # CHD models
  CHD_Model_9a_Effect_Size = chd_9a$effect_size,
  CHD_Model_9a_CI = chd_9a$ci,
  CHD_Model_9a_Pct = chd_9a$pct,
  CHD_Model_9b_Effect_Size = chd_9b$effect_size,
  CHD_Model_9b_CI = chd_9b$ci,
  CHD_Model_9b_Pct = chd_9b$pct,
  CHD_Model_9c_Effect_Size = chd_9c$effect_size,
  CHD_Model_9c_CI = chd_9c$ci,
  CHD_Model_9c_Pct = chd_9c$pct,
  CHD_Model_9d_CI = chd_9d$ci,
  CHD_Model_9d_Pct = chd_9d$pct,
  
  # Stroke models
  Stroke_Model_9a_Effect_Size = stroke_9a$effect_size,
  Stroke_Model_9a_CI = stroke_9a$ci,
  Stroke_Model_9a_Pct = stroke_9a$pct,
  Stroke_Model_9b_Effect_Size = stroke_9b$effect_size,
  Stroke_Model_9b_CI = stroke_9b$ci,
  Stroke_Model_9b_Pct = stroke_9b$pct,
  Stroke_Model_9c_Effect_Size = stroke_9c$effect_size,
  Stroke_Model_9c_CI = stroke_9c$ci,
  Stroke_Model_9c_Pct = stroke_9c$pct,
  Stroke_Model_9d_CI = stroke_9d$ci,
  Stroke_Model_9d_Pct = stroke_9d$pct
)

# Write the combined table to CSV
write.csv(combined_table, "Table_S5_Combined.csv", row.names = FALSE)
