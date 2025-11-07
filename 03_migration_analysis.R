###############################################################################################
# MIGRATION SENSITIVITY ANALYSIS FILTERING ----
#
# Generate filtered subset_data for migration sensitivity analysis based on bottom/top 50th percentile
# of in/out migration counties
###############################################################################################

setwd(working_directory_output)

# Extract FIPS as strings and find common FIPS then convert back to integers
common_fips <- Reduce(intersect, list(
  as.character(county_noflow$FIPS),
  as.character(inflow_county_US_foreign$FIPS),
  as.character(outflow_county_US_foreign$FIPS)
)) |> as.integer()

remove_rows_with_half_nas <- function(df) {
  # Calculate number of NAs per row
  na_count <- rowSums(is.na(df))
  # Get threshold (half of columns)
  threshold <- (ncol(df)-1) / 2
  # Keep rows with fewer NAs than threshold
  df[na_count < threshold, ]
}

####
#' Function to acquire bottom 50th percentile in/out/both migration County FIPS and means
#' Calculates yearly total population then takes proportion based on that
#' @param inflow_data inflow dataset (ex: "inflow_county_US")
#' @param noflow_data noflow dataset
#' @param outflow_data outflow dataset
#' @param type Either "inflow" or "outflow"
#' @return List containing merged data and bottom 50% County FIPS subset_data for in/out/both migration
####

process_migration_data_yearly <- function(inflow_data, noflow_data, outflow_data, type = c("inflow", "outflow")) {
  type <- match.arg(type)
  
  inflow_data <- inflow_data[,1:11]
  noflow_data <- noflow_data[,1:11]
  outflow_data <- outflow_data[,1:11]
  
  inflow_data <- na.omit(inflow_data)
  noflow_data <- na.omit(noflow_data)
  outflow_data <- na.omit(outflow_data)
  
  # Subset data to FIPS with only some NAs
  # inflow_data <- remove_rows_with_half_nas(inflow_data)
  # noflow_data <- remove_rows_with_half_nas(noflow_data)
  # outflow_data <- remove_rows_with_half_nas(outflow_data)
  
  # Ensure common FIPS across all datasets
  common_fips <- Reduce(intersect, list(
    inflow_data$FIPS,
    noflow_data$FIPS,
    outflow_data$FIPS
  ))
  
  # Subset data to common FIPS
  inflow_data <- inflow_data |> filter(FIPS %in% common_fips)
  noflow_data <- noflow_data |> filter(FIPS %in% common_fips)
  outflow_data <- outflow_data |> filter(FIPS %in% common_fips)
  
  # Compute yearly TotalPopulation = inflow + noflow - outflow
  TotalPopulation <- inflow_data[, -1] + noflow_data[, -1] - outflow_data[, -1]
  
  # Normalize yearly migration by yearly TotalPopulation
  mig_proportion <- get(paste0(type,"_data"))[, -1] / TotalPopulation
  
  # Add FIPS back and compute mean across years
  mig_proportion$FIPS <- get(paste0(type,"_data"))$FIPS
  mig_proportion$mean_mig <- rowMeans(mig_proportion[, 1:10], na.rm = TRUE)  # 10 years (2011-2020)
  # Add FIPS back and compute mean across years
  TotalPopulation$FIPS <- get(paste0(type,"_data"))$FIPS
  
  # Filter to bottom 50% and top 50% of mean migration
  median_mig <- median(mig_proportion$mean_mig, na.rm = TRUE)
  bottom_50 <- mig_proportion |> filter(mean_mig < median_mig)
  top_50 <- mig_proportion |> filter(mean_mig > median_mig)
  
  list(
    merged = mig_proportion,
    bottom_50 = bottom_50,
    top_50 = top_50,
    TotalPopulation = TotalPopulation
  )
}

####
#' Function to find elements in either set but not in the intersection
#' @param x data set 1
#' @param y data set 2
####

outersect <- function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}

#' Function to filter subset_data by migration_data
#'
#' @param data geospatial dataset (ex:subset_data)
#' @param migration_data migration county FIPS and in/out/both means (ex: inflow_result$bottom_50)
####

filter_subset <- function(data, migration_data) {
  semi_join(data, migration_data, by = c("CountyFIPS" = "FIPS"))
}

#### Create migration results V2 ####
migration_results <- list()

# Process US data (non-foreign)
inflow_result <- process_migration_data_yearly(
  inflow_county_US, county_noflow, outflow_county_US, "inflow"
)
outflow_result <- process_migration_data_yearly(
  inflow_county_US, county_noflow, outflow_county_US, "outflow"
)

# Process foreign data
inflow_foreign_result <- process_migration_data_yearly(
  inflow_county_US_foreign, county_noflow, outflow_county_US_foreign, "inflow"
)
outflow_foreign_result <- process_migration_data_yearly(
  inflow_county_US_foreign, county_noflow, outflow_county_US_foreign, "outflow"
)

# Combine results
migration_results[["bottom_50"]] <- list(
  inflow = inflow_result$bottom_50,
  outflow = outflow_result$bottom_50,
  bothFIPS = intersect(inflow_result$bottom_50$FIPS, outflow_result$bottom_50$FIPS),
  inflow_subset = filter_subset(subset_data, inflow_result$bottom_50),
  outflow_subset = filter_subset(subset_data, outflow_result$bottom_50),
  both_subset = filter_subset(subset_data, semi_join(inflow_result$bottom_50, outflow_result$bottom_50, by = "FIPS")),
  uncommonFIPS = outersect(inflow_result$bottom_50$FIPS, outflow_result$bottom_50$FIPS)
)

migration_results[["bottom_50_foreign"]] <- list(
  inflow = inflow_foreign_result$bottom_50,
  outflow = outflow_foreign_result$bottom_50,
  bothFIPS = intersect(inflow_foreign_result$bottom_50$FIPS, outflow_foreign_result$bottom_50$FIPS),
  inflow_subset = filter_subset(subset_data, inflow_foreign_result$bottom_50),
  outflow_subset = filter_subset(subset_data, outflow_foreign_result$bottom_50),
  both_subset = filter_subset(subset_data, semi_join(inflow_foreign_result$bottom_50, outflow_foreign_result$bottom_50, by = "FIPS")),
  uncommonFIPS = outersect(inflow_foreign_result$bottom_50$FIPS, outflow_foreign_result$bottom_50$FIPS)
)

migration_results[["top_50"]] <- list(
  inflow = inflow_result$top_50,
  outflow = outflow_result$top_50,
  bothFIPS = intersect(inflow_result$top_50$FIPS, outflow_result$top_50$FIPS),
  inflow_subset = filter_subset(subset_data, inflow_result$top_50),
  outflow_subset = filter_subset(subset_data, outflow_result$top_50),
  both_subset = filter_subset(subset_data, semi_join(inflow_result$top_50, outflow_result$top_50, by = "FIPS")),
  uncommonFIPS = outersect(inflow_result$top_50$FIPS, outflow_result$top_50$FIPS)
)

migration_results[["top_50_foreign"]] <- list(
  inflow = inflow_foreign_result$top_50,
  outflow = outflow_foreign_result$top_50,
  bothFIPS = intersect(inflow_foreign_result$top_50$FIPS, outflow_foreign_result$top_50$FIPS),
  inflow_subset = filter_subset(subset_data, inflow_foreign_result$top_50),
  outflow_subset = filter_subset(subset_data, outflow_foreign_result$top_50),
  both_subset = filter_subset(subset_data, semi_join(inflow_foreign_result$top_50, outflow_foreign_result$top_50, by = "FIPS")),
  uncommonFIPS = outersect(inflow_foreign_result$top_50$FIPS, outflow_foreign_result$top_50$FIPS)
)

median_migration_values <- list(
  US_only = list(
    inflow_median = median(inflow_result$merged$mean_mig, na.rm = TRUE),
    outflow_median = median(outflow_result$merged$mean_mig, na.rm = TRUE)
  ),
  US_and_Foreign = list(
    inflow_median = median(inflow_foreign_result$merged$mean_mig, na.rm = TRUE),
    outflow_median = median(outflow_foreign_result$merged$mean_mig, na.rm = TRUE)
  )
)

#### Filter negative total population ####
negative_rows <- inflow_foreign_result$TotalPopulation %>% 
  filter(if_any(everything(), ~ . < 0))

setwd(working_directory_data) #for MacOS
save(migration_results,
     file = "migration_results.Rdata")
setwd(working_directory_output) #for MacOS

#### Generate ArcGIS tables ####

ARC_migration_results_bot_inflow <- migration_results$bottom_50_foreign$inflow[,c("FIPS","mean_mig")]
ARC_migration_results_bot_inflow$FIPS <- sprintf("%05d", ARC_migration_results_bot_inflow$FIPS)
ARC_migration_results_bot_inflow$FIPS <- as.character(ARC_migration_results_bot_inflow$FIPS)
names(ARC_migration_results_bot_inflow) <- c("FIPS","mean_mig_bot_inflow")

ARC_migration_results_bot_outflow <- migration_results$bottom_50_foreign$outflow[,c("FIPS","mean_mig")]
ARC_migration_results_bot_outflow$FIPS <- sprintf("%05d", ARC_migration_results_bot_outflow$FIPS)
ARC_migration_results_bot_outflow$FIPS <- as.character(ARC_migration_results_bot_outflow$FIPS)
names(ARC_migration_results_bot_outflow) <- c("FIPS","mean_mig_bot_outflow")

ARC_migration_results_top_inflow <- migration_results$top_50_foreign$inflow[,c("FIPS","mean_mig")]
ARC_migration_results_top_inflow$FIPS <- sprintf("%05d", ARC_migration_results_top_inflow$FIPS)
ARC_migration_results_top_inflow$FIPS <- as.character(ARC_migration_results_top_inflow$FIPS)
names(ARC_migration_results_top_inflow) <- c("FIPS","mean_mig_top_inflow")

ARC_migration_results_top_outflow <- migration_results$top_50_foreign$outflow[,c("FIPS","mean_mig")]
ARC_migration_results_top_outflow$FIPS <- sprintf("%05d", ARC_migration_results_top_outflow$FIPS)
ARC_migration_results_top_outflow$FIPS <- as.character(ARC_migration_results_top_outflow$FIPS)
names(ARC_migration_results_top_outflow) <- c("FIPS","mean_mig_top_outflow")

ARC_migration_results_bot_bothflow <- data.frame(migration_results$bottom_50_foreign$bothFIPS)
names(ARC_migration_results_bot_bothflow) <- "FIPS"
ARC_migration_results_bot_bothflow$Is_Bot_Bothflow <- TRUE
ARC_migration_results_bot_bothflow$FIPS <- sprintf("%05d", ARC_migration_results_bot_bothflow$FIPS)
ARC_migration_results_bot_bothflow$FIPS <- as.character(ARC_migration_results_bot_bothflow$FIPS)

ARC_migration_results_top_bothflow <- data.frame(migration_results$top_50_foreign$bothFIPS)
names(ARC_migration_results_top_bothflow) <- "FIPS"
ARC_migration_results_top_bothflow$Is_Top_Bothflow <- TRUE
ARC_migration_results_top_bothflow$FIPS <- sprintf("%05d", ARC_migration_results_top_bothflow$FIPS)
ARC_migration_results_top_bothflow$FIPS <- as.character(ARC_migration_results_top_bothflow$FIPS)

ARC_migration_results_outer_net <- data.frame(migration_results$bottom_50$uncommonFIPS)
ARC_migration_results_outer_net$Is_Uncommon_FIPS <- TRUE
names(ARC_migration_results_outer_net) <- c("FIPS","Is_Uncommon_FIPS")
ARC_migration_results_outer_net$FIPS <- sprintf("%05d", ARC_migration_results_outer_net$FIPS)

### Write files for ArcGIS ###
write.csv(ARC_migration_results_bot_inflow, "ARCGIS_mig_bot_inflow.csv", row.names = FALSE)
write.csv(ARC_migration_results_bot_outflow, "ARCGIS_mig_bot_outflow.csv", row.names = FALSE)
write.csv(ARC_migration_results_top_inflow, "ARCGIS_mig_top_inflow.csv", row.names = FALSE)
write.csv(ARC_migration_results_top_outflow, "ARCGIS_mig_top_outflow.csv", row.names = FALSE)
write.csv(ARC_migration_results_bot_bothflow, "ARCGIS_mig_bot_bothflow_FIPS.csv", row.names = FALSE)
write.csv(ARC_migration_results_top_bothflow, "ARCGIS_mig_top_bothflow_FIPS.csv", row.names = FALSE)
write.csv(ARC_migration_results_outer_net, "ARCGIS_mig_highlow_lowhigh_FIPS.csv", row.names= FALSE)

names(ARC_migration_results_top_inflow) <- c("FIPS", "mean_mig_inflow")
names(ARC_migration_results_bot_inflow) <- c("FIPS", "mean_mig_inflow")
ARC_migration_inflow <- rbind(ARC_migration_results_top_inflow,ARC_migration_results_bot_inflow)
names(ARC_migration_results_top_outflow) <- c("FIPS", "mean_mig_outflow")
names(ARC_migration_results_bot_outflow) <- c("FIPS", "mean_mig_outflow")
ARC_migration_outflow <- rbind(ARC_migration_results_top_outflow,ARC_migration_results_bot_outflow)

write.csv(ARC_migration_inflow, "ARCGIS_migration_inflow.csv", row.names = FALSE)
write.csv(ARC_migration_outflow, "ARCGIS_migration_outflow.csv", row.names = FALSE)

###############################################################################################
# MIGRATION SENSITIVITY ANALYSIS  ----
#
# anomaly models based on filtered subset_data which
# only include counties with low in-migration/out-migration/both (recreates figure 2)
###############################################################################################

setwd(working_directory_output)

#### Make anomaly effect size table for in-migration/out-migration/both ####

# subset_data filtered to below median of in/out/both from US data

subset_data_bot_50_in <- migration_results[[2]][[4]]
subset_data_bot_50_out <- migration_results[[2]][[5]]
subset_data_bot_50_both <- migration_results[[2]][[6]]
subset_data_top_50_in <- migration_results[[4]][[4]]
subset_data_top_50_out <- migration_results[[4]][[5]]
subset_data_top_50_both <- migration_results[[4]][[6]]

# List of subsets
subset_data_list <- list(
  bottom_50 = list(
    "in" = subset_data_bot_50_in,
    "out" = subset_data_bot_50_out,
    "both" = subset_data_bot_50_both),
  top_50 = list(
    "in" = subset_data_top_50_in,
    "out" = subset_data_top_50_out,
    "both" = subset_data_top_50_both 
  )
)

# List of dependent variable names
dependent_vars <- c("CHD_CrudePrev", "STROKE_CrudePrev")
dependent_vars_names <- c("CHD", "Stroke")

# Environmental variables
env_vars <- c(
  "HI_C_Change70_79_13_22",
  "Change_Temp_C_70_79_13_22",
  "Change_RH_70_79_13_22",
  "windU_diff70_79_13_22",
  "windV_diff70_79_13_22",
  "latent_diff70_79_13_22_P_Mill",
  "solar_diff70_79_13_22_P_Mill",
  "thermal_diff70_79_13_22_P_Mill",
  "sensible_diff70_79_13_22_P_Mill",
  "evap_diff70_79_13_22_P_x10",
  "pressure_diff70_79_13_22_P_div10",
  "precip_diff70_79_13_22_P_x10",
  "transpir_diff70_79_13_22_P_x1000",
  "downwards_solar_diff70_79_13_22_P_Mill"
)

# Better looking names
env_var_names <- c(
  "Heat Index", "Air Temperature", "Humidity", "Eastward Wind", "Northward Wind",
  "Latent Heat Flux", "Surface-Absorbed Sunlight", "Thermal Radiation",
  "Sensible Heat Flux", "Evaporation", "Surface Pressure", "Precipitation",
  "Transpiration", "Sunlight"
)
# Model suffix
model_suffix <- paste(
  "+ OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces",
  "+ CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1",
  "+ Temp_C_2013_2022 + LCchangeMEAN + (1 | CountyFIPS)"
)
# Final result container
migration_model_results <- list()

# Loop over dependent variables (CHD/Stroke)
for (x in seq_along(dependent_vars)) {
  dep_var <- dependent_vars[x]
  dep_name <- dependent_vars_names[x]
  
  migration_model_results[[dep_name]] <- list()
  
  # Loop over percentiles (bottom_50/top_50)
  for (subset_name in names(subset_data_list)) {
    # Initialize storage for migration types
    migration_model_results[[dep_name]][[subset_name]] <- list()
    
    # Loop over migration types (in/out/both)
    for (mig_type in c("in", "out", "both")) {
      data <- subset_data_list[[subset_name]][[mig_type]]
      
      # Compute averages
      averages <- sapply(env_vars, function(var) mean(data[[var]], na.rm = TRUE))
      
      # Initialize results dataframe
      result_df <- tibble(
        Variable = env_var_names,
        SE = NA_real_,
        Effect_Anomaly_Size = NA_real_,
        Lower_CI = NA_real_,
        Upper_CI = NA_real_,
        Combined_CI = NA_character_
      )
      
      for (i in seq_along(env_vars)) {
        var <- env_vars[i]
        formula_str <- paste(dep_var, "~", var, model_suffix)
        formula <- as.formula(formula_str)
        
        model <- lmer(formula, data = data)
        model_summary <- summary(model)
        
        est <- coef(model_summary)[var, "Estimate"]
        se <- coef(model_summary)[var, "Std. Error"]
        
        avg <- averages[var]
        scaled_est <- est * avg
        
        result_df$SE[i] <- se
        result_df$Effect_Anomaly_Size[i] <- scaled_est
        result_df$Lower_CI[i] <- scaled_est - 1.96 * se
        result_df$Upper_CI[i] <- scaled_est + 1.96 * se
        result_df$Combined_CI[i] <- sprintf("(%.2f, %.2f)", 
                                            result_df$Lower_CI[i], 
                                            result_df$Upper_CI[i])
      }
      
      # Store per migration type
      migration_model_results[[dep_name]][[subset_name]][[mig_type]] <- result_df
    }
  }
}

# To use migration_model_results, call migration_model_results[[A]][[B]] where:
# A = 1 -> CHD, 2 -> Stroke
# B = 1 -> in dataframe, 2 -> out dataframe, 3 -> both dataframe

Bot_Migration_CHD <- bind_cols(migration_model_results[[1]][[1]])
Bot_Migration_CHD <- Bot_Migration_CHD[,c("Variable...1", "Effect_Anomaly_Size...3", "Combined_CI...6", "Effect_Anomaly_Size...9", "Combined_CI...12", "Effect_Anomaly_Size...15", "Combined_CI...18")]
Bot_Migration_Stroke <- bind_cols(migration_model_results[[2]][[1]])
Bot_Migration_Stroke <- Bot_Migration_Stroke[,c("Effect_Anomaly_Size...3", "Combined_CI...6", "Effect_Anomaly_Size...9", "Combined_CI...12", "Effect_Anomaly_Size...15", "Combined_CI...18")]

Bot_Migration_Table <- bind_cols(Bot_Migration_CHD, Bot_Migration_Stroke)

Top_Migration_CHD <- bind_cols(migration_model_results[[1]][[2]])
Top_Migration_CHD <- Top_Migration_CHD[,c("Variable...1", "Effect_Anomaly_Size...3", "Combined_CI...6", "Effect_Anomaly_Size...9", "Combined_CI...12", "Effect_Anomaly_Size...15", "Combined_CI...18")]
Top_Migration_Stroke <- bind_cols(migration_model_results[[2]][[2]])
Top_Migration_Stroke <- Top_Migration_Stroke[,c("Effect_Anomaly_Size...3", "Combined_CI...6", "Effect_Anomaly_Size...9", "Combined_CI...12", "Effect_Anomaly_Size...15", "Combined_CI...18")]

Top_Migration_Table <- bind_cols(Top_Migration_CHD, Top_Migration_Stroke)

write.csv(Bot_Migration_Table, "TableS6_Bot_Migration.csv", row.names = FALSE)
write.csv(Top_Migration_Table, "TableS7_Top_Migration.csv", row.names = FALSE)
