#####################################################################################################
# MIGRATION DATA LOADING // ENVIRONMENT SETUP (NEW SCRIPTS) ----
#
#####################################################################################################

#### Read files in separate directory #########

setwd(working_directory_data)

# List of files to process

files_in <- c(
  "countyinflow1112.csv",
  "countyinflow1213.csv",
  "countyinflow1314.csv",
  "countyinflow1415.csv",
  "countyinflow1516.csv",
  "countyinflow1617.csv",
  "countyinflow1718.csv",
  "countyinflow1819.csv",
  "countyinflow1920.csv",
  "countyinflow2021.csv",
  "countyinflow2122.csv"
)

files_out <- c(
  "countyoutflow1112.csv",
  "countyoutflow1213.csv",
  "countyoutflow1314.csv",
  "countyoutflow1415.csv",
  "countyoutflow1516.csv",
  "countyoutflow1617.csv",
  "countyoutflow1718.csv",
  "countyoutflow1819.csv",
  "countyoutflow1920.csv",
  "countyoutflow2021.csv",
  "countyoutflow2122.csv"
)

files_all <- c(
  "1112inmigall.xls",
  "1213inmigall.xls",
  "1314inmigall.xls",
  "1415inmigall.xls",
  "1516inmigall.xls",
  "1617inmigall.xls",
  "1718inmigall.xls",
  "1819inmigall.xls",
  "1920inmigall.xls",
  "2021inmigall.xlsx",
  "2122inmigall.xlsx"
)

#### Initialize lists to store data ####

us_in_data <- list()
us_foreign_in_data <- list()
us_out_data <- list()
us_foreign_out_data <- list()
us_no_data <- list()
us_foreign_no_data <- list()
total_migration_in_data <- list()
total_migration_out_data <- list()
all_migration <- list()

#### Process total migration ####

for (file in files_all) {
  # Read file based on extension
  file_path <- file
  if (grepl("\\.xlsx$", file)) {
    data <- readxl::read_xlsx(file_path)
  } else {
    data <- readxl::read_xls(file_path)
  }
  data <- as.data.frame(data)
  # Extract year from filename (first 4 characters)
  year <- substr(file, 1, 4)
  
  # Safely extract data from row 8, column 5 (DEPRECATED)
  if (nrow(data) >= 8 && ncol(data) >= 5) {
    all_data <- data[8, 5]
    all_migration[[year]] <- all_data
  } else {
    warning(paste("File", file, "doesn't have enough rows/columns. Skipping..."))
    all_migration[[year]] <- NA
  }
}

#### Process in-migration files ####

for (file in files_in) {
  data <- read.csv(file)
  
  ## Find FIPS for in-migration files (y2_ prefix)
  data$y2_statefips <- as.character(data$y2_statefips)
  data$y2_countyfips <- str_pad(as.character(data$y2_countyfips), 
                                width = 3, side = "left", pad = "0")
  data$y1_statefips <- as.character(data$y1_statefips)
  data$y1_countyfips <- str_pad(as.character(data$y1_countyfips), 
                                width = 3, side = "left", pad = "0")
  data$FIPS <- as.integer(paste0(data$y2_statefips, data$y2_countyfips)) # FIPS FOR DESTINATION
  data$FIPS2 <- as.integer(paste0(data$y1_statefips,data$y1_countyfips)) # FIPS FOR ORIGIN -> LIKELY DEPRECATED
  year <- substr(file, 13, 16)
  
  ## Process US-only records (DEPRECATED PER USING US & FOREIGN)
  # Process no migration
  us_only_no <- data[str_ends(data$y1_countyname, "Non-migrants"), 
                     c("FIPS", "n2")]
  if (nrow(us_only_no) > 0) {
    names(us_only_no)[2] <- paste0("n2_", year)
    us_no_data[[year]] <- us_only_no
  }
  
  us_only <- data[str_ends(data$y1_countyname, "-US") & 
                    !str_ends(data$y1_countyname, "-US and Foreign"), 
                  c("FIPS", "n2")]
  if (nrow(us_only) > 0) {
    names(us_only)[2] <- paste0("n2_", year)
    us_in_data[[year]] <- us_only
  }
  
  # Process US-and-Foreign records
  us_foreign <- data[str_ends(data$y1_countyname, "-US and Foreign"), 
                     c("FIPS", "n2")]
  if (nrow(us_foreign) > 0) {
    names(us_foreign)[2] <- paste0("n2_", year)
    us_foreign_in_data[[year]] <- us_foreign
  }
}

#### Process out-migration files ####
for (file in files_out) {
  data <- read.csv(file)
  
  # Find FIPS for out-migration files (y1_ prefix)
  data$y1_statefips <- as.character(data$y1_statefips)
  data$y1_countyfips <- str_pad(as.character(data$y1_countyfips), 
                                width = 3, side = "left", pad = "0")
  data$FIPS <- as.integer(paste0(data$y1_statefips, data$y1_countyfips)) #FIPS FOR ORIGIN
  
  year <- substr(file, 14, 17)
  
  # Extract and remove Total Migration records for out-migration (based on y2_countyname)
  total_migration_out <- data[startsWith(data$y2_countyname, "Total Migration"), 
                              c("FIPS", "n2")]
  if (nrow(total_migration_out) > 0) {
    names(total_migration_out)[2] <- paste0("n2_", year)
    total_migration_out_data[[year]] <- total_migration_out
  }
  data <- data[!startsWith(data$y2_countyname, "Total Migration"), ]
  
  # Process US-only records (DEPRECATED PER USING US & FOREIGN)
  us_only <- data[str_ends(data$y2_countyname, "-US") & 
                    !str_ends(data$y2_countyname, "-US and Foreign"), 
                  c("FIPS", "n2")]
  if (nrow(us_only) > 0) {
    names(us_only)[2] <- paste0("n2_", year)
    us_out_data[[year]] <- us_only
  }
  
  # Process US-and-Foreign records
  us_foreign <- data[str_ends(data$y2_countyname, "-US and Foreign"), 
                     c("FIPS", "n2")]
  if (nrow(us_foreign) > 0) {
    names(us_foreign)[2] <- paste0("n2_", year)
    us_foreign_out_data[[year]] <- us_foreign
  }
}

#### Remove -1 values ####

#### 
#' Function to remove -1s
#' 
#' 
####

# Create a function to remove rows with -1 from each dataset in a list
remove_minus1_rows <- function(data_list) {
  lapply(data_list, function(df) {
    df[rowSums(df == -1, na.rm = TRUE) == 0, ]
  })
}

# Apply the 'remove minus one' function to each migration dataframe
us_in_data_clean <- remove_minus1_rows(us_in_data)
us_foreign_in_data_clean <- remove_minus1_rows(us_foreign_in_data)
us_out_data_clean <- remove_minus1_rows(us_out_data)
us_foreign_out_data_clean <- remove_minus1_rows(us_foreign_out_data)
us_no_data_clean <- remove_minus1_rows(us_no_data)

final_us_in <- Reduce(function(x, y) full_join(x, y, by = "FIPS"), us_in_data_clean)
final_us_foreign_in <- Reduce(function(x, y) full_join(x, y, by = "FIPS"), us_foreign_in_data_clean)
final_us_out <- Reduce(function(x, y) full_join(x, y, by = "FIPS"), us_out_data_clean)
final_us_foreign_out <- Reduce(function(x, y) full_join(x, y, by = "FIPS"), us_foreign_out_data_clean)
final_no <- Reduce(function(x, y) full_join(x, y, by = "FIPS"), us_no_data_clean)

# Verify no -1 (DEPRECATED)
rows_with_minus1_for_in <- final_us_foreign_in[rowSums(final_us_foreign_in == -1) > 0, ]
rows_with_minus1_for_in <- na.omit(rows_with_minus1_for_in)
rows_with_minus1_for_out <- final_us_foreign_out[rowSums(final_us_foreign_out == -1) > 0, ]
rows_with_minus1_for_out <- na.omit(rows_with_minus1_for_out)
rows_with_minus1_no <- final_no[rowSums(final_no == -1) > 0, ]
rows_with_minus1_no <- na.omit(rows_with_minus1_no)

# Combine all migration data
final_all_migration <- t(do.call(rbind, all_migration))

setwd(working_directory_data)

# Write the results
write.csv(final_us_in, "county_inflow_US_only.csv", row.names = FALSE)
write.csv(final_us_foreign_in, "county_inflow_US_and_Foreign.csv", row.names = FALSE)
write.csv(final_us_out, "county_outflow_US_only.csv", row.names = FALSE)
write.csv(final_us_foreign_out, "county_outflow_US_and_Foreign.csv", row.names = FALSE)
write.csv(final_no, "county_noflow.csv", row.names = FALSE)
write.csv(final_all_migration, "final_all_migration.csv", row.names = FALSE) #(LIKELY DEPRECATED)

inflow_county_US <- read.csv("county_inflow_US_only.csv", header = TRUE, na.strings = "NA")
inflow_county_US_foreign <- read.csv("county_inflow_US_and_Foreign.csv", header = TRUE, na.strings = "NA")
outflow_county_US <- read.csv("county_outflow_US_only.csv", header = TRUE, na.strings = "NA")
outflow_county_US_foreign <- read.csv("county_outflow_US_and_Foreign.csv", header = TRUE, na.strings = "NA")
county_noflow <- read.csv("county_noflow.csv", header = TRUE, na.strings = "NA")

setwd(working_directory_output)

#####################################################################################################
# SUPPLEMENTAL // ENVIRONMENT SETUP (OLD SCRIPTS) ----
#
#####################################################################################################

#### Set working directory ####################

setwd(working_directory_data)

#### read and merge csvs ######################
# combine all tracts and climate division data 
unjoined_tracts <- read.csv("Unjoined_Tracts2010_Edited.csv", header = TRUE, na.strings = "NA")
tracts <- read.csv("Tracts2010_CLIMDIV.csv", header = TRUE, na.strings = "NA")
climdiv <- rbind(tracts, unjoined_tracts)

# read and merge all other fields
county <- read.csv("County2010FIPS.csv",  header = TRUE, na.strings = "NA")
hvi <- read.csv("Climate_Resilience.csv", header = TRUE, na.strings = "NA")
hi <- read.csv("decade_HI_C_06_28_2024.csv", header = TRUE, na.strings = "NA")
era5_vars <- read.csv("DecadeMeans_ERA5variables_07_26_2024.csv", header = TRUE, na.strings = "NA")
lc <- read.csv("LandCover_Change_04_15_2024.csv", header = TRUE, na.strings = "NA")
places <- read.csv("final_result_11_13_2023.csv", header = TRUE,  na.strings = "NA") # averaged places data
svi <- read.csv("SVI_2018.csv", header = TRUE,  na.strings = "NA")
seasonal <- read.csv("heatmetricstest_08_08_2024.csv", header = TRUE, na.strings = "NA")
down_solar <- read.csv("DecadeMeans_Downwards_Solar_07_29_2024.csv", header = TRUE, na.strings = "NA")
pop18over <- read.csv("Pop18Over_ACS.csv", header = TRUE, na.strings = "NA")

setwd(working_directory_output)
# sort climdiv data
climdiv_sorted <- climdiv[order(climdiv$GEOID10), ]

# change column name to enable merge #
colnames(county)[colnames(county) == "TractFIPS"] <-"GEOID10"

# sort climdiv data
county_sorted <- county[order(county$GEOID10), ]

# merge the climdiv and hvi data
climdiv_county <- merge(climdiv_sorted, county_sorted, by = "GEOID10")


# change column name to enable merge #
colnames(hvi)[colnames(hvi) == "GEOID"] <-"GEOID10"

# sort HVI data by GEOID10
hvi_sorted <- hvi[order(hvi$GEOID10), ]

# merge the climdiv and hvi data
climdiv_hvi <- merge(climdiv_county, hvi_sorted, by = "GEOID10")


# sort svi data
# change column name to enable merge #
colnames(svi)[colnames(svi) == "FIPS"] <-"GEOID10"
svi_sorted <- svi[order(svi$GEOID10), ]

# merge the climdiv and svi data
climdiv_svi <- merge(climdiv_hvi, svi_sorted, by = "GEOID10")

# change column name in places and sort #
colnames(places)[colnames(places) == "TractFIPS"] <- "GEOID10"
places_sorted <- places[order(places$GEOID10), ]

# merge the climdiv and places data
climdiv_places <- merge(climdiv_svi, places_sorted, by = "GEOID10")

# sort #
pop18over_sorted <- pop18over[order(pop18over$GEOID10), ]

# merge the climdiv and places data
climdiv_pop18over <- merge(climdiv_places, pop18over_sorted, by = "GEOID10")

# sort the merged data by CLIMDIV
climdiv_pop18over <- climdiv_pop18over[order(climdiv_pop18over$CLIMDIV), ]
print(climdiv_pop18over)

# Sort the hi data frame by CLIMDIV
hi_sorted <- hi[order(hi$CLIMDIV), ]

# Merge hi_sorted with climdiv_places by CLIMDIV
climdiv_hi <- merge(climdiv_pop18over, hi_sorted, by = "CLIMDIV")

# Sort era5 data frame by CLIMDIV
era5_vars_sorted <- era5_vars[order(era5_vars$CLIMDIV), ]

# Merge era5_vars_sorted with climdiv by CLIMDIV
climdiv_era5 <- merge(climdiv_hi, era5_vars_sorted, by = "CLIMDIV")

# Sort lc data frame by CLIMDIV
lc_sorted <- lc[order(lc$CLIMDIV), ]

# Merge the result with lc_sorted by CLIMDIV
climdiv_lc <- merge(climdiv_era5, lc_sorted, by = "CLIMDIV")

# Sort heatwave data frame by CLIMDIV
seasonal_sorted <- seasonal[order(seasonal$CLIMDIV), ]

# Merge the result with heatwaves sorted by CLIMDIV
climdiv_seasonal <- merge(climdiv_lc, seasonal_sorted, by = "CLIMDIV")

# Sort heatwave data frame by CLIMDIV
down_solar_sorted <- down_solar[order(down_solar$CLIMDIV), ]

# Merge the result with heatwaves sorted by CLIMDIV
merged_data <- merge(climdiv_seasonal, down_solar_sorted, by = "CLIMDIV")

# Convert to NAs and keep as data frame
merged_data <- lapply(merged_data, function(x) ifelse(x == "<Null>", NA, x))
merged_data <- lapply(merged_data, function(x) ifelse(x == -999, NA, x))
merged_data <- data.frame(merged_data)

# Print the merged data
print(merged_data)

# convert from character to numeric
merged_data$CLIMDIV <- as.numeric(as.character(merged_data$CLIMDIV))
merged_data$Pop18Over <- as.numeric(as.character(merged_data$Pop18Over))
merged_data$CHD_CrudePrev <- as.numeric(as.character(merged_data$CHD_CrudePrev))
merged_data$SPL_THEME1 <- as.numeric(as.character(merged_data$SPL_THEME1))
merged_data$BPMED_CrudePrev <- as.numeric(as.character(merged_data$BPMED_CrudePrev))
merged_data$EP_AGE65 <- as.numeric(as.character(merged_data$EP_AGE65))
merged_data$CSMOKING_CrudePrev <- as.numeric(as.character(merged_data$CSMOKING_CrudePrev))
merged_data$CHECKUP_CrudePrev <- as.numeric(as.character(merged_data$CHECKUP_CrudePrev))
merged_data$OBESITY_CrudePrev <- as.numeric(as.character(merged_data$OBESITY_CrudePrev))
merged_data$STROKE_CrudePrev <- as.numeric(as.character(merged_data$STROKE_CrudePrev))

#### subset ###################
# subset data for variables of interest
subset_data <- merged_data[, c("TractFIPS.1","Pop18Over", "TotalPopulation", "CLIMDIV","HI_C_Change60_69_13_22", 
                               "HI_C_Change70_79_13_22", "HI_C_Change80_89_13_22", "HI_C_Change90_99_13_22", "HI_C_Change2000_2009_13_22",
                               "Temp_C_2013_2022", "Change_Temp_C_70_79_13_22", "Change_Temp_C_80_89_13_22", "Change_Temp_C_90_99_13_22",
                               "Change_Temp_C_2000_2009_13_22", "Change_RH_70_79_13_22", "Change_RH_80_89_13_22", "Change_RH_90_99_13_22",
                               "Change_RH_2000_2009_13_22", "PCT_ImperviousSurfaces",
                               "CHD_CrudePrev", "SPL_THEME1", "BPMED_CrudePrev",
                               "EP_AGE65", "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "STROKE_CrudePrev",
                               "OBESITY_CrudePrev", "CountyFIPS", "windU_diff70_79_13_22", "windU_diff80_89_13_22", "windU_diff90_99_13_22",
                               "windU_diff2000_2009_13_22", "windU_diff70_89_03_22", "windV_diff70_79_13_22", "windV_diff80_89_13_22", "windV_diff90_99_13_22", 
                               "windV_diff2000_2009_13_22", "windV_diff70_89_03_22","latent_diff70_79_13_22","latent_diff80_89_13_22", "latent_diff90_99_13_22",
                               "latent_diff2000_2009_13_22", "latent_diff70_89_03_22", "solar_diff70_79_13_22", "solar_diff80_89_13_22", "solar_diff90_99_13_22", 
                               "solar_diff2000_2009_13_22", "solar_diff70_89_03_22", "thermal_diff70_79_13_22", "thermal_diff80_89_13_22", "thermal_diff90_99_13_22",
                               "thermal_diff2000_2009_13_22", "thermal_diff70_89_03_22", "sensible_diff70_79_13_22", "sensible_diff80_89_13_22", "sensible_diff90_99_13_22", 
                               "sensible_diff2000_2009_13_22", "sensible_diff70_89_03_22", "evap_diff70_79_13_22",  "evap_diff80_89_13_22",  "evap_diff90_99_13_22", 
                               "evap_diff2000_2009_13_22", "evap_diff70_89_03_22", "pressure_diff70_79_13_22", "pressure_diff80_89_13_22", "pressure_diff90_99_13_22",
                               "pressure_diff2000_2009_13_22", "pressure_diff70_89_03_22", "precip_diff70_79_13_22", "precip_diff80_89_13_22", "precip_diff90_99_13_22", 
                               "precip_diff2000_2009_13_22", "precip_diff70_89_03_22", "transpir_diff70_79_13_22", "transpir_diff80_89_13_22", "transpir_diff90_99_13_22", 
                               "transpir_diff2000_2009_13_22", "transpir_diff70_89_03_22",
                               "LCchangeMEAN", 
                               "downwards_solar_diff70_79_13_22", "downwards_solar_diff80_89_13_22",
                               "downwards_solar_diff90_99_13_22", "downwards_solar_diff2000_2009_13_22", 
                               "Anomaly_YearlyMeanHI_1322", "Anomaly_DaysAbove32_1322", "Anomaly_DaysAbove34_1322",
                               "Anomaly_DaysAbove36_1322", "Anomaly_DaysAbove38_1322", "Anomaly_DaysAbove41_1322",
                               "Anomaly_HoursAbove32_1322", "Anomaly_HoursAbove34_1322",
                               "Anomaly_HoursAbove36_1322", "Anomaly_HoursAbove38_1322", "Anomaly_HoursAbove41_1322",
                               "Anomaly_SummerMeanHI_1322", "Anomaly_WinterMeanHI_1322","Anomaly_Daily_1322", "Anomaly_Weekly_1322",
                               "Anomaly_Monthly_1322","Anomaly_Yearly_1322", "Anomaly_SD_MinHI_Summer_1322",
                               "Anomaly_SD_MinHI_Winter_1322","Anomaly_SD_MaxHI_Summer_1322", "Anomaly_SD_MaxHI_Winter_1322",
                               "Anomaly_HeatWaves_4_32_1322", "Anomaly_HeatWaves_7_32_1322", 
                               "Anomaly_HeatWaves_4_34_1322", "Anomaly_HeatWaves_7_34_1322","Anomaly_HeatWaves_4_36_1322",
                               "Anomaly_HeatWaves_7_36_1322", "Anomaly_MeanNighttimeHI_Other_1322","Anomaly_MeanNighttimeHI_Summer_1322",
                               "Anomaly_MeanNighttimeHI_Winter_1322", "Anomaly_MeanNighttimeHI_Annual_1322", 
                               "Anomaly_MeanDaytimeHI_Other_1322","Anomaly_MeanDaytimeHI_Summer_1322",
                               "Anomaly_MeanDaytimeHI_Winter_1322", "Anomaly_MeanDaytimeHI_Annual_1322", 
                               "Anomaly_MedianSummerMaxHI_1322",
                               "Anomaly_Percentile95SummerMaxHI_1322", "Anomaly_WarmTailSpreadSummer_1322", "Anomaly_MeanSummerMaxHI_1322"
)]


## Exclude blank values and change to NA
subset_data[subset_data == ""] <- NA


#### Scale variables ##############################################
# divide variables by 1 million
subset_data$latent_diff70_79_13_22_P_Mill <- subset_data$latent_diff70_79_13_22 / 1000000
subset_data$latent_diff80_89_13_22_P_Mill <- subset_data$latent_diff80_89_13_22 / 1000000
subset_data$latent_diff90_99_13_22_P_Mill <- subset_data$latent_diff90_99_13_22 / 1000000
subset_data$latent_diff2000_2009_13_22_P_Mill <- subset_data$latent_diff2000_2009_13_22 / 1000000

subset_data$solar_diff70_79_13_22_P_Mill <- subset_data$solar_diff70_79_13_22 / 1000000
subset_data$solar_diff80_89_13_22_P_Mill <- subset_data$solar_diff80_89_13_22 / 1000000
subset_data$solar_diff90_99_13_22_P_Mill <- subset_data$solar_diff90_99_13_22 / 1000000
subset_data$solar_diff2000_2009_13_22_P_Mill <- subset_data$solar_diff2000_2009_13_22 / 1000000

subset_data$thermal_diff70_79_13_22_P_Mill <- subset_data$thermal_diff70_79_13_22 / 1000000
subset_data$thermal_diff80_89_13_22_P_Mill <- subset_data$thermal_diff80_89_13_22 / 1000000
subset_data$thermal_diff90_99_13_22_P_Mill <- subset_data$thermal_diff90_99_13_22 / 1000000
subset_data$thermal_diff2000_2009_13_22_P_Mill <- subset_data$thermal_diff2000_2009_13_22 / 1000000

subset_data$sensible_diff70_79_13_22_P_Mill <- subset_data$sensible_diff70_79_13_22 / 1000000
subset_data$sensible_diff80_89_13_22_P_Mill <- subset_data$sensible_diff80_89_13_22 / 1000000
subset_data$sensible_diff90_99_13_22_P_Mill <- subset_data$sensible_diff90_99_13_22 / 1000000
subset_data$sensible_diff2000_2009_13_22_P_Mill <- subset_data$sensible_diff2000_2009_13_22 / 1000000

subset_data$downwards_solar_diff70_79_13_22_P_Mill <- subset_data$downwards_solar_diff70_79_13_22 / 1000000
subset_data$downwards_solar_diff80_89_13_22_P_Mill <- subset_data$downwards_solar_diff80_89_13_22 / 1000000
subset_data$downwards_solar_diff90_99_13_22_P_Mill <- subset_data$downwards_solar_diff90_99_13_22 / 1000000
subset_data$downwards_solar_diff2000_2009_13_22_P_Mill <- subset_data$downwards_solar_diff2000_2009_13_22 / 1000000

# divide variables by 10 to scale 
subset_data$pressure_diff70_79_13_22_P_div10 <- subset_data$pressure_diff70_79_13_22 / 10
subset_data$pressure_diff80_89_13_22_P_div10 <- subset_data$pressure_diff80_89_13_22 / 10
subset_data$pressure_diff90_99_13_22_P_div10 <- subset_data$pressure_diff90_99_13_22 / 10
subset_data$pressure_diff2000_2009_13_22_P_div10 <- subset_data$pressure_diff2000_2009_13_22 / 10

# divide variables by 10 to scale 
subset_data$OBESITY_CrudePrev_P_div10 <- subset_data$OBESITY_CrudePrev / 10

# View the modified data frame
head(subset_data)

# multiply variables by 10 to scale 
subset_data$evap_diff70_79_13_22_P_x10 <- subset_data$evap_diff70_79_13_22 * 10
subset_data$evap_diff80_89_13_22_P_x10 <- subset_data$evap_diff80_89_13_22 * 10
subset_data$evap_diff90_99_13_22_P_x10 <- subset_data$evap_diff90_99_13_22 * 10
subset_data$evap_diff2000_2009_13_22_P_x10 <- subset_data$evap_diff2000_2009_13_22 * 10

# multiply variables by 10 to scale 
subset_data$precip_diff70_79_13_22_P_x10 <- subset_data$precip_diff70_79_13_22 * 10
subset_data$precip_diff80_89_13_22_P_x10 <- subset_data$precip_diff80_89_13_22 * 10
subset_data$precip_diff90_99_13_22_P_x10 <- subset_data$precip_diff90_99_13_22 * 10
subset_data$precip_diff2000_2009_13_22_P_x10 <- subset_data$precip_diff2000_2009_13_22 * 10

# View the modified data frame
head(subset_data)

# multiply variables by 1000 to scale 
# Multiply specified variables by 1000 and store with '_P_x1000' suffix
subset_data$transpir_diff70_79_13_22_P_x1000 <- subset_data$transpir_diff70_79_13_22 * 1000
subset_data$transpir_diff80_89_13_22_P_x1000 <- subset_data$transpir_diff80_89_13_22 * 1000
subset_data$transpir_diff90_99_13_22_P_x1000 <- subset_data$transpir_diff90_99_13_22 * 1000
subset_data$transpir_diff2000_2009_13_22_P_x1000 <- subset_data$transpir_diff2000_2009_13_22 * 1000


# View the modified data frame
head(subset_data)

#write to csv file for arcgis 
write.csv(subset_data, "Scaled_data_forARCgis_09_16_2024.csv", row.names = FALSE)

#####################################################################################################
###################### The following code creates the supplemental figures #############################
#####################################################################################################
################################################ Figure S5 - seasonal metrics moved to supplemental #############################
# create empty list to store results from linear models
coefficients_chd_seasonal <- list()
se_chd_seasonal <- list()
lower_bound_chd_seasonal <- list()
upper_bound_chd_seasonal <- list()

# calculate anomaly size
avg_dailyAnomaly <- mean(subset_data$Anomaly_Daily_1322)
avg_weeklyAnomaly <- mean(subset_data$Anomaly_Weekly_1322)
avg_monthlyAnomaly <- mean(subset_data$Anomaly_Monthly_1322)
avg_yearlyAnomaly <- mean(subset_data$Anomaly_Yearly_1322)
avg_SDminHISummer <- mean(subset_data$Anomaly_SD_MinHI_Summer_1322)
avg_SDminHIWinter <- mean(subset_data$Anomaly_SD_MinHI_Winter_1322)
avg_SDmaxHISummer <- mean(subset_data$Anomaly_SD_MaxHI_Summer_1322)
avg_SDmaxHIWinter <- mean(subset_data$Anomaly_SD_MaxHI_Winter_1322)

avg_heatwaves4_32 <- mean(subset_data$Anomaly_HeatWaves_4_32_1322)
avg_heatwaves7_32 <- mean(subset_data$Anomaly_HeatWaves_7_32_1322)
avg_heatwaves4_34 <- mean(subset_data$Anomaly_HeatWaves_4_34_1322)
avg_heatwaves7_34 <- mean(subset_data$Anomaly_HeatWaves_7_34_1322)
avg_heatwaves4_36 <- mean(subset_data$Anomaly_HeatWaves_4_36_1322)
avg_heatwaves7_36 <- mean(subset_data$Anomaly_HeatWaves_7_36_1322)

avg_warmTailSpreadSummerHI <- mean(subset_data$Anomaly_WarmTailSpreadSummer_1322)
#### CHD seasonal metrics  #############################
# daily anomaly 
chd_dailyAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_Daily_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_dailyAnomaly)
chd_dailyAnomaly_summary <- summary(chd_dailyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_dailyAnomaly <- coef(chd_dailyAnomaly_summary)["Anomaly_Daily_1322", "Estimate"] * avg_dailyAnomaly
se_chd_seasonal$chd_dailyAnomaly <- coef(chd_dailyAnomaly_summary)["Anomaly_Daily_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_dailyAnomaly <- coefficients_chd_seasonal$chd_dailyAnomaly - 1.96 * se_chd_seasonal$chd_dailyAnomaly
upper_bound_chd_seasonal$chd_dailyAnomaly <- coefficients_chd_seasonal$chd_dailyAnomaly + 1.96 * se_chd_seasonal$chd_dailyAnomaly

# weekly anomaly 
chd_weeklyAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_Weekly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_weeklyAnomaly)
chd_weeklyAnomaly_summary <- summary(chd_weeklyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_weeklyAnomaly <- coef(chd_weeklyAnomaly_summary)["Anomaly_Weekly_1322", "Estimate"] * avg_weeklyAnomaly
se_chd_seasonal$chd_weeklyAnomaly <- coef(chd_weeklyAnomaly_summary)["Anomaly_Weekly_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_weeklyAnomaly <- coefficients_chd_seasonal$chd_weeklyAnomaly - 1.96 * se_chd_seasonal$chd_weeklyAnomaly
upper_bound_chd_seasonal$chd_weeklyAnomaly <- coefficients_chd_seasonal$chd_weeklyAnomaly + 1.96 * se_chd_seasonal$chd_weeklyAnomaly

# monthly anomaly 
chd_monthlyAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_Monthly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_monthlyAnomaly)
chd_monthlyAnomaly_summary <- summary(chd_monthlyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_monthlyAnomaly <- coef(chd_monthlyAnomaly_summary)["Anomaly_Monthly_1322", "Estimate"] * avg_monthlyAnomaly
se_chd_seasonal$chd_monthlyAnomaly <- coef(chd_monthlyAnomaly_summary)["Anomaly_Monthly_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_monthlyAnomaly <- coefficients_chd_seasonal$chd_monthlyAnomaly - 1.96 * se_chd_seasonal$chd_monthlyAnomaly
upper_bound_chd_seasonal$chd_monthlyAnomaly <- coefficients_chd_seasonal$chd_monthlyAnomaly + 1.96 * se_chd_seasonal$chd_monthlyAnomaly

# yearly anomaly 
chd_yearlyAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_Yearly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_yearlyAnomaly)
chd_yearlyAnomaly_summary <- summary(chd_yearlyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_yearlyAnomaly <- coef(chd_yearlyAnomaly_summary)["Anomaly_Yearly_1322", "Estimate"] * avg_yearlyAnomaly
se_chd_seasonal$chd_yearlyAnomaly <- coef(chd_yearlyAnomaly_summary)["Anomaly_Yearly_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_yearlyAnomaly <- coefficients_chd_seasonal$chd_yearlyAnomaly - 1.96 * se_chd_seasonal$chd_yearlyAnomaly
upper_bound_chd_seasonal$chd_yearlyAnomaly <- coefficients_chd_seasonal$chd_yearlyAnomaly + 1.96 * se_chd_seasonal$chd_yearlyAnomaly

# SDminSummer 
chd_SDminSummer <- lmer(CHD_CrudePrev ~  Anomaly_SD_MinHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_SDminSummer)
chd_SDminSummer_summary <- summary(chd_SDminSummer)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_SDminSummer <- coef(chd_SDminSummer_summary)["Anomaly_SD_MinHI_Summer_1322", "Estimate"] * avg_SDminHISummer
se_chd_seasonal$chd_SDminSummer <- coef(chd_SDminSummer_summary)["Anomaly_SD_MinHI_Summer_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_SDminSummer <- coefficients_chd_seasonal$chd_SDminSummer - 1.96 * se_chd_seasonal$chd_SDminSummer
upper_bound_chd_seasonal$chd_SDminSummer <- coefficients_chd_seasonal$chd_SDminSummer + 1.96 * se_chd_seasonal$chd_SDminSummer

# SDmaxSummer 
chd_SDmaxSummer <- lmer(CHD_CrudePrev ~  Anomaly_SD_MaxHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_SDmaxSummer)
chd_SDmaxSummer_summary <- summary(chd_SDmaxSummer)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_SDmaxSummer <- coef(chd_SDmaxSummer_summary)["Anomaly_SD_MaxHI_Summer_1322", "Estimate"] * avg_SDmaxHISummer
se_chd_seasonal$chd_SDmaxSummer <- coef(chd_SDmaxSummer_summary)["Anomaly_SD_MaxHI_Summer_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_SDmaxSummer <- coefficients_chd_seasonal$chd_SDmaxSummer - 1.96 * se_chd_seasonal$chd_SDmaxSummer
upper_bound_chd_seasonal$chd_SDmaxSummer <- coefficients_chd_seasonal$chd_SDmaxSummer + 1.96 * se_chd_seasonal$chd_SDmaxSummer

# SDminWinter 
chd_SDminWinter <- lmer(CHD_CrudePrev ~  Anomaly_SD_MinHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_SDminWinter)
chd_SDminWinter_summary <- summary(chd_SDminWinter)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_SDminWinter <- coef(chd_SDminWinter_summary)["Anomaly_SD_MinHI_Winter_1322", "Estimate"] * avg_SDminHIWinter
se_chd_seasonal$chd_SDminWinter <- coef(chd_SDminWinter_summary)["Anomaly_SD_MinHI_Winter_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_SDminWinter <- coefficients_chd_seasonal$chd_SDminWinter - 1.96 * se_chd_seasonal$chd_SDminWinter
upper_bound_chd_seasonal$chd_SDminWinter <- coefficients_chd_seasonal$chd_SDminWinter + 1.96 * se_chd_seasonal$chd_SDminWinter

# SDmaxWinter 
chd_SDmaxWinter <- lmer(CHD_CrudePrev ~  Anomaly_SD_MaxHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_SDmaxWinter)
chd_SDmaxWinter_summary <- summary(chd_SDmaxWinter)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_SDmaxWinter <- coef(chd_SDmaxWinter_summary)["Anomaly_SD_MaxHI_Winter_1322", "Estimate"] * avg_SDmaxHIWinter
se_chd_seasonal$chd_SDmaxWinter <- coef(chd_SDmaxWinter_summary)["Anomaly_SD_MaxHI_Winter_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_SDmaxWinter <- coefficients_chd_seasonal$chd_SDmaxWinter - 1.96 * se_chd_seasonal$chd_SDmaxWinter
upper_bound_chd_seasonal$chd_SDmaxWinter <- coefficients_chd_seasonal$chd_SDmaxWinter + 1.96 * se_chd_seasonal$chd_SDmaxWinter

# heat waves 4 days over 32 
chd_heatwaves4_32 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_4_32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves4_32)
chd_heatwaves4_32_summary <- summary(chd_heatwaves4_32)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves4_32 <- coef(chd_heatwaves4_32_summary)["Anomaly_HeatWaves_4_32_1322", "Estimate"] * avg_heatwaves4_32
se_chd_seasonal$chd_heatwaves4_32 <- coef(chd_heatwaves4_32_summary)["Anomaly_HeatWaves_4_32_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves4_32 <- coefficients_chd_seasonal$chd_heatwaves4_32 - 1.96 * se_chd_seasonal$chd_heatwaves4_32
upper_bound_chd_seasonal$chd_heatwaves4_32 <- coefficients_chd_seasonal$chd_heatwaves4_32 + 1.96 * se_chd_seasonal$chd_heatwaves4_32

# heat waves 7 days over 32 
chd_heatwaves7_32 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_7_32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves7_32)
chd_heatwaves7_32_summary <- summary(chd_heatwaves7_32)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves7_32 <- coef(chd_heatwaves7_32_summary)["Anomaly_HeatWaves_7_32_1322", "Estimate"] * avg_heatwaves7_32
se_chd_seasonal$chd_heatwaves7_32 <- coef(chd_heatwaves7_32_summary)["Anomaly_HeatWaves_7_32_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves7_32 <- coefficients_chd_seasonal$chd_heatwaves7_32 - 1.96 * se_chd_seasonal$chd_heatwaves7_32
upper_bound_chd_seasonal$chd_heatwaves7_32 <- coefficients_chd_seasonal$chd_heatwaves7_32 + 1.96 * se_chd_seasonal$chd_heatwaves7_32

# heat waves 4 days over 34 
chd_heatwaves4_34 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_4_34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves4_34)
chd_heatwaves4_34_summary <- summary(chd_heatwaves4_34)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves4_34 <- coef(chd_heatwaves4_34_summary)["Anomaly_HeatWaves_4_34_1322", "Estimate"] * avg_heatwaves4_34
se_chd_seasonal$chd_heatwaves4_34 <- coef(chd_heatwaves4_34_summary)["Anomaly_HeatWaves_4_34_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves4_34 <- coefficients_chd_seasonal$chd_heatwaves4_34 - 1.96 * se_chd_seasonal$chd_heatwaves4_34
upper_bound_chd_seasonal$chd_heatwaves4_34 <- coefficients_chd_seasonal$chd_heatwaves4_34 + 1.96 * se_chd_seasonal$chd_heatwaves4_34

# heat waves 7 days over 34 
chd_heatwaves7_34 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_7_34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves7_34)
chd_heatwaves7_34_summary <- summary(chd_heatwaves7_34)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves7_34 <- coef(chd_heatwaves7_34_summary)["Anomaly_HeatWaves_7_34_1322", "Estimate"] * avg_heatwaves7_34
se_chd_seasonal$chd_heatwaves7_34 <- coef(chd_heatwaves7_34_summary)["Anomaly_HeatWaves_7_34_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves7_34 <- coefficients_chd_seasonal$chd_heatwaves7_34 - 1.96 * se_chd_seasonal$chd_heatwaves7_34
upper_bound_chd_seasonal$chd_heatwaves7_34 <- coefficients_chd_seasonal$chd_heatwaves7_34 + 1.96 * se_chd_seasonal$chd_heatwaves7_34

# heat waves 4 days over 36 
chd_heatwaves4_36 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_4_36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves4_36)
chd_heatwaves4_36_summary <- summary(chd_heatwaves4_36)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves4_36 <- coef(chd_heatwaves4_36_summary)["Anomaly_HeatWaves_4_36_1322", "Estimate"] * avg_heatwaves4_36
se_chd_seasonal$chd_heatwaves4_36 <- coef(chd_heatwaves4_36_summary)["Anomaly_HeatWaves_4_36_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves4_36 <- coefficients_chd_seasonal$chd_heatwaves4_36 - 1.96 * se_chd_seasonal$chd_heatwaves4_36
upper_bound_chd_seasonal$chd_heatwaves4_36 <- coefficients_chd_seasonal$chd_heatwaves4_36 + 1.96 * se_chd_seasonal$chd_heatwaves4_36

# heat waves 7 days over 36 
chd_heatwaves7_36 <- lmer(CHD_CrudePrev ~  Anomaly_HeatWaves_7_36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_heatwaves7_36)
chd_heatwaves7_36_summary <- summary(chd_heatwaves7_36)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_heatwaves7_36 <- coef(chd_heatwaves7_36_summary)["Anomaly_HeatWaves_7_36_1322", "Estimate"] * avg_heatwaves7_36
se_chd_seasonal$chd_heatwaves7_36 <- coef(chd_heatwaves7_36_summary)["Anomaly_HeatWaves_7_36_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_heatwaves7_36 <- coefficients_chd_seasonal$chd_heatwaves7_36 - 1.96 * se_chd_seasonal$chd_heatwaves7_36
upper_bound_chd_seasonal$chd_heatwaves7_36 <- coefficients_chd_seasonal$chd_heatwaves7_36 + 1.96 * se_chd_seasonal$chd_heatwaves7_36

# difference between spread of 95th percentile and median of daily summer max HI
chd_tailSpreadSummerHImax <- lmer(CHD_CrudePrev ~  Anomaly_WarmTailSpreadSummer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                    (1 | CountyFIPS), data = subset_data)
summary(chd_tailSpreadSummerHImax)
chd_tailSpreadSummerHImax_summary <- summary(chd_tailSpreadSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_chd_seasonal$chd_tailSpreadSummerHImax <- coef(chd_tailSpreadSummerHImax_summary)["Anomaly_WarmTailSpreadSummer_1322", "Estimate"] * avg_warmTailSpreadSummerHI
se_chd_seasonal$chd_tailSpreadSummerHImax <- coef(chd_tailSpreadSummerHImax_summary)["Anomaly_WarmTailSpreadSummer_1322", "Std. Error"]
lower_bound_chd_seasonal$chd_tailSpreadSummerHImax <- coefficients_chd_seasonal$chd_tailSpreadSummerHImax - 1.96 * se_chd_seasonal$chd_tailSpreadSummerHImax
upper_bound_chd_seasonal$chd_tailSpreadSummerHImax <- coefficients_chd_seasonal$chd_tailSpreadSummerHImax + 1.96 * se_chd_seasonal$chd_tailSpreadSummerHImax

#### Combine the lists into a data frame #####################
chd_seasonal <- data.frame(
  Variable = c("SD Daily", "SD Weekly", "SD Monthly",
               "SD Yearly", "SD Min Summer", "SD Max Summer", "SD Min Winter", "SD Max Winter", 
               "4 days ≥ 32", "7 days ≥ 32", "4 days ≥ 34", "7 days ≥ 34", "4 days ≥ 36", 
               "7 days ≥ 36", "95th %ile vs Median Max Summer"),
  Effect_Anomaly_Size = unlist(coefficients_chd_seasonal),
  SE = unlist(se_chd_seasonal),
  Lower_CI = unlist(lower_bound_chd_seasonal),
  Upper_CI = unlist(upper_bound_chd_seasonal)
)
#### Stroke seasonal metrics ##########################
# Create empty lists to store results from linear models
coefficients_stroke_seasonal <- list()
se_stroke_seasonal <- list()
pvalue_stroke <- list()
lower_bound_stroke_seasonal <- list()
upper_bound_stroke_seasonal <- list()

### calculate anomaly size
avg_dailyAnomaly <- mean(subset_data$Anomaly_Daily_1322)
avg_weeklyAnomaly <- mean(subset_data$Anomaly_Weekly_1322)
avg_monthlyAnomaly <- mean(subset_data$Anomaly_Monthly_1322)
avg_yearlyAnomaly <- mean(subset_data$Anomaly_Yearly_1322)
avg_SDminHISummer <- mean(subset_data$Anomaly_SD_MinHI_Summer_1322)
avg_SDminHIWinter <- mean(subset_data$Anomaly_SD_MinHI_Winter_1322)
avg_SDmaxHISummer <- mean(subset_data$Anomaly_SD_MaxHI_Summer_1322)
avg_SDmaxHIWinter <- mean(subset_data$Anomaly_SD_MaxHI_Winter_1322)

avg_heatwaves4_32 <- mean(subset_data$Anomaly_HeatWaves_4_32_1322)
avg_heatwaves7_32 <- mean(subset_data$Anomaly_HeatWaves_7_32_1322)
avg_heatwaves4_34 <- mean(subset_data$Anomaly_HeatWaves_4_34_1322)
avg_heatwaves7_34 <- mean(subset_data$Anomaly_HeatWaves_7_34_1322)
avg_heatwaves4_36 <- mean(subset_data$Anomaly_HeatWaves_4_36_1322)
avg_heatwaves7_36 <- mean(subset_data$Anomaly_HeatWaves_7_36_1322)

avg_warmTailSpreadSummerHI <- mean(subset_data$Anomaly_WarmTailSpreadSummer_1322)

###### run the linear models
# daily anomaly 
stroke_dailyAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_Daily_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_dailyAnomaly)
stroke_dailyAnomaly_summary <- summary(stroke_dailyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_dailyAnomaly <- coef(stroke_dailyAnomaly_summary)["Anomaly_Daily_1322", "Estimate"] * avg_dailyAnomaly
se_stroke_seasonal$stroke_dailyAnomaly <- coef(stroke_dailyAnomaly_summary)["Anomaly_Daily_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_dailyAnomaly <- coefficients_stroke_seasonal$stroke_dailyAnomaly - 1.96 * se_stroke_seasonal$stroke_dailyAnomaly
upper_bound_stroke_seasonal$stroke_dailyAnomaly <- coefficients_stroke_seasonal$stroke_dailyAnomaly + 1.96 * se_stroke_seasonal$stroke_dailyAnomaly

# weekly anomaly 
stroke_weeklyAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_Weekly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_weeklyAnomaly)
stroke_weeklyAnomaly_summary <- summary(stroke_weeklyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_weeklyAnomaly <- coef(stroke_weeklyAnomaly_summary)["Anomaly_Weekly_1322", "Estimate"] * avg_weeklyAnomaly
se_stroke_seasonal$stroke_weeklyAnomaly <- coef(stroke_weeklyAnomaly_summary)["Anomaly_Weekly_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_weeklyAnomaly <- coefficients_stroke_seasonal$stroke_weeklyAnomaly - 1.96 * se_stroke_seasonal$stroke_weeklyAnomaly
upper_bound_stroke_seasonal$stroke_weeklyAnomaly <- coefficients_stroke_seasonal$stroke_weeklyAnomaly + 1.96 * se_stroke_seasonal$stroke_weeklyAnomaly

# monthly anomaly 
stroke_monthlyAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_Monthly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)
summary(stroke_monthlyAnomaly)
stroke_monthlyAnomaly_summary <- summary(stroke_monthlyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_monthlyAnomaly <- coef(stroke_monthlyAnomaly_summary)["Anomaly_Monthly_1322", "Estimate"] * avg_monthlyAnomaly
se_stroke_seasonal$stroke_monthlyAnomaly <- coef(stroke_monthlyAnomaly_summary)["Anomaly_Monthly_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_monthlyAnomaly <- coefficients_stroke_seasonal$stroke_monthlyAnomaly - 1.96 * se_stroke_seasonal$stroke_monthlyAnomaly
upper_bound_stroke_seasonal$stroke_monthlyAnomaly <- coefficients_stroke_seasonal$stroke_monthlyAnomaly + 1.96 * se_stroke_seasonal$stroke_monthlyAnomaly

# yearly anomaly 
stroke_yearlyAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_Yearly_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_yearlyAnomaly)
stroke_yearlyAnomaly_summary <- summary(stroke_yearlyAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_yearlyAnomaly <- coef(stroke_yearlyAnomaly_summary)["Anomaly_Yearly_1322", "Estimate"] * avg_yearlyAnomaly
se_stroke_seasonal$stroke_yearlyAnomaly <- coef(stroke_yearlyAnomaly_summary)["Anomaly_Yearly_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_yearlyAnomaly <- coefficients_stroke_seasonal$stroke_yearlyAnomaly - 1.96 * se_stroke_seasonal$stroke_yearlyAnomaly
upper_bound_stroke_seasonal$stroke_yearlyAnomaly <- coefficients_stroke_seasonal$stroke_yearlyAnomaly + 1.96 * se_stroke_seasonal$stroke_yearlyAnomaly

# SDminSummer 
stroke_SDminSummer <- lmer(STROKE_CrudePrev ~  Anomaly_SD_MinHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_SDminSummer)
stroke_SDminSummer_summary <- summary(stroke_SDminSummer)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_SDminSummer <- coef(stroke_SDminSummer_summary)["Anomaly_SD_MinHI_Summer_1322", "Estimate"] * avg_SDminHISummer
se_stroke_seasonal$stroke_SDminSummer <- coef(stroke_SDminSummer_summary)["Anomaly_SD_MinHI_Summer_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_SDminSummer <- coefficients_stroke_seasonal$stroke_SDminSummer - 1.96 * se_stroke_seasonal$stroke_SDminSummer
upper_bound_stroke_seasonal$stroke_SDminSummer <- coefficients_stroke_seasonal$stroke_SDminSummer + 1.96 * se_stroke_seasonal$stroke_SDminSummer

# SDmaxSummer 
stroke_SDmaxSummer <- lmer(STROKE_CrudePrev ~  Anomaly_SD_MaxHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_SDmaxSummer)
stroke_SDmaxSummer_summary <- summary(stroke_SDmaxSummer)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_SDmaxSummer <- coef(stroke_SDmaxSummer_summary)["Anomaly_SD_MaxHI_Summer_1322", "Estimate"] * avg_SDmaxHISummer
se_stroke_seasonal$stroke_SDmaxSummer <- coef(stroke_SDmaxSummer_summary)["Anomaly_SD_MaxHI_Summer_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_SDmaxSummer <- coefficients_stroke_seasonal$stroke_SDmaxSummer - 1.96 * se_stroke_seasonal$stroke_SDmaxSummer
upper_bound_stroke_seasonal$stroke_SDmaxSummer <- coefficients_stroke_seasonal$stroke_SDmaxSummer + 1.96 * se_stroke_seasonal$stroke_SDmaxSummer

# SDminWinter 
stroke_SDminWinter <- lmer(STROKE_CrudePrev ~ Anomaly_SD_MinHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_SDminWinter)
stroke_SDminWinter_summary <- summary(stroke_SDminWinter)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_SDminWinter <- coef(stroke_SDminWinter_summary)["Anomaly_SD_MinHI_Winter_1322", "Estimate"] * avg_SDminHIWinter
se_stroke_seasonal$stroke_SDminWinter <- coef(stroke_SDminWinter_summary)["Anomaly_SD_MinHI_Winter_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_SDminWinter <- coefficients_stroke_seasonal$stroke_SDminWinter - 1.96 * se_stroke_seasonal$stroke_SDminWinter
upper_bound_stroke_seasonal$stroke_SDminWinter <- coefficients_stroke_seasonal$stroke_SDminWinter + 1.96 * se_stroke_seasonal$stroke_SDminWinter

# SDmaxWinter 
stroke_SDmaxWinter <- lmer(STROKE_CrudePrev ~  Anomaly_SD_MaxHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_SDmaxWinter)
stroke_SDmaxWinter_summary <- summary(stroke_SDmaxWinter)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_SDmaxWinter <- coef(stroke_SDmaxWinter_summary)["Anomaly_SD_MaxHI_Winter_1322", "Estimate"] * avg_SDmaxHIWinter
se_stroke_seasonal$stroke_SDmaxWinter <- coef(stroke_SDmaxWinter_summary)["Anomaly_SD_MaxHI_Winter_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_SDmaxWinter <- coefficients_stroke_seasonal$stroke_SDmaxWinter - 1.96 * se_stroke_seasonal$stroke_SDmaxWinter
upper_bound_stroke_seasonal$stroke_SDmaxWinter <- coefficients_stroke_seasonal$stroke_SDmaxWinter + 1.96 * se_stroke_seasonal$stroke_SDmaxWinter

# heat waves 4 days over 32 
stroke_heatwaves4_32 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_4_32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves4_32)
stroke_heatwaves4_32_summary <- summary(stroke_heatwaves4_32)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves4_32 <- coef(stroke_heatwaves4_32_summary)["Anomaly_HeatWaves_4_32_1322", "Estimate"] * avg_heatwaves4_32
se_stroke_seasonal$stroke_heatwaves4_32 <- coef(stroke_heatwaves4_32_summary)["Anomaly_HeatWaves_4_32_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves4_32 <- coefficients_stroke_seasonal$stroke_heatwaves4_32 - 1.96 * se_stroke_seasonal$stroke_heatwaves4_32
upper_bound_stroke_seasonal$stroke_heatwaves4_32 <- coefficients_stroke_seasonal$stroke_heatwaves4_32 + 1.96 * se_stroke_seasonal$stroke_heatwaves4_32

# heat waves 7 days over 32 
stroke_heatwaves7_32 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_7_32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves7_32)
stroke_heatwaves7_32_summary <- summary(stroke_heatwaves7_32)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves7_32 <- coef(stroke_heatwaves7_32_summary)["Anomaly_HeatWaves_7_32_1322", "Estimate"] * avg_heatwaves7_32
se_stroke_seasonal$stroke_heatwaves7_32 <- coef(stroke_heatwaves7_32_summary)["Anomaly_HeatWaves_7_32_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves7_32 <- coefficients_stroke_seasonal$stroke_heatwaves7_32 - 1.96 * se_stroke_seasonal$stroke_heatwaves7_32
upper_bound_stroke_seasonal$stroke_heatwaves7_32 <- coefficients_stroke_seasonal$stroke_heatwaves7_32 + 1.96 * se_stroke_seasonal$stroke_heatwaves7_32

# heat waves 4 days over 34 
stroke_heatwaves4_34 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_4_34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves4_34)
stroke_heatwaves4_34_summary <- summary(stroke_heatwaves4_34)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves4_34 <- coef(stroke_heatwaves4_34_summary)["Anomaly_HeatWaves_4_34_1322", "Estimate"] * avg_heatwaves4_34
se_stroke_seasonal$stroke_heatwaves4_34 <- coef(stroke_heatwaves4_34_summary)["Anomaly_HeatWaves_4_34_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves4_34 <- coefficients_stroke_seasonal$stroke_heatwaves4_34 - 1.96 * se_stroke_seasonal$stroke_heatwaves4_34
upper_bound_stroke_seasonal$stroke_heatwaves4_34 <- coefficients_stroke_seasonal$stroke_heatwaves4_34 + 1.96 * se_stroke_seasonal$stroke_heatwaves4_34

# heat waves 7 days over 34 
stroke_heatwaves7_34 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_7_34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves7_34)
stroke_heatwaves7_34_summary <- summary(stroke_heatwaves7_34)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves7_34 <- coef(stroke_heatwaves7_34_summary)["Anomaly_HeatWaves_7_34_1322", "Estimate"] * avg_heatwaves7_34
se_stroke_seasonal$stroke_heatwaves7_34 <- coef(stroke_heatwaves7_34_summary)["Anomaly_HeatWaves_7_34_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves7_34 <- coefficients_stroke_seasonal$stroke_heatwaves7_34 - 1.96 * se_stroke_seasonal$stroke_heatwaves7_34
upper_bound_stroke_seasonal$stroke_heatwaves7_34 <- coefficients_stroke_seasonal$stroke_heatwaves7_34 + 1.96 * se_stroke_seasonal$stroke_heatwaves7_34

# heat waves 4 days over 36 
stroke_heatwaves4_36 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_4_36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves4_36)
stroke_heatwaves4_36_summary <- summary(stroke_heatwaves4_36)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves4_36 <- coef(stroke_heatwaves4_36_summary)["Anomaly_HeatWaves_4_36_1322", "Estimate"] * avg_heatwaves4_36
se_stroke_seasonal$stroke_heatwaves4_36 <- coef(stroke_heatwaves4_36_summary)["Anomaly_HeatWaves_4_36_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves4_36 <- coefficients_stroke_seasonal$stroke_heatwaves4_36 - 1.96 * se_stroke_seasonal$stroke_heatwaves4_36
upper_bound_stroke_seasonal$stroke_heatwaves4_36 <- coefficients_stroke_seasonal$stroke_heatwaves4_36 + 1.96 * se_stroke_seasonal$stroke_heatwaves4_36

# heat waves 7 days over 36 
stroke_heatwaves7_36 <- lmer(STROKE_CrudePrev ~  Anomaly_HeatWaves_7_36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_heatwaves7_36)
stroke_heatwaves7_36_summary <- summary(stroke_heatwaves7_36)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_heatwaves7_36 <- coef(stroke_heatwaves7_36_summary)["Anomaly_HeatWaves_7_36_1322", "Estimate"] * avg_heatwaves7_36
se_stroke_seasonal$stroke_heatwaves7_36 <- coef(stroke_heatwaves7_36_summary)["Anomaly_HeatWaves_7_36_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_heatwaves7_36 <- coefficients_stroke_seasonal$stroke_heatwaves7_36 - 1.96 * se_stroke_seasonal$stroke_heatwaves7_36
upper_bound_stroke_seasonal$stroke_heatwaves7_36 <- coefficients_stroke_seasonal$stroke_heatwaves7_36 + 1.96 * se_stroke_seasonal$stroke_heatwaves7_36

# difference between spread of 95th percentile and median of daily summer max HI
stroke_tailSpreadSummerHImax <- lmer(STROKE_CrudePrev ~  Anomaly_WarmTailSpreadSummer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                       (1 | CountyFIPS), data = subset_data)
summary(stroke_tailSpreadSummerHImax)
stroke_tailSpreadSummerHImax_summary <- summary(stroke_tailSpreadSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke_seasonal$stroke_tailSpreadSummerHImax <- coef(stroke_tailSpreadSummerHImax_summary)["Anomaly_WarmTailSpreadSummer_1322", "Estimate"] * avg_warmTailSpreadSummerHI
se_stroke_seasonal$stroke_tailSpreadSummerHImax <- coef(stroke_tailSpreadSummerHImax_summary)["Anomaly_WarmTailSpreadSummer_1322", "Std. Error"]
lower_bound_stroke_seasonal$stroke_tailSpreadSummerHImax <- coefficients_stroke_seasonal$stroke_tailSpreadSummerHImax - 1.96 * se_stroke_seasonal$stroke_tailSpreadSummerHImax
upper_bound_stroke_seasonal$stroke_tailSpreadSummerHImax <- coefficients_stroke_seasonal$stroke_tailSpreadSummerHImax + 1.96 * se_stroke_seasonal$stroke_tailSpreadSummerHImax

#### Combine the lists into a data frame ##################
stroke_seasonal <- data.frame(
  Variable = c("SD Daily", "SD Weekly", "SD Monthly",
               "SD Yearly", "SD Min Summer", "SD Max Summer", "SD Min Winter", "SD Max Winter", 
               "4 days ≥ 32", "7 days ≥ 32", "4 days ≥ 34", "7 days ≥ 34", "4 days ≥ 36", 
               "7 days ≥ 36", "95th %ile vs Median Max Summer"),
  Effect_Anomaly_Size = unlist(coefficients_stroke_seasonal),
  SE = unlist(se_stroke_seasonal),
  Lower_CI = unlist(lower_bound_stroke_seasonal),
  Upper_CI = unlist(upper_bound_stroke_seasonal)
)

###################################### Figure   S5 forest plot ################################
#### Combined revised forest plot ######################
# Combine the datasets for CHD and Stroke, retaining both outcomes for each variable
combined_seasonal <- rbind(
  cbind(chd_seasonal, Outcome = "CHD"),
  cbind(stroke_seasonal, Outcome = "Stroke")
)
# Extract variable names and create groups based on their prefixes
combined_seasonal$Variable_Group <- sub("_.*", "", combined_seasonal$Variable)

# To check the new grouping
table(combined_seasonal$Variable_Group)

# Define the custom order for Variable_Group
custom_order <- c("SD Max Winter", "SD Min Winter", "SD Max Summer", 
                  "SD Min Summer", "SD Yearly", "SD Monthly", 
                  "SD Weekly", "SD Daily",
                  "7 days ≥ 36", "4 days ≥ 36", "7 days ≥ 34", 
                  "4 days ≥ 34", "7 days ≥ 32", 
                  "4 days ≥ 32", "95th %ile vs Median Max Summer")                


# Reverse the order of the custom_order
reversed_order <- rev(custom_order)

# Convert Variable_Group to a factor with the reversed custom order
combined_seasonal$Variable_Group <- factor(combined_seasonal$Variable_Group, levels = reversed_order)

# Sort the data by Variable_Group, Outcome, and Effect_Anomaly_Size
combined_seasonal <- combined_seasonal[order(combined_seasonal$Variable_Group, combined_seasonal$Outcome), ]

# Create a combined variable that includes both the Variable and Outcome for plotting
combined_seasonal <- combined_seasonal %>%
  mutate(Variable_Outcome = paste(Variable, Outcome, sep = " - "))

# Ensure that Variable_Outcome follows the same order as in the sorted combined_seasonal
combined_seasonal$Variable_Outcome <- factor(combined_seasonal$Variable_Outcome, levels = unique(combined_seasonal$Variable_Outcome))

# write data to csv for figure 
write.csv(combined_seasonal, "Seasonal_Data_09_16_2024.csv")

fig_seasonal <- ggplot(combined_seasonal, aes(x = as.numeric(Effect_Anomaly_Size), y = Variable_Outcome, color = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Add black vertical line at x = 0
  geom_vline(xintercept = -0.25, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = -0.25
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = -0.5
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 0.25
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 0.5
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = as.numeric(Lower_CI), xmax = as.numeric(Upper_CI)), height = 0.2) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +  # Set x-axis limits from -1 to 1
  labs(
    x = "Prevalence",
  ) +
  scale_color_manual(values = c("CHD" = "black", "Stroke" = "red"), guide = guide_legend(title = NULL)) +  # Remove color legend title
  scale_shape_manual(values = c("CHD" = 16, "Stroke" = 17), guide = guide_legend(title = NULL)) +  # Remove shape legend title
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.position = "bottom",
    legend.box.margin = margin(t = -10, b = 10),  # Adjust space above and below the legend
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.title.x = element_text(face = "bold"),  # Make x-axis title bold
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

plot(fig_seasonal)


# Save the plot with a specific resolution
ggsave("Figure_Seasonal.png", plot = fig_seasonal, width = 2.75, height = 5.55, dpi = 400)

################################################ Figure S6 correlation table #############################################
#install.packages("corrplot")
library("corrplot")
library("Hmisc")

# List of specified variables
original_vars <- c("HI_C_Change70_79_13_22", "Change_Temp_C_70_79_13_22", "Change_RH_70_79_13_22",
                   "windU_diff70_79_13_22", "windV_diff70_79_13_22", "latent_diff70_79_13_22",
                   "solar_diff70_79_13_22", "thermal_diff70_79_13_22", "sensible_diff70_79_13_22", "evap_diff70_79_13_22",
                   "pressure_diff70_79_13_22", 
                   "precip_diff70_79_13_22", "transpir_diff70_79_13_22", "downwards_solar_diff70_79_13_22")

# List of new descriptive variable names
new_vars <- c("Heat Index Anomaly", "Air Temperature Anomaly", "Humidity Anomaly", 
              "Eastward Wind Anomaly", "Northward Wind Anomaly", "Latent Heat Flux Anomaly", 
              "Absorbed Sunlight Anomaly", "Thermal Radiation Anomaly", "Sensible Heat Flux Anomaly", "Evaporation Anomaly",
              "Surface Pressure Anomaly", "Precipitation Anomaly", "Transpiration Anomaly", "Sunlight Anomaly")

# Ensure all specified variables are in the dataset
common_vars <- intersect(original_vars, colnames(subset_data))

# Compute the correlation matrix (Rsquared) using the Pearson method
cor_matrix <- cor(subset_data[, common_vars, drop = FALSE], method = "pearson", use = "complete.obs")

# Rename the rows and columns of the correlation matrix
colnames(cor_matrix) <- new_vars[match(colnames(cor_matrix), original_vars)]
rownames(cor_matrix) <- new_vars[match(rownames(cor_matrix), original_vars)]

# Reset the plotting device
plot.new()

# Set font family to Arial
par(family = "Arial")

png("FigureS7_correlogram.png", width = 1100, height = 795)
# Create a correlogram with correlation coefficients as numbers, adjusting the text size - export 1100 width and 795 height
corrplot(cor_matrix, addCoef.col = "black", col = COL2("RdBu"), number.cex = 1,
         tl.cex = 1.5, tl.col = "black", font = 1, mar = c(0, 0, 0, 0))
dev.off()
cor_matrix_2 <- cor_matrix^2
# Optionally save the plot with specified dimensions

# Create a correlogram with correlation coefficients as numbers, adjusting the text size - export 1100 width and 795 height
png("FigureS7_correlogram_2.png", width = 1100, height = 795)
corrplot(cor_matrix_2, addCoef.col = "black", col.lim = c(0,1), col = COL1("Blues"), number.cex = 1,
         tl.cex = 1.5, tl.col = "black", font = 1, mar = c(0, 0, 0, 0))
dev.off()



##########################################################################################################################
################################################ CHD Figure S7 ####################################################
##########################################################################################################################
########################### CHD Lists for Figure S7 ###########################
# create empty list to store results from linear models
coefficients_chd_figS7 <- list()
pvalue_chd_figS7 <- list()
se_chd_figS7 <- list()
lower_bound_chd_figS7 <- list()
upper_bound_chd_figS7 <- list()

# average CHD
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)

########################### CHD 1970 - 1980 ######################
# average anomaly size 
avg_HI_70_79 <- mean(subset_data$HI_C_Change70_79_13_22)
avg_temp_70_79 <- mean(subset_data$Change_Temp_C_70_79_13_22)
avg_rh_70_79 <- mean(subset_data$Change_RH_70_79_13_22)
avg_windu_70_79 <- mean(subset_data$windU_diff70_79_13_22)
avg_windv_70_79 <- mean(subset_data$windV_diff70_79_13_22)
avg_latent_70_79 <- mean(subset_data$latent_diff70_79_13_22_P_Mill)
avg_solar_70_79 <- mean(subset_data$solar_diff70_79_13_22_P_Mill)
avg_thermal_70_79 <- mean(subset_data$thermal_diff70_79_13_22_P_Mill)
avg_sensible_70_79 <- mean(subset_data$sensible_diff70_79_13_22_P_Mill)
avg_evap_70_79 <- mean(subset_data$evap_diff70_79_13_22_P_x10)
avg_press_70_79 <- mean(subset_data$pressure_diff70_79_13_22_P_div10)
avg_precip_70_79 <- mean(subset_data$precip_diff70_79_13_22_P_x10)
avg_transpir_70_79 <- mean(subset_data$transpir_diff70_79_13_22_P_x1000)
avg_downwards_solar_70_79 <- mean(subset_data$downwards_solar_diff70_79_13_22_P_Mill)

# HI change 70-80
chd_hi_70_79 <- lmer(CHD_CrudePrev ~ HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_hi_70_79)
chd_hi_70_79_summary <- summary(chd_hi_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"] * avg_HI_70_79
se_chd_figS7$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_hi_70_79 <- coefficients_chd_figS7$chd_hi_70_79 - 1.96 * se_chd_figS7$chd_hi_70_79
upper_bound_chd_figS7$chd_hi_70_79 <- coefficients_chd_figS7$chd_hi_70_79 + 1.96 * se_chd_figS7$chd_hi_70_79

# Temperature change 70-80
chd_temp_70_79 <- lmer(CHD_CrudePrev ~ Change_Temp_C_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)
summary(chd_temp_70_79)
chd_temp_70_79_summary <- summary(chd_temp_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_temp_70_79 <- coef(chd_temp_70_79_summary)["Change_Temp_C_70_79_13_22", "Estimate"] * avg_temp_70_79
se_chd_figS7$chd_temp_70_79 <- coef(chd_temp_70_79_summary)["Change_Temp_C_70_79_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_temp_70_79 <- coefficients_chd_figS7$chd_temp_70_79 - 1.96 * se_chd_figS7$chd_temp_70_79
upper_bound_chd_figS7$chd_temp_70_79 <- coefficients_chd_figS7$chd_temp_70_79 + 1.96 * se_chd_figS7$chd_temp_70_79

# Relative humidity change 70-80
chd_rh_70_79 <- lmer(CHD_CrudePrev ~ Change_RH_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_rh_70_79)
chd_rh_70_79_summary <- summary(chd_rh_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_rh_70_79 <- coef(chd_rh_70_79_summary)["Change_RH_70_79_13_22", "Estimate"] * avg_rh_70_79
se_chd_figS7$chd_rh_70_79 <- coef(chd_rh_70_79_summary)["Change_RH_70_79_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_rh_70_79 <- coefficients_chd_figS7$chd_rh_70_79 - 1.96 * se_chd_figS7$chd_rh_70_79
upper_bound_chd_figS7$chd_rh_70_79 <- coefficients_chd_figS7$chd_rh_70_79 + 1.96 * se_chd_figS7$chd_rh_70_79

# Wind U change 70-80
chd_windu_70_79 <- lmer(CHD_CrudePrev ~ windU_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windu_70_79)
chd_windu_70_79_summary <- summary(chd_windu_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windu_70_79 <- coef(chd_windu_70_79_summary)["windU_diff70_79_13_22", "Estimate"] * avg_windu_70_79
se_chd_figS7$chd_windu_70_79 <- coef(chd_windu_70_79_summary)["windU_diff70_79_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windu_70_79 <- coefficients_chd_figS7$chd_windu_70_79 - 1.96 * se_chd_figS7$chd_windu_70_79
upper_bound_chd_figS7$chd_windu_70_79 <- coefficients_chd_figS7$chd_windu_70_79 + 1.96 * se_chd_figS7$chd_windu_70_79

# Wind V change 70-80
chd_windv_70_79 <- lmer(CHD_CrudePrev ~ windV_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windv_70_79)
chd_windv_70_79_summary <- summary(chd_windv_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windv_70_79 <- coef(chd_windv_70_79_summary)["windV_diff70_79_13_22", "Estimate"] * avg_windv_70_79
se_chd_figS7$chd_windv_70_79 <- coef(chd_windv_70_79_summary)["windV_diff70_79_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windv_70_79 <- coefficients_chd_figS7$chd_windv_70_79 - 1.96 * se_chd_figS7$chd_windv_70_79
upper_bound_chd_figS7$chd_windv_70_79 <- coefficients_chd_figS7$chd_windv_70_79 + 1.96 * se_chd_figS7$chd_windv_70_79

# Latent change 70-80
chd_latent_70_79 <- lmer(CHD_CrudePrev ~ latent_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_latent_70_79)
chd_latent_70_79_summary <- summary(chd_latent_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_latent_70_79 <- coef(chd_latent_70_79_summary)["latent_diff70_79_13_22_P_Mill", "Estimate"] * avg_latent_70_79
se_chd_figS7$chd_latent_70_79 <- coef(chd_latent_70_79_summary)["latent_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_latent_70_79 <- coefficients_chd_figS7$chd_latent_70_79 - 1.96 * se_chd_figS7$chd_latent_70_79
upper_bound_chd_figS7$chd_latent_70_79 <- coefficients_chd_figS7$chd_latent_70_79 + 1.96 * se_chd_figS7$chd_latent_70_79

# Solar change 70-80
chd_solar_70_79 <- lmer(CHD_CrudePrev ~ solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_solar_70_79)
chd_solar_70_79_summary <- summary(chd_solar_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_solar_70_79 <- coef(chd_solar_70_79_summary)["solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_solar_70_79
se_chd_figS7$chd_solar_70_79 <- coef(chd_solar_70_79_summary)["solar_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_solar_70_79 <- coefficients_chd_figS7$chd_solar_70_79 - 1.96 * se_chd_figS7$chd_solar_70_79
upper_bound_chd_figS7$chd_solar_70_79 <- coefficients_chd_figS7$chd_solar_70_79 + 1.96 * se_chd_figS7$chd_solar_70_79

# Thermal change 70-80
chd_thermal_70_79 <- lmer(CHD_CrudePrev ~ thermal_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                          + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_thermal_70_79)
chd_thermal_70_79_summary <- summary(chd_thermal_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_thermal_70_79 <- coef(chd_thermal_70_79_summary)["thermal_diff70_79_13_22_P_Mill", "Estimate"] * avg_thermal_70_79
se_chd_figS7$chd_thermal_70_79 <- coef(chd_thermal_70_79_summary)["thermal_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_thermal_70_79 <- coefficients_chd_figS7$chd_thermal_70_79 - 1.96 * se_chd_figS7$chd_thermal_70_79
upper_bound_chd_figS7$chd_thermal_70_79 <- coefficients_chd_figS7$chd_thermal_70_79 + 1.96 * se_chd_figS7$chd_thermal_70_79


# Sensible change 70-80
chd_sensible_70_79 <- lmer(CHD_CrudePrev ~ sensible_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_sensible_70_79)
chd_sensible_70_79_summary <- summary(chd_sensible_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_sensible_70_79 <- coef(chd_sensible_70_79_summary)["sensible_diff70_79_13_22_P_Mill", "Estimate"] * avg_sensible_70_79
se_chd_figS7$chd_sensible_70_79 <- coef(chd_sensible_70_79_summary)["sensible_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_sensible_70_79 <- coefficients_chd_figS7$chd_sensible_70_79 - 1.96 * se_chd_figS7$chd_sensible_70_79
upper_bound_chd_figS7$chd_sensible_70_79 <- coefficients_chd_figS7$chd_sensible_70_79 + 1.96 * se_chd_figS7$chd_sensible_70_79

# Evaporation change 70-80
chd_evap_70_79 <- lmer(CHD_CrudePrev ~ evap_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)

summary(chd_evap_70_79)
chd_evap_70_79_summary <- summary(chd_evap_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_evap_70_79 <- coef(chd_evap_70_79_summary)["evap_diff70_79_13_22_P_x10", "Estimate"] * avg_evap_70_79
se_chd_figS7$chd_evap_70_79 <- coef(chd_evap_70_79_summary)["evap_diff70_79_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_evap_70_79 <- coefficients_chd_figS7$chd_evap_70_79 - 1.96 * se_chd_figS7$chd_evap_70_79
upper_bound_chd_figS7$chd_evap_70_79 <- coefficients_chd_figS7$chd_evap_70_79 + 1.96 * se_chd_figS7$chd_evap_70_79

# Surface pressure change 70-80
chd_pressure_70_79 <- lmer(CHD_CrudePrev ~ pressure_diff70_79_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_pressure_70_79)
chd_pressure_70_79_summary <- summary(chd_pressure_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_pressure_70_79 <- coef(chd_pressure_70_79_summary)["pressure_diff70_79_13_22_P_div10", "Estimate"] * avg_press_70_79
se_chd_figS7$chd_pressure_70_79 <- coef(chd_pressure_70_79_summary)["pressure_diff70_79_13_22_P_div10", "Std. Error"]
lower_bound_chd_figS7$chd_pressure_70_79 <- coefficients_chd_figS7$chd_pressure_70_79 - 1.96 * se_chd_figS7$chd_pressure_70_79
upper_bound_chd_figS7$chd_pressure_70_79 <- coefficients_chd_figS7$chd_pressure_70_79 + 1.96 * se_chd_figS7$chd_pressure_70_79

# Precipitation change 70-80
chd_precip_70_79 <- lmer(CHD_CrudePrev ~ precip_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)

summary(chd_precip_70_79)
chd_precip_70_79_summary <- summary(chd_precip_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_precip_70_79 <- coef(chd_precip_70_79_summary)["precip_diff70_79_13_22_P_x10", "Estimate"] * avg_precip_70_79
se_chd_figS7$chd_precip_70_79 <- coef(chd_precip_70_79_summary)["precip_diff70_79_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_precip_70_79 <- coefficients_chd_figS7$chd_precip_70_79 - 1.96 * se_chd_figS7$chd_precip_70_79
upper_bound_chd_figS7$chd_precip_70_79 <- coefficients_chd_figS7$chd_precip_70_79 + 1.96 * se_chd_figS7$chd_precip_70_79

# Transpiration change 70-80
chd_transpir_70_79 <- lmer(CHD_CrudePrev ~ transpir_diff70_79_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_transpir_70_79)
chd_transpir_70_79_summary <- summary(chd_transpir_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_transpir_70_79 <- coef(chd_transpir_70_79_summary)["transpir_diff70_79_13_22_P_x1000", "Estimate"] * avg_transpir_70_79
se_chd_figS7$chd_transpir_70_79 <- coef(chd_transpir_70_79_summary)["transpir_diff70_79_13_22_P_x1000", "Std. Error"]
lower_bound_chd_figS7$chd_transpir_70_79 <- coefficients_chd_figS7$chd_transpir_70_79 - 1.96 * se_chd_figS7$chd_transpir_70_79
upper_bound_chd_figS7$chd_transpir_70_79 <- coefficients_chd_figS7$chd_transpir_70_79 + 1.96 * se_chd_figS7$chd_transpir_70_79

# Downwards solar raidation change 70-89
chd_down_solar_70_79 <- lmer(CHD_CrudePrev ~ downwards_solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                             + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(chd_down_solar_70_79)
chd_down_solar_70_79_summary <- summary(chd_down_solar_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_down_solar_70_79 <- coef(chd_down_solar_70_79_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_downwards_solar_70_79
se_chd_figS7$chd_down_solar_70_79 <- coef(chd_down_solar_70_79_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_down_solar_70_79 <- coefficients_chd_figS7$chd_down_solar_70_79 - 1.96 * se_chd_figS7$chd_down_solar_70_79
upper_bound_chd_figS7$chd_down_solar_70_79 <- coefficients_chd_figS7$chd_down_solar_70_79 + 1.96 * se_chd_figS7$chd_down_solar_70_79


########################### CHD 1980 - 1990 ######################
# average anomaly size 
avg_HI_80_89 <- mean(subset_data$HI_C_Change80_89_13_22)
avg_temp_80_89 <- mean(subset_data$Change_Temp_C_80_89_13_22)
avg_rh_80_89 <- mean(subset_data$Change_RH_80_89_13_22)
avg_windu_80_89 <- mean(subset_data$windU_diff80_89_13_22)
avg_windv_80_89 <- mean(subset_data$windV_diff80_89_13_22)
avg_latent_80_89 <- mean(subset_data$latent_diff80_89_13_22_P_Mill)
avg_solar_80_89 <- mean(subset_data$solar_diff80_89_13_22_P_Mill)
avg_thermal_80_89 <- mean(subset_data$thermal_diff80_89_13_22_P_Mill)
avg_sensible_80_89 <- mean(subset_data$sensible_diff80_89_13_22_P_Mill)
avg_evap_80_89 <- mean(subset_data$evap_diff80_89_13_22_P_x10)
avg_press_80_89 <- mean(subset_data$pressure_diff80_89_13_22_P_div10)
avg_precip_80_89 <- mean(subset_data$precip_diff80_89_13_22_P_x10)
avg_transpir_80_89 <- mean(subset_data$transpir_diff80_89_13_22_P_x1000)
avg_downwards_solar_80_89 <- mean(subset_data$downwards_solar_diff80_89_13_22_P_Mill)

# HI change 80-89
chd_hi_80_89 <- lmer(CHD_CrudePrev ~ HI_C_Change80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_hi_80_89)
chd_hi_80_89_summary <- summary(chd_hi_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_hi_80_89 <- coef(chd_hi_80_89_summary)["HI_C_Change80_89_13_22", "Estimate"] * avg_HI_80_89
se_chd_figS7$chd_hi_80_89 <- coef(chd_hi_80_89_summary)["HI_C_Change80_89_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_hi_80_89 <- coefficients_chd_figS7$chd_hi_80_89 - 1.96 * se_chd_figS7$chd_hi_80_89
upper_bound_chd_figS7$chd_hi_80_89 <- coefficients_chd_figS7$chd_hi_80_89 + 1.96 * se_chd_figS7$chd_hi_80_89

# Temperature change 80-89
chd_temp_80_89 <- lmer(CHD_CrudePrev ~ Change_Temp_C_80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)
summary(chd_temp_80_89)
chd_temp_80_89_summary <- summary(chd_temp_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_temp_80_89 <- coef(chd_temp_80_89_summary)["Change_Temp_C_80_89_13_22", "Estimate"]* avg_temp_80_89
se_chd_figS7$chd_temp_80_89 <- coef(chd_temp_80_89_summary)["Change_Temp_C_80_89_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_temp_80_89 <- coefficients_chd_figS7$chd_temp_80_89 - 1.96 * se_chd_figS7$chd_temp_80_89
upper_bound_chd_figS7$chd_temp_80_89 <- coefficients_chd_figS7$chd_temp_80_89 + 1.96 * se_chd_figS7$chd_temp_80_89

# Relative humidity change 80-89
chd_rh_80_89 <- lmer(CHD_CrudePrev ~ Change_RH_80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_rh_80_89)
chd_rh_80_89_summary <- summary(chd_rh_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_rh_80_89 <- coef(chd_rh_80_89_summary)["Change_RH_80_89_13_22", "Estimate"] * avg_rh_80_89
se_chd_figS7$chd_rh_80_89 <- coef(chd_rh_80_89_summary)["Change_RH_80_89_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_rh_80_89 <- coefficients_chd_figS7$chd_rh_80_89 - 1.96 * se_chd_figS7$chd_rh_80_89
upper_bound_chd_figS7$chd_rh_80_89 <- coefficients_chd_figS7$chd_rh_80_89 + 1.96 * se_chd_figS7$chd_rh_80_89

# Wind U change 80-89
chd_windu_80_89 <- lmer(CHD_CrudePrev ~ windU_diff80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windu_80_89)
chd_windu_80_89_summary <- summary(chd_windu_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windu_80_89 <- coef(chd_windu_80_89_summary)["windU_diff80_89_13_22", "Estimate"] * avg_windu_80_89
se_chd_figS7$chd_windu_80_89 <- coef(chd_windu_80_89_summary)["windU_diff80_89_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windu_80_89 <- coefficients_chd_figS7$chd_windu_80_89 - 1.96 * se_chd_figS7$chd_windu_80_89
upper_bound_chd_figS7$chd_windu_80_89 <- coefficients_chd_figS7$chd_windu_80_89 + 1.96 * se_chd_figS7$chd_windu_80_89

# Wind V change 80-89
chd_windv_80_89 <- lmer(CHD_CrudePrev ~ windV_diff80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windv_80_89)
chd_windv_80_89_summary <- summary(chd_windv_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windv_80_89 <- coef(chd_windv_80_89_summary)["windV_diff80_89_13_22", "Estimate"] * avg_windv_80_89
se_chd_figS7$chd_windv_80_89 <- coef(chd_windv_80_89_summary)["windV_diff80_89_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windv_80_89 <- coefficients_chd_figS7$chd_windv_80_89 - 1.96 * se_chd_figS7$chd_windv_80_89
upper_bound_chd_figS7$chd_windv_80_89 <- coefficients_chd_figS7$chd_windv_80_89 + 1.96 * se_chd_figS7$chd_windv_80_89

# Latent change 80-89
chd_latent_80_89 <- lmer(CHD_CrudePrev ~ latent_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_latent_80_89)
chd_latent_80_89_summary <- summary(chd_latent_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_latent_80_89 <- coef(chd_latent_80_89_summary)["latent_diff80_89_13_22_P_Mill", "Estimate"]* avg_latent_80_89
se_chd_figS7$chd_latent_80_89 <- coef(chd_latent_80_89_summary)["latent_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_latent_80_89 <- coefficients_chd_figS7$chd_latent_80_89 - 1.96 * se_chd_figS7$chd_latent_80_89
upper_bound_chd_figS7$chd_latent_80_89 <- coefficients_chd_figS7$chd_latent_80_89 + 1.96 * se_chd_figS7$chd_latent_80_89

# Solar change 80-89
chd_solar_80_89 <- lmer(CHD_CrudePrev ~ solar_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_solar_80_89)
chd_solar_80_89_summary <- summary(chd_solar_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_solar_80_89 <- coef(chd_solar_80_89_summary)["solar_diff80_89_13_22_P_Mill", "Estimate"] * avg_solar_80_89
se_chd_figS7$chd_solar_80_89 <- coef(chd_solar_80_89_summary)["solar_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_solar_80_89 <- coefficients_chd_figS7$chd_solar_80_89 - 1.96 * se_chd_figS7$chd_solar_80_89
upper_bound_chd_figS7$chd_solar_80_89 <- coefficients_chd_figS7$chd_solar_80_89 + 1.96 * se_chd_figS7$chd_solar_80_89

# Thermal change 80-89
chd_thermal_80_89 <- lmer(CHD_CrudePrev ~ thermal_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                          + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_thermal_80_89)
chd_thermal_80_89_summary <- summary(chd_thermal_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_thermal_80_89 <- coef(chd_thermal_80_89_summary)["thermal_diff80_89_13_22_P_Mill", "Estimate"]* avg_thermal_80_89
se_chd_figS7$chd_thermal_80_89 <- coef(chd_thermal_80_89_summary)["thermal_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_thermal_80_89 <- coefficients_chd_figS7$chd_thermal_80_89 - 1.96 * se_chd_figS7$chd_thermal_80_89
upper_bound_chd_figS7$chd_thermal_80_89 <- coefficients_chd_figS7$chd_thermal_80_89 + 1.96 * se_chd_figS7$chd_thermal_80_89


# Sensible change 80-89
chd_sensible_80_89 <- lmer(CHD_CrudePrev ~ sensible_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_sensible_80_89)
chd_sensible_80_89_summary <- summary(chd_sensible_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_sensible_80_89 <- coef(chd_sensible_80_89_summary)["sensible_diff80_89_13_22_P_Mill", "Estimate"] * avg_sensible_80_89
se_chd_figS7$chd_sensible_80_89 <- coef(chd_sensible_80_89_summary)["sensible_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_sensible_80_89 <- coefficients_chd_figS7$chd_sensible_80_89 - 1.96 * se_chd_figS7$chd_sensible_80_89
upper_bound_chd_figS7$chd_sensible_80_89 <- coefficients_chd_figS7$chd_sensible_80_89 + 1.96 * se_chd_figS7$chd_sensible_80_89

# Evaporation change 80-89
chd_evap_80_89 <- lmer(CHD_CrudePrev ~ evap_diff80_89_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)

summary(chd_evap_80_89)
chd_evap_80_89_summary <- summary(chd_evap_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_evap_80_89 <- coef(chd_evap_80_89_summary)["evap_diff80_89_13_22_P_x10", "Estimate"] * avg_evap_80_89
se_chd_figS7$chd_evap_80_89 <- coef(chd_evap_80_89_summary)["evap_diff80_89_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_evap_80_89 <- coefficients_chd_figS7$chd_evap_80_89 - 1.96 * se_chd_figS7$chd_evap_80_89
upper_bound_chd_figS7$chd_evap_80_89 <- coefficients_chd_figS7$chd_evap_80_89 + 1.96 * se_chd_figS7$chd_evap_80_89

# Surface pressure change 80-89
chd_pressure_80_89 <- lmer(CHD_CrudePrev ~ pressure_diff80_89_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_pressure_80_89)
chd_pressure_80_89_summary <- summary(chd_pressure_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_pressure_80_89 <- coef(chd_pressure_80_89_summary)["pressure_diff80_89_13_22_P_div10", "Estimate"] * avg_press_80_89
se_chd_figS7$chd_pressure_80_89 <- coef(chd_pressure_80_89_summary)["pressure_diff80_89_13_22_P_div10", "Std. Error"]
lower_bound_chd_figS7$chd_pressure_80_89 <- coefficients_chd_figS7$chd_pressure_80_89 - 1.96 * se_chd_figS7$chd_pressure_80_89
upper_bound_chd_figS7$chd_pressure_80_89 <- coefficients_chd_figS7$chd_pressure_80_89 + 1.96 * se_chd_figS7$chd_pressure_80_89

# Precipitation change 80-89
chd_precip_80_89 <- lmer(CHD_CrudePrev ~ precip_diff80_89_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)

summary(chd_precip_80_89)
chd_precip_80_89_summary <- summary(chd_precip_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_precip_80_89 <- coef(chd_precip_80_89_summary)["precip_diff80_89_13_22_P_x10", "Estimate"] * avg_precip_80_89
se_chd_figS7$chd_precip_80_89 <- coef(chd_precip_80_89_summary)["precip_diff80_89_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_precip_80_89 <- coefficients_chd_figS7$chd_precip_80_89 - 1.96 * se_chd_figS7$chd_precip_80_89
upper_bound_chd_figS7$chd_precip_80_89 <- coefficients_chd_figS7$chd_precip_80_89 + 1.96 * se_chd_figS7$chd_precip_80_89

# Transpiration change 80-89
chd_transpir_80_89 <- lmer(CHD_CrudePrev ~ transpir_diff80_89_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_transpir_80_89)
chd_transpir_80_89_summary <- summary(chd_transpir_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_transpir_80_89 <- coef(chd_transpir_80_89_summary)["transpir_diff80_89_13_22_P_x1000", "Estimate"] * avg_transpir_80_89
se_chd_figS7$chd_transpir_80_89 <- coef(chd_transpir_80_89_summary)["transpir_diff80_89_13_22_P_x1000", "Std. Error"]
lower_bound_chd_figS7$chd_transpir_80_89 <- coefficients_chd_figS7$chd_transpir_80_89 - 1.96 * se_chd_figS7$chd_transpir_80_89
upper_bound_chd_figS7$chd_transpir_80_89 <- coefficients_chd_figS7$chd_transpir_80_89 + 1.96 * se_chd_figS7$chd_transpir_80_89

# Downwards solar raidation change 80-89
chd_down_solar_80_89 <- lmer(CHD_CrudePrev ~ downwards_solar_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                             + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(chd_down_solar_80_89)
chd_down_solar_80_89_summary <- summary(chd_down_solar_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_down_solar_80_89 <- coef(chd_down_solar_80_89_summary)["downwards_solar_diff80_89_13_22_P_Mill", "Estimate"] * avg_downwards_solar_80_89
se_chd_figS7$chd_down_solar_80_89 <- coef(chd_down_solar_80_89_summary)["downwards_solar_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_down_solar_80_89 <- coefficients_chd_figS7$chd_down_solar_80_89 - 1.96 * se_chd_figS7$chd_down_solar_80_89
upper_bound_chd_figS7$chd_down_solar_80_89 <- coefficients_chd_figS7$chd_down_solar_80_89 + 1.96 * se_chd_figS7$chd_down_solar_80_89

########################### CHD 1990 - 2000 ###########################################################
# average anomaly size 
avg_HI_90_99 <- mean(subset_data$HI_C_Change90_99_13_22)
avg_temp_90_99 <- mean(subset_data$Change_Temp_C_90_99_13_22)
avg_rh_90_99 <- mean(subset_data$Change_RH_90_99_13_22)
avg_windu_90_99 <- mean(subset_data$windU_diff90_99_13_22)
avg_windv_90_99 <- mean(subset_data$windV_diff90_99_13_22)
avg_latent_90_99 <- mean(subset_data$latent_diff90_99_13_22_P_Mill)
avg_solar_90_99 <- mean(subset_data$solar_diff90_99_13_22_P_Mill)
avg_thermal_90_99 <- mean(subset_data$thermal_diff90_99_13_22_P_Mill)
avg_sensible_90_99 <- mean(subset_data$sensible_diff90_99_13_22_P_Mill)
avg_evap_90_99 <- mean(subset_data$evap_diff90_99_13_22_P_x10)
avg_press_90_99 <- mean(subset_data$pressure_diff90_99_13_22_P_div10)
avg_precip_90_99 <- mean(subset_data$precip_diff90_99_13_22_P_x10)
avg_transpir_90_99 <- mean(subset_data$transpir_diff90_99_13_22_P_x1000)
avg_downwards_solar_90_99 <- mean(subset_data$downwards_solar_diff90_99_13_22_P_Mill)

# HI change 90-99
chd_hi_90_99 <- lmer(CHD_CrudePrev ~ HI_C_Change90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_hi_90_99)
chd_hi_90_99_summary <- summary(chd_hi_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_hi_90_99 <- coef(chd_hi_90_99_summary)["HI_C_Change90_99_13_22", "Estimate"] * avg_HI_90_99
se_chd_figS7$chd_hi_90_99 <- coef(chd_hi_90_99_summary)["HI_C_Change90_99_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_hi_90_99 <- coefficients_chd_figS7$chd_hi_90_99 - 1.96 * se_chd_figS7$chd_hi_90_99
upper_bound_chd_figS7$chd_hi_90_99 <- coefficients_chd_figS7$chd_hi_90_99 + 1.96 * se_chd_figS7$chd_hi_90_99

# Temperature change 90-99
chd_temp_90_99 <- lmer(CHD_CrudePrev ~ Change_Temp_C_90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)
summary(chd_temp_90_99)
chd_temp_90_99_summary <- summary(chd_temp_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_temp_90_99 <- coef(chd_temp_90_99_summary)["Change_Temp_C_90_99_13_22", "Estimate"] * avg_temp_90_99
se_chd_figS7$chd_temp_90_99 <- coef(chd_temp_90_99_summary)["Change_Temp_C_90_99_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_temp_90_99 <- coefficients_chd_figS7$chd_temp_90_99 - 1.96 * se_chd_figS7$chd_temp_90_99
upper_bound_chd_figS7$chd_temp_90_99 <- coefficients_chd_figS7$chd_temp_90_99 + 1.96 * se_chd_figS7$chd_temp_90_99

# Relative humidity change 90-99
chd_rh_90_99 <- lmer(CHD_CrudePrev ~ Change_RH_90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_rh_90_99)
chd_rh_90_99_summary <- summary(chd_rh_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_rh_90_99 <- coef(chd_rh_90_99_summary)["Change_RH_90_99_13_22", "Estimate"] * avg_rh_90_99
se_chd_figS7$chd_rh_90_99 <- coef(chd_rh_90_99_summary)["Change_RH_90_99_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_rh_90_99 <- coefficients_chd_figS7$chd_rh_90_99 - 1.96 * se_chd_figS7$chd_rh_90_99
upper_bound_chd_figS7$chd_rh_90_99 <- coefficients_chd_figS7$chd_rh_90_99 + 1.96 * se_chd_figS7$chd_rh_90_99

# Wind U change 90-99
chd_windu_90_99 <- lmer(CHD_CrudePrev ~ windU_diff90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windu_90_99)
chd_windu_90_99_summary <- summary(chd_windu_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windu_90_99 <- coef(chd_windu_90_99_summary)["windU_diff90_99_13_22", "Estimate"] * avg_windu_90_99
se_chd_figS7$chd_windu_90_99 <- coef(chd_windu_90_99_summary)["windU_diff90_99_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windu_90_99 <- coefficients_chd_figS7$chd_windu_90_99 - 1.96 * se_chd_figS7$chd_windu_90_99
upper_bound_chd_figS7$chd_windu_90_99 <- coefficients_chd_figS7$chd_windu_90_99 + 1.96 * se_chd_figS7$chd_windu_90_99

# Wind V change 90-99
chd_windv_90_99 <- lmer(CHD_CrudePrev ~ windV_diff90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_windv_90_99)
chd_windv_90_99_summary <- summary(chd_windv_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windv_90_99 <- coef(chd_windv_90_99_summary)["windV_diff90_99_13_22", "Estimate"] * avg_windv_90_99
se_chd_figS7$chd_windv_90_99 <- coef(chd_windv_90_99_summary)["windV_diff90_99_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windv_90_99 <- coefficients_chd_figS7$chd_windv_90_99 - 1.96 * se_chd_figS7$chd_windv_90_99
upper_bound_chd_figS7$chd_windv_90_99 <- coefficients_chd_figS7$chd_windv_90_99 + 1.96 * se_chd_figS7$chd_windv_90_99

# Latent change 90-99
chd_latent_90_99 <- lmer(CHD_CrudePrev ~ latent_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_latent_90_99)
chd_latent_90_99_summary <- summary(chd_latent_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_latent_90_99 <- coef(chd_latent_90_99_summary)["latent_diff90_99_13_22_P_Mill", "Estimate"] * avg_latent_90_99
se_chd_figS7$chd_latent_90_99 <- coef(chd_latent_90_99_summary)["latent_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_latent_90_99 <- coefficients_chd_figS7$chd_latent_90_99 - 1.96 * se_chd_figS7$chd_latent_90_99
upper_bound_chd_figS7$chd_latent_90_99 <- coefficients_chd_figS7$chd_latent_90_99 + 1.96 * se_chd_figS7$chd_latent_90_99

# Solar change 90-99
chd_solar_90_99 <- lmer(CHD_CrudePrev ~ solar_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_solar_90_99)
chd_solar_90_99_summary <- summary(chd_solar_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_solar_90_99 <- coef(chd_solar_90_99_summary)["solar_diff90_99_13_22_P_Mill", "Estimate"] * avg_solar_90_99
se_chd_figS7$chd_solar_90_99 <- coef(chd_solar_90_99_summary)["solar_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_solar_90_99 <- coefficients_chd_figS7$chd_solar_90_99 - 1.96 * se_chd_figS7$chd_solar_90_99
upper_bound_chd_figS7$chd_solar_90_99 <- coefficients_chd_figS7$chd_solar_90_99 + 1.96 * se_chd_figS7$chd_solar_90_99

# Thermal change 90-99
chd_thermal_90_99 <- lmer(CHD_CrudePrev ~ thermal_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                          + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_thermal_90_99)
chd_thermal_90_99_summary <- summary(chd_thermal_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_thermal_90_99 <- coef(chd_thermal_90_99_summary)["thermal_diff90_99_13_22_P_Mill", "Estimate"] * avg_thermal_90_99
se_chd_figS7$chd_thermal_90_99 <- coef(chd_thermal_90_99_summary)["thermal_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_thermal_90_99 <- coefficients_chd_figS7$chd_thermal_90_99 - 1.96 * se_chd_figS7$chd_thermal_90_99
upper_bound_chd_figS7$chd_thermal_90_99 <- coefficients_chd_figS7$chd_thermal_90_99 + 1.96 * se_chd_figS7$chd_thermal_90_99


# Sensible change 90-99
chd_sensible_90_99 <- lmer(CHD_CrudePrev ~ sensible_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_sensible_90_99)
chd_sensible_90_99_summary <- summary(chd_sensible_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_sensible_90_99 <- coef(chd_sensible_90_99_summary)["sensible_diff90_99_13_22_P_Mill", "Estimate"]* avg_sensible_90_99
se_chd_figS7$chd_sensible_90_99 <- coef(chd_sensible_90_99_summary)["sensible_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_sensible_90_99 <- coefficients_chd_figS7$chd_sensible_90_99 - 1.96 * se_chd_figS7$chd_sensible_90_99
upper_bound_chd_figS7$chd_sensible_90_99 <- coefficients_chd_figS7$chd_sensible_90_99 + 1.96 * se_chd_figS7$chd_sensible_90_99

# Evaporation change 90-99
chd_evap_90_99 <- lmer(CHD_CrudePrev ~ evap_diff90_99_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)

summary(chd_evap_90_99)
chd_evap_90_99_summary <- summary(chd_evap_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_evap_90_99 <- coef(chd_evap_90_99_summary)["evap_diff90_99_13_22_P_x10", "Estimate"] * avg_evap_90_99
se_chd_figS7$chd_evap_90_99 <- coef(chd_evap_90_99_summary)["evap_diff90_99_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_evap_90_99 <- coefficients_chd_figS7$chd_evap_90_99 - 1.96 * se_chd_figS7$chd_evap_90_99
upper_bound_chd_figS7$chd_evap_90_99 <- coefficients_chd_figS7$chd_evap_90_99 + 1.96 * se_chd_figS7$chd_evap_90_99

# Surface pressure change 90-99
chd_pressure_90_99 <- lmer(CHD_CrudePrev ~ pressure_diff90_99_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_pressure_90_99)
chd_pressure_90_99_summary <- summary(chd_pressure_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_pressure_90_99 <- coef(chd_pressure_90_99_summary)["pressure_diff90_99_13_22_P_div10", "Estimate"] * avg_press_90_99
se_chd_figS7$chd_pressure_90_99 <- coef(chd_pressure_90_99_summary)["pressure_diff90_99_13_22_P_div10", "Std. Error"]
lower_bound_chd_figS7$chd_pressure_90_99 <- coefficients_chd_figS7$chd_pressure_90_99 - 1.96 * se_chd_figS7$chd_pressure_90_99
upper_bound_chd_figS7$chd_pressure_90_99 <- coefficients_chd_figS7$chd_pressure_90_99 + 1.96 * se_chd_figS7$chd_pressure_90_99

# Precipitation change 90-99
chd_precip_90_99 <- lmer(CHD_CrudePrev ~ precip_diff90_99_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)

summary(chd_precip_90_99)
chd_precip_90_99_summary <- summary(chd_precip_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_precip_90_99 <- coef(chd_precip_90_99_summary)["precip_diff90_99_13_22_P_x10", "Estimate"]* avg_precip_90_99
se_chd_figS7$chd_precip_90_99 <- coef(chd_precip_90_99_summary)["precip_diff90_99_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_precip_90_99 <- coefficients_chd_figS7$chd_precip_90_99 - 1.96 * se_chd_figS7$chd_precip_90_99
upper_bound_chd_figS7$chd_precip_90_99 <- coefficients_chd_figS7$chd_precip_90_99 + 1.96 * se_chd_figS7$chd_precip_90_99

# Transpiration change 90-99
chd_transpir_90_99 <- lmer(CHD_CrudePrev ~ transpir_diff90_99_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_transpir_90_99)
chd_transpir_90_99_summary <- summary(chd_transpir_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_transpir_90_99 <- coef(chd_transpir_90_99_summary)["transpir_diff90_99_13_22_P_x1000", "Estimate"] * avg_transpir_90_99
se_chd_figS7$chd_transpir_90_99 <- coef(chd_transpir_90_99_summary)["transpir_diff90_99_13_22_P_x1000", "Std. Error"]
lower_bound_chd_figS7$chd_transpir_90_99 <- coefficients_chd_figS7$chd_transpir_90_99 - 1.96 * se_chd_figS7$chd_transpir_90_99
upper_bound_chd_figS7$chd_transpir_90_99 <- coefficients_chd_figS7$chd_transpir_90_99 + 1.96 * se_chd_figS7$chd_transpir_90_99

# Downwards solar radiation change 90-99
chd_down_solar_90_99 <- lmer(CHD_CrudePrev ~ downwards_solar_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                             + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(chd_down_solar_90_99)
chd_down_solar_90_99_summary <- summary(chd_down_solar_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_down_solar_90_99 <- coef(chd_down_solar_90_99_summary)["downwards_solar_diff90_99_13_22_P_Mill", "Estimate"] * avg_downwards_solar_90_99
se_chd_figS7$chd_down_solar_90_99 <- coef(chd_down_solar_90_99_summary)["downwards_solar_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_down_solar_90_99 <- coefficients_chd_figS7$chd_down_solar_90_99 - 1.96 * se_chd_figS7$chd_down_solar_90_99
upper_bound_chd_figS7$chd_down_solar_90_99 <- coefficients_chd_figS7$chd_down_solar_90_99 + 1.96 * se_chd_figS7$chd_down_solar_90_99


########################### CHD 2000 - 2010 ############################################################
# average anomaly size 
avg_HI_2000_2009 <- mean(subset_data$HI_C_Change2000_2009_13_22)
avg_temp_2000_2009 <- mean(subset_data$Change_Temp_C_2000_2009_13_22)
avg_rh_2000_2009 <- mean(subset_data$Change_RH_2000_2009_13_22)
avg_windu_2000_2009 <- mean(subset_data$windU_diff2000_2009_13_22)
avg_windv_2000_2009 <- mean(subset_data$windV_diff2000_2009_13_22)
avg_latent_2000_2009 <- mean(subset_data$latent_diff2000_2009_13_22_P_Mill)
avg_solar_2000_2009 <- mean(subset_data$solar_diff2000_2009_13_22_P_Mill)
avg_thermal_2000_2009 <- mean(subset_data$thermal_diff2000_2009_13_22_P_Mill)
avg_sensible_2000_2009 <- mean(subset_data$sensible_diff2000_2009_13_22_P_Mill)
avg_evap_2000_2009 <- mean(subset_data$evap_diff2000_2009_13_22_P_x10)
avg_press_2000_2009 <- mean(subset_data$pressure_diff2000_2009_13_22_P_div10)
avg_precip_2000_2009 <- mean(subset_data$precip_diff2000_2009_13_22_P_x10)
avg_transpir_2000_2009 <- mean(subset_data$transpir_diff2000_2009_13_22_P_x1000)
avg_downwards_solar_2000_2009 <- mean(subset_data$downwards_solar_diff2000_2009_13_22_P_Mill)

# HI change 2000-2009
chd_hi_2000_2009 <- lmer(CHD_CrudePrev ~ HI_C_Change2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_hi_2000_2009)
chd_hi_2000_2009_summary <- summary(chd_hi_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_hi_2000_2009 <- coef(chd_hi_2000_2009_summary)["HI_C_Change2000_2009_13_22", "Estimate"]* avg_HI_2000_2009
se_chd_figS7$chd_hi_2000_2009 <- coef(chd_hi_2000_2009_summary)["HI_C_Change2000_2009_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_hi_2000_2009 <- coefficients_chd_figS7$chd_hi_2000_2009 - 1.96 * se_chd_figS7$chd_hi_2000_2009
upper_bound_chd_figS7$chd_hi_2000_2009 <- coefficients_chd_figS7$chd_hi_2000_2009 + 1.96 * se_chd_figS7$chd_hi_2000_2009

# Temperature change 2000-2009
chd_temp_2000_2009 <- lmer(CHD_CrudePrev ~ Change_Temp_C_2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_temp_2000_2009)
chd_temp_2000_2009_summary <- summary(chd_temp_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_temp_2000_2009 <- coef(chd_temp_2000_2009_summary)["Change_Temp_C_2000_2009_13_22", "Estimate"] * avg_temp_2000_2009
se_chd_figS7$chd_temp_2000_2009 <- coef(chd_temp_2000_2009_summary)["Change_Temp_C_2000_2009_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_temp_2000_2009 <- coefficients_chd_figS7$chd_temp_2000_2009 - 1.96 * se_chd_figS7$chd_temp_2000_2009
upper_bound_chd_figS7$chd_temp_2000_2009 <- coefficients_chd_figS7$chd_temp_2000_2009 + 1.96 * se_chd_figS7$chd_temp_2000_2009

# Relative humidity change 2000-2009
chd_rh_2000_2009 <- lmer(CHD_CrudePrev ~ Change_RH_2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                         + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_rh_2000_2009)
chd_rh_2000_2009_summary <- summary(chd_rh_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_rh_2000_2009 <- coef(chd_rh_2000_2009_summary)["Change_RH_2000_2009_13_22", "Estimate"] * avg_rh_2000_2009
se_chd_figS7$chd_rh_2000_2009 <- coef(chd_rh_2000_2009_summary)["Change_RH_2000_2009_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_rh_2000_2009 <- coefficients_chd_figS7$chd_rh_2000_2009 - 1.96 * se_chd_figS7$chd_rh_2000_2009
upper_bound_chd_figS7$chd_rh_2000_2009 <- coefficients_chd_figS7$chd_rh_2000_2009 + 1.96 * se_chd_figS7$chd_rh_2000_2009

# Wind U change 2000-2009
chd_windu_2000_2009 <- lmer(CHD_CrudePrev ~ windU_diff2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                            + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_windu_2000_2009)
chd_windu_2000_2009_summary <- summary(chd_windu_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windu_2000_2009 <- coef(chd_windu_2000_2009_summary)["windU_diff2000_2009_13_22", "Estimate"] * avg_windu_2000_2009
se_chd_figS7$chd_windu_2000_2009 <- coef(chd_windu_2000_2009_summary)["windU_diff2000_2009_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windu_2000_2009 <- coefficients_chd_figS7$chd_windu_2000_2009 - 1.96 * se_chd_figS7$chd_windu_2000_2009
upper_bound_chd_figS7$chd_windu_2000_2009 <- coefficients_chd_figS7$chd_windu_2000_2009 + 1.96 * se_chd_figS7$chd_windu_2000_2009

# Wind V change 2000-2009
chd_windv_2000_2009 <- lmer(CHD_CrudePrev ~ windV_diff2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                            + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_windv_2000_2009)
chd_windv_2000_2009_summary <- summary(chd_windv_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_windv_2000_2009 <- coef(chd_windv_2000_2009_summary)["windV_diff2000_2009_13_22", "Estimate"] * avg_windv_2000_2009
se_chd_figS7$chd_windv_2000_2009 <- coef(chd_windv_2000_2009_summary)["windV_diff2000_2009_13_22", "Std. Error"]
lower_bound_chd_figS7$chd_windv_2000_2009 <- coefficients_chd_figS7$chd_windv_2000_2009 - 1.96 * se_chd_figS7$chd_windv_2000_2009
upper_bound_chd_figS7$chd_windv_2000_2009 <- coefficients_chd_figS7$chd_windv_2000_2009 + 1.96 * se_chd_figS7$chd_windv_2000_2009

# Latent change 2000-2009
chd_latent_2000_2009 <- lmer(CHD_CrudePrev ~ latent_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                             + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(chd_latent_2000_2009)
chd_latent_2000_2009_summary <- summary(chd_latent_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_latent_2000_2009 <- coef(chd_latent_2000_2009_summary)["latent_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_latent_2000_2009
se_chd_figS7$chd_latent_2000_2009 <- coef(chd_latent_2000_2009_summary)["latent_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_latent_2000_2009 <- coefficients_chd_figS7$chd_latent_2000_2009 - 1.96 * se_chd_figS7$chd_latent_2000_2009
upper_bound_chd_figS7$chd_latent_2000_2009 <- coefficients_chd_figS7$chd_latent_2000_2009 + 1.96 * se_chd_figS7$chd_latent_2000_2009

# Solar change 2000-2009
chd_solar_2000_2009 <- lmer(CHD_CrudePrev ~ solar_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                            + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_solar_2000_2009)
chd_solar_2000_2009_summary <- summary(chd_solar_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_solar_2000_2009 <- coef(chd_solar_2000_2009_summary)["solar_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_solar_2000_2009
se_chd_figS7$chd_solar_2000_2009 <- coef(chd_solar_2000_2009_summary)["solar_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_solar_2000_2009 <- coefficients_chd_figS7$chd_solar_2000_2009 - 1.96 * se_chd_figS7$chd_solar_2000_2009
upper_bound_chd_figS7$chd_solar_2000_2009 <- coefficients_chd_figS7$chd_solar_2000_2009 + 1.96 * se_chd_figS7$chd_solar_2000_2009

# Thermal change 2000-2009
chd_thermal_2000_2009 <- lmer(CHD_CrudePrev ~ thermal_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                              + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)
summary(chd_thermal_2000_2009)
chd_thermal_2000_2009_summary <- summary(chd_thermal_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_thermal_2000_2009 <- coef(chd_thermal_2000_2009_summary)["thermal_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_thermal_2000_2009
se_chd_figS7$chd_thermal_2000_2009 <- coef(chd_thermal_2000_2009_summary)["thermal_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_thermal_2000_2009 <- coefficients_chd_figS7$chd_thermal_2000_2009 - 1.96 * se_chd_figS7$chd_thermal_2000_2009
upper_bound_chd_figS7$chd_thermal_2000_2009 <- coefficients_chd_figS7$chd_thermal_2000_2009 + 1.96 * se_chd_figS7$chd_thermal_2000_2009


# Sensible change 2000-2009
chd_sensible_2000_2009 <- lmer(CHD_CrudePrev ~ sensible_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                               + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)

summary(chd_sensible_2000_2009)
chd_sensible_2000_2009_summary <- summary(chd_sensible_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_sensible_2000_2009 <- coef(chd_sensible_2000_2009_summary)["sensible_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_sensible_2000_2009
se_chd_figS7$chd_sensible_2000_2009 <- coef(chd_sensible_2000_2009_summary)["sensible_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_sensible_2000_2009 <- coefficients_chd_figS7$chd_sensible_2000_2009 - 1.96 * se_chd_figS7$chd_sensible_2000_2009
upper_bound_chd_figS7$chd_sensible_2000_2009 <- coefficients_chd_figS7$chd_sensible_2000_2009 + 1.96 * se_chd_figS7$chd_sensible_2000_2009

# Evaporation change 2000-2009
chd_evap_2000_2009 <- lmer(CHD_CrudePrev ~ evap_diff2000_2009_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                           + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)

summary(chd_evap_2000_2009)
chd_evap_2000_2009_summary <- summary(chd_evap_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_evap_2000_2009 <- coef(chd_evap_2000_2009_summary)["evap_diff2000_2009_13_22_P_x10", "Estimate"] * avg_evap_2000_2009
se_chd_figS7$chd_evap_2000_2009 <- coef(chd_evap_2000_2009_summary)["evap_diff2000_2009_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_evap_2000_2009 <- coefficients_chd_figS7$chd_evap_2000_2009 - 1.96 * se_chd_figS7$chd_evap_2000_2009
upper_bound_chd_figS7$chd_evap_2000_2009 <- coefficients_chd_figS7$chd_evap_2000_2009 + 1.96 * se_chd_figS7$chd_evap_2000_2009

# Surface pressure change 2000-2009
chd_pressure_2000_2009 <- lmer(CHD_CrudePrev ~ pressure_diff2000_2009_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                               + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)

summary(chd_pressure_2000_2009)
chd_pressure_2000_2009_summary <- summary(chd_pressure_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_pressure_2000_2009 <- coef(chd_pressure_2000_2009_summary)["pressure_diff2000_2009_13_22_P_div10", "Estimate"] * avg_press_2000_2009
se_chd_figS7$chd_pressure_2000_2009 <- coef(chd_pressure_2000_2009_summary)["pressure_diff2000_2009_13_22_P_div10", "Std. Error"]
lower_bound_chd_figS7$chd_pressure_2000_2009 <- coefficients_chd_figS7$chd_pressure_2000_2009 - 1.96 * se_chd_figS7$chd_pressure_2000_2009
upper_bound_chd_figS7$chd_pressure_2000_2009 <- coefficients_chd_figS7$chd_pressure_2000_2009 + 1.96 * se_chd_figS7$chd_pressure_2000_2009

# Precipitation change 2000-2009
chd_precip_2000_2009 <- lmer(CHD_CrudePrev ~ precip_diff2000_2009_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                             + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)

summary(chd_precip_2000_2009)
chd_precip_2000_2009_summary <- summary(chd_precip_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_precip_2000_2009 <- coef(chd_precip_2000_2009_summary)["precip_diff2000_2009_13_22_P_x10", "Estimate"] * avg_precip_2000_2009
se_chd_figS7$chd_precip_2000_2009 <- coef(chd_precip_2000_2009_summary)["precip_diff2000_2009_13_22_P_x10", "Std. Error"]
lower_bound_chd_figS7$chd_precip_2000_2009 <- coefficients_chd_figS7$chd_precip_2000_2009 - 1.96 * se_chd_figS7$chd_precip_2000_2009
upper_bound_chd_figS7$chd_precip_2000_2009 <- coefficients_chd_figS7$chd_precip_2000_2009 + 1.96 * se_chd_figS7$chd_precip_2000_2009

# Transpiration change 2000-2009
chd_transpir_2000_2009 <- lmer(CHD_CrudePrev ~ transpir_diff2000_2009_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                               + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(chd_transpir_2000_2009)
chd_transpir_2000_2009_summary <- summary(chd_transpir_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_transpir_2000_2009 <- coef(chd_transpir_2000_2009_summary)["transpir_diff2000_2009_13_22_P_x1000", "Estimate"] * avg_transpir_2000_2009
se_chd_figS7$chd_transpir_2000_2009 <- coef(chd_transpir_2000_2009_summary)["transpir_diff2000_2009_13_22_P_x1000", "Std. Error"]
lower_bound_chd_figS7$chd_transpir_2000_2009 <- coefficients_chd_figS7$chd_transpir_2000_2009 - 1.96 * se_chd_figS7$chd_transpir_2000_2009
upper_bound_chd_figS7$chd_transpir_2000_2009 <- coefficients_chd_figS7$chd_transpir_2000_2009 + 1.96 * se_chd_figS7$chd_transpir_2000_2009

# Downwards solar raidation change 2000-2009
chd_down_solar_2000_2009 <- lmer(CHD_CrudePrev ~ downwards_solar_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces 
                                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                                 + Temp_C_2013_2022 + LCchangeMEAN +
                                   (1 | CountyFIPS), data = subset_data)
summary(chd_down_solar_2000_2009)
chd_down_solar_2000_2009_summary <- summary(chd_down_solar_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_figS7$chd_down_solar_2000_2009 <- coef(chd_down_solar_2000_2009_summary)["downwards_solar_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_downwards_solar_2000_2009
se_chd_figS7$chd_down_solar_2000_2009 <- coef(chd_down_solar_2000_2009_summary)["downwards_solar_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_chd_figS7$chd_down_solar_2000_2009 <- coefficients_chd_figS7$chd_down_solar_2000_2009 - 1.96 * se_chd_figS7$chd_down_solar_2000_2009
upper_bound_chd_figS7$chd_down_solar_2000_2009 <- coefficients_chd_figS7$chd_down_solar_2000_2009 + 1.96 * se_chd_figS7$chd_down_solar_2000_2009


########################### Create CHD data frame #####################
# Define the variables
variables <- c("Heat Index", "Air Temperature", "Humidity", "Eastward Wind", "Northward Wind", "Latent Heat Flux",
               "Absorbed Sunlight", "Thermal Radiation", "Sensible Heat Flux", "Evaporation", "Surface Pressure",
               "Precipitation", "Transpiration", "Sunlight")

# Create a data frame for the forest plot
chd_figS7 <- data.frame(
  Variable = rep(variables, times = 4),  # Repeat variables for each time period
  Time_Period = rep(c("1970-1979", "1980-1989", "1990-1999", "2000-2009"), each = length(variables)),
  Effect_Anomaly_Size = unlist(coefficients_chd_figS7),  # Unlist coefficients from your data
  SE = unlist(se_chd_figS7),  # Unlist standard errors from your data
  Lower_CI = unlist(lower_bound_chd_figS7),  # Unlist lower bounds from your data
  Upper_CI = unlist(upper_bound_chd_figS7)  # Unlist upper bounds from your data
)

# Convert Time_Period to factor to ensure correct order in facet_grid
chd_figS7$Time_Period <- factor(chd_figS7$Time_Period, levels = c("1970-1979", "1980-1989", "1990-1999", "2000-2009"))

##########################################################################################################################
################################################ Stroke Figure S7 #################################################
##########################################################################################################################
########################### Stroke Lists for Figure S7 ###########################
# create empty list to store results from linear models
coefficients_stroke_figS7 <- list()
pvalue_stroke_figS7 <- list()
se_stroke_figS7 <- list()
lower_bound_stroke_figS7 <- list()
upper_bound_stroke_figS7 <- list()

# average STROKE
avg_STROKE <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

########################### Stroke 1970-1980 #############################
# average anomaly size
avg_HI_70_79 <- mean(subset_data$HI_C_Change70_79_13_22)
avg_temp_70_79 <- mean(subset_data$Change_Temp_C_70_79_13_22)
avg_rh_70_79 <- mean(subset_data$Change_RH_70_79_13_22)
avg_windu_70_79 <- mean(subset_data$windU_diff70_79_13_22)
avg_windv_70_79 <- mean(subset_data$windV_diff70_79_13_22)
avg_latent_70_79 <- mean(subset_data$latent_diff70_79_13_22_P_Mill)
avg_solar_70_79 <- mean(subset_data$solar_diff70_79_13_22_P_Mill)
avg_thermal_70_79 <- mean(subset_data$thermal_diff70_79_13_22_P_Mill)
avg_sensible_70_79 <- mean(subset_data$sensible_diff70_79_13_22_P_Mill)
avg_evap_70_79 <- mean(subset_data$evap_diff70_79_13_22_P_x10)
avg_press_70_79 <- mean(subset_data$pressure_diff70_79_13_22_P_div10)
avg_precip_70_79 <- mean(subset_data$precip_diff70_79_13_22_P_x10)
avg_transpir_70_79 <- mean(subset_data$transpir_diff70_79_13_22_P_x1000)
avg_downwards_solar_70_79 <- mean(subset_data$downwards_solar_diff70_79_13_22_P_Mill)

# HI change 70-79
stroke_hi_70_79 <- lmer(STROKE_CrudePrev ~ HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_70_79)
stroke_hi_70_79_summary <- summary(stroke_hi_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"] * avg_HI_70_79
se_stroke_figS7$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_hi_70_79 <- coefficients_stroke_figS7$stroke_hi_70_79 - 1.96 * se_stroke_figS7$stroke_hi_70_79
upper_bound_stroke_figS7$stroke_hi_70_79 <- coefficients_stroke_figS7$stroke_hi_70_79 + 1.96 * se_stroke_figS7$stroke_hi_70_79

# Temperature change 70-79
stroke_temp_70_79 <- lmer(STROKE_CrudePrev ~ Change_Temp_C_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(stroke_temp_70_79)
stroke_temp_70_79_summary <- summary(stroke_temp_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_temp_70_79 <- coef(stroke_temp_70_79_summary)["Change_Temp_C_70_79_13_22", "Estimate"] * avg_temp_70_79
se_stroke_figS7$stroke_temp_70_79 <- coef(stroke_temp_70_79_summary)["Change_Temp_C_70_79_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_temp_70_79 <- coefficients_stroke_figS7$stroke_temp_70_79 - 1.96 * se_stroke_figS7$stroke_temp_70_79
upper_bound_stroke_figS7$stroke_temp_70_79 <- coefficients_stroke_figS7$stroke_temp_70_79 + 1.96 * se_stroke_figS7$stroke_temp_70_79

# Relative humidity change 70-79
stroke_rh_70_79 <- lmer(STROKE_CrudePrev ~ Change_RH_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_rh_70_79)
stroke_rh_70_79_summary <- summary(stroke_rh_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_rh_70_79 <- coef(stroke_rh_70_79_summary)["Change_RH_70_79_13_22", "Estimate"] * avg_rh_70_79
se_stroke_figS7$stroke_rh_70_79 <- coef(stroke_rh_70_79_summary)["Change_RH_70_79_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_rh_70_79 <- coefficients_stroke_figS7$stroke_rh_70_79 - 1.96 * se_stroke_figS7$stroke_rh_70_79
upper_bound_stroke_figS7$stroke_rh_70_79 <- coefficients_stroke_figS7$stroke_rh_70_79 + 1.96 * se_stroke_figS7$stroke_rh_70_79

# Wind U change 70-79
stroke_windu_70_79 <- lmer(STROKE_CrudePrev ~ windU_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windu_70_79)
stroke_windu_70_79_summary <- summary(stroke_windu_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windu_70_79 <- coef(stroke_windu_70_79_summary)["windU_diff70_79_13_22", "Estimate"] * avg_windu_70_79
se_stroke_figS7$stroke_windu_70_79 <- coef(stroke_windu_70_79_summary)["windU_diff70_79_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windu_70_79 <- coefficients_stroke_figS7$stroke_windu_70_79 - 1.96 * se_stroke_figS7$stroke_windu_70_79
upper_bound_stroke_figS7$stroke_windu_70_79 <- coefficients_stroke_figS7$stroke_windu_70_79 + 1.96 * se_stroke_figS7$stroke_windu_70_79

# Wind V change 70-79
stroke_windv_70_79 <- lmer(STROKE_CrudePrev ~ windV_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windv_70_79)
stroke_windv_70_79_summary <- summary(stroke_windv_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windv_70_79 <- coef(stroke_windv_70_79_summary)["windV_diff70_79_13_22", "Estimate"] * avg_windv_70_79
se_stroke_figS7$stroke_windv_70_79 <- coef(stroke_windv_70_79_summary)["windV_diff70_79_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windv_70_79 <- coefficients_stroke_figS7$stroke_windv_70_79 - 1.96 * se_stroke_figS7$stroke_windv_70_79
upper_bound_stroke_figS7$stroke_windv_70_79 <- coefficients_stroke_figS7$stroke_windv_70_79 + 1.96 * se_stroke_figS7$stroke_windv_70_79

# Latent change 70-79
stroke_latent_70_79 <- lmer(STROKE_CrudePrev ~ latent_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_latent_70_79)
stroke_latent_70_79_summary <- summary(stroke_latent_70_79)
# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_latent_70_79 <- coef(stroke_latent_70_79_summary)["latent_diff70_79_13_22_P_Mill", "Estimate"] * avg_latent_70_79
se_stroke_figS7$stroke_latent_70_79 <- coef(stroke_latent_70_79_summary)["latent_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_latent_70_79 <- coefficients_stroke_figS7$stroke_latent_70_79 - 1.96 * se_stroke_figS7$stroke_latent_70_79
upper_bound_stroke_figS7$stroke_latent_70_79 <- coefficients_stroke_figS7$stroke_latent_70_79 + 1.96 * se_stroke_figS7$stroke_latent_70_79

# Solar change 70-79
stroke_solar_70_79 <- lmer(STROKE_CrudePrev ~ solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_solar_70_79)
stroke_solar_70_79_summary <- summary(stroke_solar_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_solar_70_79 <- coef(stroke_solar_70_79_summary)["solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_solar_70_79
se_stroke_figS7$stroke_solar_70_79 <- coef(stroke_solar_70_79_summary)["solar_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_solar_70_79 <- coefficients_stroke_figS7$stroke_solar_70_79 - 1.96 * se_stroke_figS7$stroke_solar_70_79
upper_bound_stroke_figS7$stroke_solar_70_79 <- coefficients_stroke_figS7$stroke_solar_70_79 + 1.96 * se_stroke_figS7$stroke_solar_70_79

# Thermal change 70-79
stroke_thermal_70_79 <- lmer(STROKE_CrudePrev ~ thermal_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                               + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_thermal_70_79)
stroke_thermal_70_79_summary <- summary(stroke_thermal_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_thermal_70_79 <- coef(stroke_thermal_70_79_summary)["thermal_diff70_79_13_22_P_Mill", "Estimate"] * avg_thermal_70_79
se_stroke_figS7$stroke_thermal_70_79 <- coef(stroke_thermal_70_79_summary)["thermal_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_thermal_70_79 <- coefficients_stroke_figS7$stroke_thermal_70_79 - 1.96 * se_stroke_figS7$stroke_thermal_70_79
upper_bound_stroke_figS7$stroke_thermal_70_79 <- coefficients_stroke_figS7$stroke_thermal_70_79 + 1.96 * se_stroke_figS7$stroke_thermal_70_79

# Sensible heat change 70-79
stroke_sensible_70_79 <- lmer(STROKE_CrudePrev ~ sensible_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_sensible_70_79)
stroke_sensible_70_79_summary <- summary(stroke_sensible_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_sensible_70_79 <- coef(stroke_sensible_70_79_summary)["sensible_diff70_79_13_22_P_Mill", "Estimate"] * avg_sensible_70_79
se_stroke_figS7$stroke_sensible_70_79 <- coef(stroke_sensible_70_79_summary)["sensible_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_sensible_70_79 <- coefficients_stroke_figS7$stroke_sensible_70_79 - 1.96 * se_stroke_figS7$stroke_sensible_70_79
upper_bound_stroke_figS7$stroke_sensible_70_79 <- coefficients_stroke_figS7$stroke_sensible_70_79 + 1.96 * se_stroke_figS7$stroke_sensible_70_79

# Evaporation change 70-79
stroke_evap_70_79 <- lmer(STROKE_CrudePrev ~ evap_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)

summary(stroke_evap_70_79)
stroke_evap_70_79_summary <- summary(stroke_evap_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_evap_70_79 <- coef(stroke_evap_70_79_summary)["evap_diff70_79_13_22_P_x10", "Estimate"] * avg_evap_70_79
se_stroke_figS7$stroke_evap_70_79 <- coef(stroke_evap_70_79_summary)["evap_diff70_79_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_evap_70_79 <- coefficients_stroke_figS7$stroke_evap_70_79 - 1.96 * se_stroke_figS7$stroke_evap_70_79
upper_bound_stroke_figS7$stroke_evap_70_79 <- coefficients_stroke_figS7$stroke_evap_70_79 + 1.96 * se_stroke_figS7$stroke_evap_70_79

# Surface pressure change 70-79
stroke_pressure_70_79 <- lmer(STROKE_CrudePrev ~ pressure_diff70_79_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_pressure_70_79)
stroke_pressure_70_79_summary <- summary(stroke_pressure_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_pressure_70_79 <- coef(stroke_pressure_70_79_summary)["pressure_diff70_79_13_22_P_div10", "Estimate"] * avg_press_70_79
se_stroke_figS7$stroke_pressure_70_79 <- coef(stroke_pressure_70_79_summary)["pressure_diff70_79_13_22_P_div10", "Std. Error"]
lower_bound_stroke_figS7$stroke_pressure_70_79 <- coefficients_stroke_figS7$stroke_pressure_70_79 - 1.96 * se_stroke_figS7$stroke_pressure_70_79
upper_bound_stroke_figS7$stroke_pressure_70_79 <- coefficients_stroke_figS7$stroke_pressure_70_79 + 1.96 * se_stroke_figS7$stroke_pressure_70_79

# Precipitation change 70-79 (stroke analysis)
stroke_precip_70_79 <- lmer(STROKE_CrudePrev ~ precip_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)

summary(stroke_precip_70_79)
stroke_precip_70_79_summary <- summary(stroke_precip_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_precip_70_79 <- coef(stroke_precip_70_79_summary)["precip_diff70_79_13_22_P_x10", "Estimate"] * avg_precip_70_79
se_stroke_figS7$stroke_precip_70_79 <- coef(stroke_precip_70_79_summary)["precip_diff70_79_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_precip_70_79 <- coefficients_stroke_figS7$stroke_precip_70_79 - 1.96 * se_stroke_figS7$stroke_precip_70_79
upper_bound_stroke_figS7$stroke_precip_70_79 <- coefficients_stroke_figS7$stroke_precip_70_79 + 1.96 * se_stroke_figS7$stroke_precip_70_79

# Transpiration change 70-79 (stroke analysis)
stroke_transpir_70_79 <- lmer(STROKE_CrudePrev ~ transpir_diff70_79_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_transpir_70_79)
stroke_transpir_70_79_summary <- summary(stroke_transpir_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_transpir_70_79 <- coef(stroke_transpir_70_79_summary)["transpir_diff70_79_13_22_P_x1000", "Estimate"] * avg_transpir_70_79
se_stroke_figS7$stroke_transpir_70_79 <- coef(stroke_transpir_70_79_summary)["transpir_diff70_79_13_22_P_x1000", "Std. Error"]
lower_bound_stroke_figS7$stroke_transpir_70_79 <- coefficients_stroke_figS7$stroke_transpir_70_79 - 1.96 * se_stroke_figS7$stroke_transpir_70_79
upper_bound_stroke_figS7$stroke_transpir_70_79 <- coefficients_stroke_figS7$stroke_transpir_70_79 + 1.96 * se_stroke_figS7$stroke_transpir_70_79

# Downwards solar radiation change 70-79 (stroke analysis)
stroke_down_solar_70_79 <- lmer(STROKE_CrudePrev ~ downwards_solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                  + Temp_C_2013_2022 + LCchangeMEAN +
                                  (1 | CountyFIPS), data = subset_data)

summary(stroke_down_solar_70_79)
stroke_transpir_70_79_summary <- summary(stroke_down_solar_70_79)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_down_solar_70_79 <- coef(stroke_transpir_70_79_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_downwards_solar_70_79
se_stroke_figS7$stroke_down_solar_70_79 <- coef(stroke_transpir_70_79_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_down_solar_70_79 <- coefficients_stroke_figS7$stroke_down_solar_70_79 - 1.96 * se_stroke_figS7$stroke_down_solar_70_79
upper_bound_stroke_figS7$stroke_down_solar_70_79 <- coefficients_stroke_figS7$stroke_down_solar_70_79 + 1.96 * se_stroke_figS7$stroke_down_solar_70_79

########################### Stroke 1980-1990 ##################################
# average anomaly size
avg_HI_80_89 <- mean(subset_data$HI_C_Change80_89_13_22)
avg_temp_80_89 <- mean(subset_data$Change_Temp_C_80_89_13_22)
avg_rh_80_89 <- mean(subset_data$Change_RH_80_89_13_22)
avg_windu_80_89 <- mean(subset_data$windU_diff80_89_13_22)
avg_windv_80_89 <- mean(subset_data$windV_diff80_89_13_22)
avg_latent_80_89 <- mean(subset_data$latent_diff80_89_13_22_P_Mill)
avg_solar_80_89 <- mean(subset_data$solar_diff80_89_13_22_P_Mill)
avg_thermal_80_89 <- mean(subset_data$thermal_diff80_89_13_22_P_Mill)
avg_sensible_80_89 <- mean(subset_data$sensible_diff80_89_13_22_P_Mill)
avg_evap_80_89 <- mean(subset_data$evap_diff80_89_13_22_P_x10)
avg_press_80_89 <- mean(subset_data$pressure_diff80_89_13_22_P_div10)
avg_precip_80_89 <- mean(subset_data$precip_diff80_89_13_22_P_x10)
avg_transpir_80_89 <- mean(subset_data$transpir_diff80_89_13_22_P_x1000)
avg_downwards_solar_80_89 <- mean(subset_data$downwards_solar_diff80_89_13_22_P_Mill)

# HI change 80-89
stroke_hi_80_89 <- lmer(STROKE_CrudePrev ~ HI_C_Change80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_80_89)
stroke_hi_80_89_summary <- summary(stroke_hi_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_hi_80_89 <- coef(stroke_hi_80_89_summary)["HI_C_Change80_89_13_22", "Estimate"] * avg_HI_80_89
se_stroke_figS7$stroke_hi_80_89 <- coef(stroke_hi_80_89_summary)["HI_C_Change80_89_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_hi_80_89 <- coefficients_stroke_figS7$stroke_hi_80_89 - 1.96 * se_stroke_figS7$stroke_hi_80_89
upper_bound_stroke_figS7$stroke_hi_80_89 <- coefficients_stroke_figS7$stroke_hi_80_89 + 1.96 * se_stroke_figS7$stroke_hi_80_89

# Temperature change 80-89
stroke_temp_80_89 <- lmer(STROKE_CrudePrev ~ Change_Temp_C_80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(stroke_temp_80_89)
stroke_temp_80_89_summary <- summary(stroke_temp_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_temp_80_89 <- coef(stroke_temp_80_89_summary)["Change_Temp_C_80_89_13_22", "Estimate"] * avg_temp_80_89
se_stroke_figS7$stroke_temp_80_89 <- coef(stroke_temp_80_89_summary)["Change_Temp_C_80_89_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_temp_80_89 <- coefficients_stroke_figS7$stroke_temp_80_89 - 1.96 * se_stroke_figS7$stroke_temp_80_89
upper_bound_stroke_figS7$stroke_temp_80_89 <- coefficients_stroke_figS7$stroke_temp_80_89 + 1.96 * se_stroke_figS7$stroke_temp_80_89

# Relative humidity change 80-89
stroke_rh_80_89 <- lmer(STROKE_CrudePrev ~ Change_RH_80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_rh_80_89)
stroke_rh_80_89_summary <- summary(stroke_rh_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_rh_80_89 <- coef(stroke_rh_80_89_summary)["Change_RH_80_89_13_22", "Estimate"] * avg_rh_80_89
se_stroke_figS7$stroke_rh_80_89 <- coef(stroke_rh_80_89_summary)["Change_RH_80_89_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_rh_80_89 <- coefficients_stroke_figS7$stroke_rh_80_89 - 1.96 * se_stroke_figS7$stroke_rh_80_89
upper_bound_stroke_figS7$stroke_rh_80_89 <- coefficients_stroke_figS7$stroke_rh_80_89 + 1.96 * se_stroke_figS7$stroke_rh_80_89

# Wind U change 80-89
stroke_windu_80_89 <- lmer(STROKE_CrudePrev ~ windU_diff80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windu_80_89)
stroke_windu_80_89_summary <- summary(stroke_windu_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windu_80_89 <- coef(stroke_windu_80_89_summary)["windU_diff80_89_13_22", "Estimate"] * avg_windu_80_89
se_stroke_figS7$stroke_windu_80_89 <- coef(stroke_windu_80_89_summary)["windU_diff80_89_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windu_80_89 <- coefficients_stroke_figS7$stroke_windu_80_89 - 1.96 * se_stroke_figS7$stroke_windu_80_89
upper_bound_stroke_figS7$stroke_windu_80_89 <- coefficients_stroke_figS7$stroke_windu_80_89 + 1.96 * se_stroke_figS7$stroke_windu_80_89

# Wind V change 80-89
stroke_windv_80_89 <- lmer(STROKE_CrudePrev ~ windV_diff80_89_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windv_80_89)
stroke_windv_80_89_summary <- summary(stroke_windv_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windv_80_89 <- coef(stroke_windv_80_89_summary)["windV_diff80_89_13_22", "Estimate"] * avg_windv_80_89
se_stroke_figS7$stroke_windv_80_89 <- coef(stroke_windv_80_89_summary)["windV_diff80_89_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windv_80_89 <- coefficients_stroke_figS7$stroke_windv_80_89 - 1.96 * se_stroke_figS7$stroke_windv_80_89
upper_bound_stroke_figS7$stroke_windv_80_89 <- coefficients_stroke_figS7$stroke_windv_80_89 + 1.96 * se_stroke_figS7$stroke_windv_80_89

# Latent change 80-89
stroke_latent_80_89 <- lmer(STROKE_CrudePrev ~ latent_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_latent_80_89)
stroke_latent_80_89_summary <- summary(stroke_latent_80_89)
# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_latent_80_89 <- coef(stroke_latent_80_89_summary)["latent_diff80_89_13_22_P_Mill", "Estimate"] * avg_latent_80_89
se_stroke_figS7$stroke_latent_80_89 <- coef(stroke_latent_80_89_summary)["latent_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_latent_80_89 <- coefficients_stroke_figS7$stroke_latent_80_89 - 1.96 * se_stroke_figS7$stroke_latent_80_89
upper_bound_stroke_figS7$stroke_latent_80_89 <- coefficients_stroke_figS7$stroke_latent_80_89 + 1.96 * se_stroke_figS7$stroke_latent_80_89

# Solar change 80-89
stroke_solar_80_89 <- lmer(STROKE_CrudePrev ~ solar_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_solar_80_89)
stroke_solar_80_89_summary <- summary(stroke_solar_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_solar_80_89 <- coef(stroke_solar_80_89_summary)["solar_diff80_89_13_22_P_Mill", "Estimate"] * avg_solar_80_89
se_stroke_figS7$stroke_solar_80_89 <- coef(stroke_solar_80_89_summary)["solar_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_solar_80_89 <- coefficients_stroke_figS7$stroke_solar_80_89 - 1.96 * se_stroke_figS7$stroke_solar_80_89
upper_bound_stroke_figS7$stroke_solar_80_89 <- coefficients_stroke_figS7$stroke_solar_80_89 + 1.96 * se_stroke_figS7$stroke_solar_80_89

# Thermal change 80-89
stroke_thermal_80_89 <- lmer(STROKE_CrudePrev ~ thermal_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                               + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_thermal_80_89)
stroke_thermal_80_89_summary <- summary(stroke_thermal_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_thermal_80_89 <- coef(stroke_thermal_80_89_summary)["thermal_diff80_89_13_22_P_Mill", "Estimate"] * avg_thermal_80_89
se_stroke_figS7$stroke_thermal_80_89 <- coef(stroke_thermal_80_89_summary)["thermal_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_thermal_80_89 <- coefficients_stroke_figS7$stroke_thermal_80_89 - 1.96 * se_stroke_figS7$stroke_thermal_80_89
upper_bound_stroke_figS7$stroke_thermal_80_89 <- coefficients_stroke_figS7$stroke_thermal_80_89 + 1.96 * se_stroke_figS7$stroke_thermal_80_89

# Sensible heat change 80-89
stroke_sensible_80_89 <- lmer(STROKE_CrudePrev ~ sensible_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_sensible_80_89)
stroke_sensible_80_89_summary <- summary(stroke_sensible_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_sensible_80_89 <- coef(stroke_sensible_80_89_summary)["sensible_diff80_89_13_22_P_Mill", "Estimate"] * avg_sensible_80_89
se_stroke_figS7$stroke_sensible_80_89 <- coef(stroke_sensible_80_89_summary)["sensible_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_sensible_80_89 <- coefficients_stroke_figS7$stroke_sensible_80_89 - 1.96 * se_stroke_figS7$stroke_sensible_80_89
upper_bound_stroke_figS7$stroke_sensible_80_89 <- coefficients_stroke_figS7$stroke_sensible_80_89 + 1.96 * se_stroke_figS7$stroke_sensible_80_89

# Evaporation change 80-89
stroke_evap_80_89 <- lmer(STROKE_CrudePrev ~ evap_diff80_89_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)

summary(stroke_evap_80_89)
stroke_evap_80_89_summary <- summary(stroke_evap_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_evap_80_89 <- coef(stroke_evap_80_89_summary)["evap_diff80_89_13_22_P_x10", "Estimate"] * avg_evap_80_89
se_stroke_figS7$stroke_evap_80_89 <- coef(stroke_evap_80_89_summary)["evap_diff80_89_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_evap_80_89 <- coefficients_stroke_figS7$stroke_evap_80_89 - 1.96 * se_stroke_figS7$stroke_evap_80_89
upper_bound_stroke_figS7$stroke_evap_80_89 <- coefficients_stroke_figS7$stroke_evap_80_89 + 1.96 * se_stroke_figS7$stroke_evap_80_89

# Surface pressure change 80-89
stroke_pressure_80_89 <- lmer(STROKE_CrudePrev ~ pressure_diff80_89_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_pressure_80_89)
stroke_pressure_80_89_summary <- summary(stroke_pressure_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_pressure_80_89 <- coef(stroke_pressure_80_89_summary)["pressure_diff80_89_13_22_P_div10", "Estimate"] * avg_press_80_89
se_stroke_figS7$stroke_pressure_80_89 <- coef(stroke_pressure_80_89_summary)["pressure_diff80_89_13_22_P_div10", "Std. Error"]
lower_bound_stroke_figS7$stroke_pressure_80_89 <- coefficients_stroke_figS7$stroke_pressure_80_89 - 1.96 * se_stroke_figS7$stroke_pressure_80_89
upper_bound_stroke_figS7$stroke_pressure_80_89 <- coefficients_stroke_figS7$stroke_pressure_80_89 + 1.96 * se_stroke_figS7$stroke_pressure_80_89

# Precipitation change 80-89 (stroke analysis)
stroke_precip_80_89 <- lmer(STROKE_CrudePrev ~ precip_diff80_89_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)

summary(stroke_precip_80_89)
stroke_precip_80_89_summary <- summary(stroke_precip_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_precip_80_89 <- coef(stroke_precip_80_89_summary)["precip_diff80_89_13_22_P_x10", "Estimate"] * avg_precip_80_89
se_stroke_figS7$stroke_precip_80_89 <- coef(stroke_precip_80_89_summary)["precip_diff80_89_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_precip_80_89 <- coefficients_stroke_figS7$stroke_precip_80_89 - 1.96 * se_stroke_figS7$stroke_precip_80_89
upper_bound_stroke_figS7$stroke_precip_80_89 <- coefficients_stroke_figS7$stroke_precip_80_89 + 1.96 * se_stroke_figS7$stroke_precip_80_89

# Transpiration change 80-89 (stroke analysis)
stroke_transpir_80_89 <- lmer(STROKE_CrudePrev ~ transpir_diff80_89_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_transpir_80_89)
stroke_transpir_80_89_summary <- summary(stroke_transpir_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_transpir_80_89 <- coef(stroke_transpir_80_89_summary)["transpir_diff80_89_13_22_P_x1000", "Estimate"] * avg_transpir_80_89
se_stroke_figS7$stroke_transpir_80_89 <- coef(stroke_transpir_80_89_summary)["transpir_diff80_89_13_22_P_x1000", "Std. Error"]
lower_bound_stroke_figS7$stroke_transpir_80_89 <- coefficients_stroke_figS7$stroke_transpir_80_89 - 1.96 * se_stroke_figS7$stroke_transpir_80_89
upper_bound_stroke_figS7$stroke_transpir_80_89 <- coefficients_stroke_figS7$stroke_transpir_80_89 + 1.96 * se_stroke_figS7$stroke_transpir_80_89

# Downwards solar radiation change 80-89 (stroke analysis)
stroke_down_solar_80_89 <- lmer(STROKE_CrudePrev ~ downwards_solar_diff80_89_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                  + Temp_C_2013_2022 + LCchangeMEAN +
                                  (1 | CountyFIPS), data = subset_data)

summary(stroke_down_solar_80_89)
stroke_transpir_80_89_summary <- summary(stroke_down_solar_80_89)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_down_solar_80_89 <- coef(stroke_transpir_80_89_summary)["downwards_solar_diff80_89_13_22_P_Mill", "Estimate"] * avg_downwards_solar_80_89
se_stroke_figS7$stroke_down_solar_80_89 <- coef(stroke_transpir_80_89_summary)["downwards_solar_diff80_89_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_down_solar_80_89 <- coefficients_stroke_figS7$stroke_down_solar_80_89 - 1.96 * se_stroke_figS7$stroke_down_solar_80_89
upper_bound_stroke_figS7$stroke_down_solar_80_89 <- coefficients_stroke_figS7$stroke_down_solar_80_89 + 1.96 * se_stroke_figS7$stroke_down_solar_80_89

########################### Stroke 1990-2000 #################################
# average anomaly size
avg_HI_90_99 <- mean(subset_data$HI_C_Change90_99_13_22)
avg_temp_90_99 <- mean(subset_data$Change_Temp_C_90_99_13_22)
avg_rh_90_99 <- mean(subset_data$Change_RH_90_99_13_22)
avg_windu_90_99 <- mean(subset_data$windU_diff90_99_13_22)
avg_windv_90_99 <- mean(subset_data$windV_diff90_99_13_22)
avg_latent_90_99 <- mean(subset_data$latent_diff90_99_13_22_P_Mill)
avg_solar_90_99 <- mean(subset_data$solar_diff90_99_13_22_P_Mill)
avg_thermal_90_99 <- mean(subset_data$thermal_diff90_99_13_22_P_Mill)
avg_sensible_90_99 <- mean(subset_data$sensible_diff90_99_13_22_P_Mill)
avg_evap_90_99 <- mean(subset_data$evap_diff90_99_13_22_P_x10)
avg_press_90_99 <- mean(subset_data$pressure_diff90_99_13_22_P_div10)
avg_precip_90_99 <- mean(subset_data$precip_diff90_99_13_22_P_x10)
avg_transpir_90_99 <- mean(subset_data$transpir_diff90_99_13_22_P_x1000)
avg_downwards_solar_90_99 <- mean(subset_data$downwards_solar_diff90_99_13_22_P_Mill)

# HI change 90-99
stroke_hi_90_99 <- lmer(STROKE_CrudePrev ~ HI_C_Change90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_90_99)
stroke_hi_90_99_summary <- summary(stroke_hi_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_hi_90_99 <- coef(stroke_hi_90_99_summary)["HI_C_Change90_99_13_22", "Estimate"] * avg_HI_90_99
se_stroke_figS7$stroke_hi_90_99 <- coef(stroke_hi_90_99_summary)["HI_C_Change90_99_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_hi_90_99 <- coefficients_stroke_figS7$stroke_hi_90_99 - 1.96 * se_stroke_figS7$stroke_hi_90_99
upper_bound_stroke_figS7$stroke_hi_90_99 <- coefficients_stroke_figS7$stroke_hi_90_99 + 1.96 * se_stroke_figS7$stroke_hi_90_99

# Temperature change 90-99
stroke_temp_90_99 <- lmer(STROKE_CrudePrev ~ Change_Temp_C_90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(stroke_temp_90_99)
stroke_temp_90_99_summary <- summary(stroke_temp_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_temp_90_99 <- coef(stroke_temp_90_99_summary)["Change_Temp_C_90_99_13_22", "Estimate"] * avg_temp_90_99
se_stroke_figS7$stroke_temp_90_99 <- coef(stroke_temp_90_99_summary)["Change_Temp_C_90_99_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_temp_90_99 <- coefficients_stroke_figS7$stroke_temp_90_99 - 1.96 * se_stroke_figS7$stroke_temp_90_99
upper_bound_stroke_figS7$stroke_temp_90_99 <- coefficients_stroke_figS7$stroke_temp_90_99 + 1.96 * se_stroke_figS7$stroke_temp_90_99

# Relative humidity change 90-99
stroke_rh_90_99 <- lmer(STROKE_CrudePrev ~ Change_RH_90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                          + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_rh_90_99)
stroke_rh_90_99_summary <- summary(stroke_rh_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_rh_90_99 <- coef(stroke_rh_90_99_summary)["Change_RH_90_99_13_22", "Estimate"] * avg_rh_90_99
se_stroke_figS7$stroke_rh_90_99 <- coef(stroke_rh_90_99_summary)["Change_RH_90_99_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_rh_90_99 <- coefficients_stroke_figS7$stroke_rh_90_99 - 1.96 * se_stroke_figS7$stroke_rh_90_99
upper_bound_stroke_figS7$stroke_rh_90_99 <- coefficients_stroke_figS7$stroke_rh_90_99 + 1.96 * se_stroke_figS7$stroke_rh_90_99

# Wind U change 90-99
stroke_windu_90_99 <- lmer(STROKE_CrudePrev ~ windU_diff90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windu_90_99)
stroke_windu_90_99_summary <- summary(stroke_windu_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windu_90_99 <- coef(stroke_windu_90_99_summary)["windU_diff90_99_13_22", "Estimate"] * avg_windu_90_99
se_stroke_figS7$stroke_windu_90_99 <- coef(stroke_windu_90_99_summary)["windU_diff90_99_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windu_90_99 <- coefficients_stroke_figS7$stroke_windu_90_99 - 1.96 * se_stroke_figS7$stroke_windu_90_99
upper_bound_stroke_figS7$stroke_windu_90_99 <- coefficients_stroke_figS7$stroke_windu_90_99 + 1.96 * se_stroke_figS7$stroke_windu_90_99

# Wind V change 90-99
stroke_windv_90_99 <- lmer(STROKE_CrudePrev ~ windV_diff90_99_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_windv_90_99)
stroke_windv_90_99_summary <- summary(stroke_windv_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windv_90_99 <- coef(stroke_windv_90_99_summary)["windV_diff90_99_13_22", "Estimate"] * avg_windv_90_99
se_stroke_figS7$stroke_windv_90_99 <- coef(stroke_windv_90_99_summary)["windV_diff90_99_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windv_90_99 <- coefficients_stroke_figS7$stroke_windv_90_99 - 1.96 * se_stroke_figS7$stroke_windv_90_99
upper_bound_stroke_figS7$stroke_windv_90_99 <- coefficients_stroke_figS7$stroke_windv_90_99 + 1.96 * se_stroke_figS7$stroke_windv_90_99

# Latent change 90-99
stroke_latent_90_99 <- lmer(STROKE_CrudePrev ~ latent_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_latent_90_99)
stroke_latent_90_99_summary <- summary(stroke_latent_90_99)
# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_latent_90_99 <- coef(stroke_latent_90_99_summary)["latent_diff90_99_13_22_P_Mill", "Estimate"] * avg_latent_90_99
se_stroke_figS7$stroke_latent_90_99 <- coef(stroke_latent_90_99_summary)["latent_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_latent_90_99 <- coefficients_stroke_figS7$stroke_latent_90_99 - 1.96 * se_stroke_figS7$stroke_latent_90_99
upper_bound_stroke_figS7$stroke_latent_90_99 <- coefficients_stroke_figS7$stroke_latent_90_99 + 1.96 * se_stroke_figS7$stroke_latent_90_99

# Solar change 90-99
stroke_solar_90_99 <- lmer(STROKE_CrudePrev ~ solar_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                             + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_solar_90_99)
stroke_solar_90_99_summary <- summary(stroke_solar_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_solar_90_99 <- coef(stroke_solar_90_99_summary)["solar_diff90_99_13_22_P_Mill", "Estimate"] * avg_solar_90_99
se_stroke_figS7$stroke_solar_90_99 <- coef(stroke_solar_90_99_summary)["solar_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_solar_90_99 <- coefficients_stroke_figS7$stroke_solar_90_99 - 1.96 * se_stroke_figS7$stroke_solar_90_99
upper_bound_stroke_figS7$stroke_solar_90_99 <- coefficients_stroke_figS7$stroke_solar_90_99 + 1.96 * se_stroke_figS7$stroke_solar_90_99

# Thermal change 90-99
stroke_thermal_90_99 <- lmer(STROKE_CrudePrev ~ thermal_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                               + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_thermal_90_99)
stroke_thermal_90_99_summary <- summary(stroke_thermal_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_thermal_90_99 <- coef(stroke_thermal_90_99_summary)["thermal_diff90_99_13_22_P_Mill", "Estimate"] * avg_thermal_90_99
se_stroke_figS7$stroke_thermal_90_99 <- coef(stroke_thermal_90_99_summary)["thermal_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_thermal_90_99 <- coefficients_stroke_figS7$stroke_thermal_90_99 - 1.96 * se_stroke_figS7$stroke_thermal_90_99
upper_bound_stroke_figS7$stroke_thermal_90_99 <- coefficients_stroke_figS7$stroke_thermal_90_99 + 1.96 * se_stroke_figS7$stroke_thermal_90_99

# Sensible heat change 90-99
stroke_sensible_90_99 <- lmer(STROKE_CrudePrev ~ sensible_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_sensible_90_99)
stroke_sensible_90_99_summary <- summary(stroke_sensible_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_sensible_90_99 <- coef(stroke_sensible_90_99_summary)["sensible_diff90_99_13_22_P_Mill", "Estimate"] * avg_sensible_90_99
se_stroke_figS7$stroke_sensible_90_99 <- coef(stroke_sensible_90_99_summary)["sensible_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_sensible_90_99 <- coefficients_stroke_figS7$stroke_sensible_90_99 - 1.96 * se_stroke_figS7$stroke_sensible_90_99
upper_bound_stroke_figS7$stroke_sensible_90_99 <- coefficients_stroke_figS7$stroke_sensible_90_99 + 1.96 * se_stroke_figS7$stroke_sensible_90_99

# Evaporation change 90-99
stroke_evap_90_99 <- lmer(STROKE_CrudePrev ~ evap_diff90_99_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                            + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)

summary(stroke_evap_90_99)
stroke_evap_90_99_summary <- summary(stroke_evap_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_evap_90_99 <- coef(stroke_evap_90_99_summary)["evap_diff90_99_13_22_P_x10", "Estimate"] * avg_evap_90_99
se_stroke_figS7$stroke_evap_90_99 <- coef(stroke_evap_90_99_summary)["evap_diff90_99_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_evap_90_99 <- coefficients_stroke_figS7$stroke_evap_90_99 - 1.96 * se_stroke_figS7$stroke_evap_90_99
upper_bound_stroke_figS7$stroke_evap_90_99 <- coefficients_stroke_figS7$stroke_evap_90_99 + 1.96 * se_stroke_figS7$stroke_evap_90_99

# Surface pressure change 90-99
stroke_pressure_90_99 <- lmer(STROKE_CrudePrev ~ pressure_diff90_99_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_pressure_90_99)
stroke_pressure_90_99_summary <- summary(stroke_pressure_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_pressure_90_99 <- coef(stroke_pressure_90_99_summary)["pressure_diff90_99_13_22_P_div10", "Estimate"] * avg_press_90_99
se_stroke_figS7$stroke_pressure_90_99 <- coef(stroke_pressure_90_99_summary)["pressure_diff90_99_13_22_P_div10", "Std. Error"]
lower_bound_stroke_figS7$stroke_pressure_90_99 <- coefficients_stroke_figS7$stroke_pressure_90_99 - 1.96 * se_stroke_figS7$stroke_pressure_90_99
upper_bound_stroke_figS7$stroke_pressure_90_99 <- coefficients_stroke_figS7$stroke_pressure_90_99 + 1.96 * se_stroke_figS7$stroke_pressure_90_99

# Precipitation change 90-99 (stroke analysis)
stroke_precip_90_99 <- lmer(STROKE_CrudePrev ~ precip_diff90_99_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)

summary(stroke_precip_90_99)
stroke_precip_90_99_summary <- summary(stroke_precip_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_precip_90_99 <- coef(stroke_precip_90_99_summary)["precip_diff90_99_13_22_P_x10", "Estimate"] * avg_precip_90_99
se_stroke_figS7$stroke_precip_90_99 <- coef(stroke_precip_90_99_summary)["precip_diff90_99_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_precip_90_99 <- coefficients_stroke_figS7$stroke_precip_90_99 - 1.96 * se_stroke_figS7$stroke_precip_90_99
upper_bound_stroke_figS7$stroke_precip_90_99 <- coefficients_stroke_figS7$stroke_precip_90_99 + 1.96 * se_stroke_figS7$stroke_precip_90_99

# Transpiration change 90-99 (stroke analysis)
stroke_transpir_90_99 <- lmer(STROKE_CrudePrev ~ transpir_diff90_99_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_transpir_90_99)
stroke_transpir_90_99_summary <- summary(stroke_transpir_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_transpir_90_99 <- coef(stroke_transpir_90_99_summary)["transpir_diff90_99_13_22_P_x1000", "Estimate"] * avg_transpir_90_99
se_stroke_figS7$stroke_transpir_90_99 <- coef(stroke_transpir_90_99_summary)["transpir_diff90_99_13_22_P_x1000", "Std. Error"]
lower_bound_stroke_figS7$stroke_transpir_90_99 <- coefficients_stroke_figS7$stroke_transpir_90_99 - 1.96 * se_stroke_figS7$stroke_transpir_90_99
upper_bound_stroke_figS7$stroke_transpir_90_99 <- coefficients_stroke_figS7$stroke_transpir_90_99 + 1.96 * se_stroke_figS7$stroke_transpir_90_99

# Downwards solar radiation change 90-99 (stroke analysis)
stroke_down_solar_90_99 <- lmer(STROKE_CrudePrev ~ downwards_solar_diff90_99_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                  + Temp_C_2013_2022 + LCchangeMEAN +
                                  (1 | CountyFIPS), data = subset_data)

summary(stroke_down_solar_90_99)
stroke_transpir_90_99_summary <- summary(stroke_down_solar_90_99)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_down_solar_90_99 <- coef(stroke_transpir_90_99_summary)["downwards_solar_diff90_99_13_22_P_Mill", "Estimate"] * avg_downwards_solar_90_99
se_stroke_figS7$stroke_down_solar_90_99 <- coef(stroke_transpir_90_99_summary)["downwards_solar_diff90_99_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_down_solar_90_99 <- coefficients_stroke_figS7$stroke_down_solar_90_99 - 1.96 * se_stroke_figS7$stroke_down_solar_90_99
upper_bound_stroke_figS7$stroke_down_solar_90_99 <- coefficients_stroke_figS7$stroke_down_solar_90_99 + 1.96 * se_stroke_figS7$stroke_down_solar_90_99

########################### Stroke 2000-2010 ##################################
# average anomaly size
avg_HI_2000_2009 <- mean(subset_data$HI_C_Change2000_2009_13_22)
avg_temp_2000_2009 <- mean(subset_data$Change_Temp_C_2000_2009_13_22)
avg_rh_2000_2009 <- mean(subset_data$Change_RH_2000_2009_13_22)
avg_windu_2000_2009 <- mean(subset_data$windU_diff2000_2009_13_22)
avg_windv_2000_2009 <- mean(subset_data$windV_diff2000_2009_13_22)
avg_latent_2000_2009 <- mean(subset_data$latent_diff2000_2009_13_22_P_Mill)
avg_solar_2000_2009 <- mean(subset_data$solar_diff2000_2009_13_22_P_Mill)
avg_thermal_2000_2009 <- mean(subset_data$thermal_diff2000_2009_13_22_P_Mill)
avg_sensible_2000_2009 <- mean(subset_data$sensible_diff2000_2009_13_22_P_Mill)
avg_evap_2000_2009 <- mean(subset_data$evap_diff2000_2009_13_22_P_x10)
avg_press_2000_2009 <- mean(subset_data$pressure_diff2000_2009_13_22_P_div10)
avg_precip_2000_2009 <- mean(subset_data$precip_diff2000_2009_13_22_P_x10)
avg_transpir_2000_2009 <- mean(subset_data$transpir_diff2000_2009_13_22_P_x1000)
avg_downwards_solar_2000_2009 <- mean(subset_data$downwards_solar_diff2000_2009_13_22_P_Mill)

# HI change 2000-2009
stroke_hi_2000_2009 <- lmer(STROKE_CrudePrev ~ HI_C_Change2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_2000_2009)
stroke_hi_2000_2009_summary <- summary(stroke_hi_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_hi_2000_2009 <- coef(stroke_hi_2000_2009_summary)["HI_C_Change2000_2009_13_22", "Estimate"] * avg_HI_2000_2009
se_stroke_figS7$stroke_hi_2000_2009 <- coef(stroke_hi_2000_2009_summary)["HI_C_Change2000_2009_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_hi_2000_2009 <- coefficients_stroke_figS7$stroke_hi_2000_2009 - 1.96 * se_stroke_figS7$stroke_hi_2000_2009
upper_bound_stroke_figS7$stroke_hi_2000_2009 <- coefficients_stroke_figS7$stroke_hi_2000_2009 + 1.96 * se_stroke_figS7$stroke_hi_2000_2009

# Temperature change 2000-2009
stroke_temp_2000_2009 <- lmer(STROKE_CrudePrev ~ Change_Temp_C_2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)
summary(stroke_temp_2000_2009)
stroke_temp_2000_2009_summary <- summary(stroke_temp_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_temp_2000_2009 <- coef(stroke_temp_2000_2009_summary)["Change_Temp_C_2000_2009_13_22", "Estimate"] * avg_temp_2000_2009
se_stroke_figS7$stroke_temp_2000_2009 <- coef(stroke_temp_2000_2009_summary)["Change_Temp_C_2000_2009_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_temp_2000_2009 <- coefficients_stroke_figS7$stroke_temp_2000_2009 - 1.96 * se_stroke_figS7$stroke_temp_2000_2009
upper_bound_stroke_figS7$stroke_temp_2000_2009 <- coefficients_stroke_figS7$stroke_temp_2000_2009 + 1.96 * se_stroke_figS7$stroke_temp_2000_2009

# Relative humidity change 2000-2009
stroke_rh_2000_2009 <- lmer(STROKE_CrudePrev ~ Change_RH_2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                              + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_rh_2000_2009)
stroke_rh_2000_2009_summary <- summary(stroke_rh_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_rh_2000_2009 <- coef(stroke_rh_2000_2009_summary)["Change_RH_2000_2009_13_22", "Estimate"] * avg_rh_2000_2009
se_stroke_figS7$stroke_rh_2000_2009 <- coef(stroke_rh_2000_2009_summary)["Change_RH_2000_2009_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_rh_2000_2009 <- coefficients_stroke_figS7$stroke_rh_2000_2009 - 1.96 * se_stroke_figS7$stroke_rh_2000_2009
upper_bound_stroke_figS7$stroke_rh_2000_2009 <- coefficients_stroke_figS7$stroke_rh_2000_2009 + 1.96 * se_stroke_figS7$stroke_rh_2000_2009

# Wind U change 2000-2009
stroke_windu_2000_2009 <- lmer(STROKE_CrudePrev ~ windU_diff2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_windu_2000_2009)
stroke_windu_2000_2009_summary <- summary(stroke_windu_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windu_2000_2009 <- coef(stroke_windu_2000_2009_summary)["windU_diff2000_2009_13_22", "Estimate"] * avg_windu_2000_2009
se_stroke_figS7$stroke_windu_2000_2009 <- coef(stroke_windu_2000_2009_summary)["windU_diff2000_2009_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windu_2000_2009 <- coefficients_stroke_figS7$stroke_windu_2000_2009 - 1.96 * se_stroke_figS7$stroke_windu_2000_2009
upper_bound_stroke_figS7$stroke_windu_2000_2009 <- coefficients_stroke_figS7$stroke_windu_2000_2009 + 1.96 * se_stroke_figS7$stroke_windu_2000_2009

# Wind V change 2000-2009
stroke_windv_2000_2009 <- lmer(STROKE_CrudePrev ~ windV_diff2000_2009_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_windv_2000_2009)
stroke_windv_2000_2009_summary <- summary(stroke_windv_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_windv_2000_2009 <- coef(stroke_windv_2000_2009_summary)["windV_diff2000_2009_13_22", "Estimate"] * avg_windv_2000_2009
se_stroke_figS7$stroke_windv_2000_2009 <- coef(stroke_windv_2000_2009_summary)["windV_diff2000_2009_13_22", "Std. Error"]
lower_bound_stroke_figS7$stroke_windv_2000_2009 <- coefficients_stroke_figS7$stroke_windv_2000_2009 - 1.96 * se_stroke_figS7$stroke_windv_2000_2009
upper_bound_stroke_figS7$stroke_windv_2000_2009 <- coefficients_stroke_figS7$stroke_windv_2000_2009 + 1.96 * se_stroke_figS7$stroke_windv_2000_2009

# Latent change 2000-2009
stroke_latent_2000_2009 <- lmer(STROKE_CrudePrev ~ latent_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                  + Temp_C_2013_2022 + LCchangeMEAN +
                                  (1 | CountyFIPS), data = subset_data)
summary(stroke_latent_2000_2009)
stroke_latent_2000_2009_summary <- summary(stroke_latent_2000_2009)
# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_latent_2000_2009 <- coef(stroke_latent_2000_2009_summary)["latent_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_latent_2000_2009
se_stroke_figS7$stroke_latent_2000_2009 <- coef(stroke_latent_2000_2009_summary)["latent_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_latent_2000_2009 <- coefficients_stroke_figS7$stroke_latent_2000_2009 - 1.96 * se_stroke_figS7$stroke_latent_2000_2009
upper_bound_stroke_figS7$stroke_latent_2000_2009 <- coefficients_stroke_figS7$stroke_latent_2000_2009 + 1.96 * se_stroke_figS7$stroke_latent_2000_2009

# Solar change 2000-2009
stroke_solar_2000_2009 <- lmer(STROKE_CrudePrev ~ solar_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_solar_2000_2009)
stroke_solar_2000_2009_summary <- summary(stroke_solar_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_solar_2000_2009 <- coef(stroke_solar_2000_2009_summary)["solar_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_solar_2000_2009
se_stroke_figS7$stroke_solar_2000_2009 <- coef(stroke_solar_2000_2009_summary)["solar_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_solar_2000_2009 <- coefficients_stroke_figS7$stroke_solar_2000_2009 - 1.96 * se_stroke_figS7$stroke_solar_2000_2009
upper_bound_stroke_figS7$stroke_solar_2000_2009 <- coefficients_stroke_figS7$stroke_solar_2000_2009 + 1.96 * se_stroke_figS7$stroke_solar_2000_2009

# Thermal change 2000-2009
stroke_thermal_2000_2009 <- lmer(STROKE_CrudePrev ~ thermal_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                   + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                   + Temp_C_2013_2022 + LCchangeMEAN +
                                   (1 | CountyFIPS), data = subset_data)
summary(stroke_thermal_2000_2009)
stroke_thermal_2000_2009_summary <- summary(stroke_thermal_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_thermal_2000_2009 <- coef(stroke_thermal_2000_2009_summary)["thermal_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_thermal_2000_2009
se_stroke_figS7$stroke_thermal_2000_2009 <- coef(stroke_thermal_2000_2009_summary)["thermal_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_thermal_2000_2009 <- coefficients_stroke_figS7$stroke_thermal_2000_2009 - 1.96 * se_stroke_figS7$stroke_thermal_2000_2009
upper_bound_stroke_figS7$stroke_thermal_2000_2009 <- coefficients_stroke_figS7$stroke_thermal_2000_2009 + 1.96 * se_stroke_figS7$stroke_thermal_2000_2009

# Sensible heat change 2000-2009
stroke_sensible_2000_2009 <- lmer(STROKE_CrudePrev ~ sensible_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                    + Temp_C_2013_2022 + LCchangeMEAN +
                                    (1 | CountyFIPS), data = subset_data)

summary(stroke_sensible_2000_2009)
stroke_sensible_2000_2009_summary <- summary(stroke_sensible_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_sensible_2000_2009 <- coef(stroke_sensible_2000_2009_summary)["sensible_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_sensible_2000_2009
se_stroke_figS7$stroke_sensible_2000_2009 <- coef(stroke_sensible_2000_2009_summary)["sensible_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_sensible_2000_2009 <- coefficients_stroke_figS7$stroke_sensible_2000_2009 - 1.96 * se_stroke_figS7$stroke_sensible_2000_2009
upper_bound_stroke_figS7$stroke_sensible_2000_2009 <- coefficients_stroke_figS7$stroke_sensible_2000_2009 + 1.96 * se_stroke_figS7$stroke_sensible_2000_2009

# Evaporation change 2000-2009
stroke_evap_2000_2009 <- lmer(STROKE_CrudePrev ~ evap_diff2000_2009_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)

summary(stroke_evap_2000_2009)
stroke_evap_2000_2009_summary <- summary(stroke_evap_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_evap_2000_2009 <- coef(stroke_evap_2000_2009_summary)["evap_diff2000_2009_13_22_P_x10", "Estimate"] * avg_evap_2000_2009
se_stroke_figS7$stroke_evap_2000_2009 <- coef(stroke_evap_2000_2009_summary)["evap_diff2000_2009_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_evap_2000_2009 <- coefficients_stroke_figS7$stroke_evap_2000_2009 - 1.96 * se_stroke_figS7$stroke_evap_2000_2009
upper_bound_stroke_figS7$stroke_evap_2000_2009 <- coefficients_stroke_figS7$stroke_evap_2000_2009 + 1.96 * se_stroke_figS7$stroke_evap_2000_2009

# Surface pressure change 2000-2009
stroke_pressure_2000_2009 <- lmer(STROKE_CrudePrev ~ pressure_diff2000_2009_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                    + Temp_C_2013_2022 + LCchangeMEAN +
                                    (1 | CountyFIPS), data = subset_data)

summary(stroke_pressure_2000_2009)
stroke_pressure_2000_2009_summary <- summary(stroke_pressure_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_pressure_2000_2009 <- coef(stroke_pressure_2000_2009_summary)["pressure_diff2000_2009_13_22_P_div10", "Estimate"] * avg_press_2000_2009
se_stroke_figS7$stroke_pressure_2000_2009 <- coef(stroke_pressure_2000_2009_summary)["pressure_diff2000_2009_13_22_P_div10", "Std. Error"]
lower_bound_stroke_figS7$stroke_pressure_2000_2009 <- coefficients_stroke_figS7$stroke_pressure_2000_2009 - 1.96 * se_stroke_figS7$stroke_pressure_2000_2009
upper_bound_stroke_figS7$stroke_pressure_2000_2009 <- coefficients_stroke_figS7$stroke_pressure_2000_2009 + 1.96 * se_stroke_figS7$stroke_pressure_2000_2009

# Precipitation change 2000-2009 (stroke analysis)
stroke_precip_2000_2009 <- lmer(STROKE_CrudePrev ~ precip_diff2000_2009_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                  + Temp_C_2013_2022 + LCchangeMEAN +
                                  (1 | CountyFIPS), data = subset_data)

summary(stroke_precip_2000_2009)
stroke_precip_2000_2009_summary <- summary(stroke_precip_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_precip_2000_2009 <- coef(stroke_precip_2000_2009_summary)["precip_diff2000_2009_13_22_P_x10", "Estimate"] * avg_precip_2000_2009
se_stroke_figS7$stroke_precip_2000_2009 <- coef(stroke_precip_2000_2009_summary)["precip_diff2000_2009_13_22_P_x10", "Std. Error"]
lower_bound_stroke_figS7$stroke_precip_2000_2009 <- coefficients_stroke_figS7$stroke_precip_2000_2009 - 1.96 * se_stroke_figS7$stroke_precip_2000_2009
upper_bound_stroke_figS7$stroke_precip_2000_2009 <- coefficients_stroke_figS7$stroke_precip_2000_2009 + 1.96 * se_stroke_figS7$stroke_precip_2000_2009

# Transpiration change 2000-2009 (stroke analysis)
stroke_transpir_2000_2009 <- lmer(STROKE_CrudePrev ~ transpir_diff2000_2009_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                    + Temp_C_2013_2022 + LCchangeMEAN +
                                    (1 | CountyFIPS), data = subset_data)

summary(stroke_transpir_2000_2009)
stroke_transpir_2000_2009_summary <- summary(stroke_transpir_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_transpir_2000_2009 <- coef(stroke_transpir_2000_2009_summary)["transpir_diff2000_2009_13_22_P_x1000", "Estimate"] * avg_transpir_2000_2009
se_stroke_figS7$stroke_transpir_2000_2009 <- coef(stroke_transpir_2000_2009_summary)["transpir_diff2000_2009_13_22_P_x1000", "Std. Error"]
lower_bound_stroke_figS7$stroke_transpir_2000_2009 <- coefficients_stroke_figS7$stroke_transpir_2000_2009 - 1.96 * se_stroke_figS7$stroke_transpir_2000_2009
upper_bound_stroke_figS7$stroke_transpir_2000_2009 <- coefficients_stroke_figS7$stroke_transpir_2000_2009 + 1.96 * se_stroke_figS7$stroke_transpir_2000_2009

# Downwards solar radiation change 2000-2009 (stroke analysis)
stroke_down_solar_2000_2009 <- lmer(STROKE_CrudePrev ~ downwards_solar_diff2000_2009_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces +
                                      + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 +
                                      + Temp_C_2013_2022 + LCchangeMEAN +
                                      (1 | CountyFIPS), data = subset_data)

summary(stroke_down_solar_2000_2009)
stroke_transpir_2000_2009_summary <- summary(stroke_down_solar_2000_2009)

# Extracting coefficients, standard errors and confidence intervals
coefficients_stroke_figS7$stroke_down_solar_2000_2009 <- coef(stroke_transpir_2000_2009_summary)["downwards_solar_diff2000_2009_13_22_P_Mill", "Estimate"] * avg_downwards_solar_2000_2009
se_stroke_figS7$stroke_down_solar_2000_2009 <- coef(stroke_transpir_2000_2009_summary)["downwards_solar_diff2000_2009_13_22_P_Mill", "Std. Error"]
lower_bound_stroke_figS7$stroke_down_solar_2000_2009 <- coefficients_stroke_figS7$stroke_down_solar_2000_2009 - 1.96 * se_stroke_figS7$stroke_down_solar_2000_2009
upper_bound_stroke_figS7$stroke_down_solar_2000_2009 <- coefficients_stroke_figS7$stroke_down_solar_2000_2009 + 1.96 * se_stroke_figS7$stroke_down_solar_2000_2009

########################### Create stroke data frame #########################################
# Define the variables
variables <- c("Heat Index", "Air Temperature", "Humidity", "Eastward Wind", "Northward Wind", "Latent Heat Flux",
               "Absorbed Sunlight", "Thermal Radiation", "Sensible Heat Flux", "Evaporation", "Surface Pressure",
               "Precipitation", "Transpiration", "Sunlight")

# Create a data frame for the forest plot
stroke_figS7 <- data.frame(
  Variable = rep(variables, times = 4),  # Repeat variables for each time period
  Time_Period = rep(c("1970-1979", "1980-1989", "1990-1999", "2000-2009"), each = length(variables)),
  Effect_Anomaly_Size = unlist(coefficients_stroke_figS7),  # Unlist coefficients from your data
  SE = unlist(se_stroke_figS7),  # Unlist standard errors from your data
  Lower_CI = unlist(lower_bound_stroke_figS7),  # Unlist lower bounds from your data
  Upper_CI = unlist(upper_bound_stroke_figS7)  # Unlist upper bounds from your data
)

# Convert Time_Period to factor to ensure correct order in facet_grid
stroke_figS7$Time_Period <- factor(stroke_figS7$Time_Period, levels = c("1970-1979", "1980-1989", "1990-1999", "2000-2009"))

########################### Combined forest plot #####################
combined_figS7 <- rbind(
  cbind(chd_figS7, Outcome = "CHD"),
  cbind(stroke_figS7, Outcome = "Stroke")
)

# create group variable name
combined_figS7$Variable_Group <- sub("_.*", "", combined_figS7$Variable)

# specify order
custom_order <- c("Surface Pressure", "Evaporation", "Latent Heat Flux", "Thermal Radiation",
                  "Northward Wind", "Precipitation", "Sensible Heat Flux", "Sunlight",
                  "Eastward Wind", "Transpiration", "Absorbed Sunlight", "Humidity",
                  "Air Temperature", "Heat Index")


# turn group name into a factor
combined_figS7$Variable_Group <- factor(combined_figS7$Variable_Group, levels = custom_order)

# group by group name and time then by outcome, create row numbers to plot more than one point per group
combined_figS7 <- combined_figS7 %>%
  group_by(Variable_Group, Time_Period) %>%
  mutate(Outcome_Order = if_else(Outcome == "CHD", 1, 2)) %>%
  ungroup()

# arrange the data frame in the right order
combined_figS7 <- combined_figS7 %>%
  arrange(Variable_Group, Time_Period, Outcome_Order)

# Create alternating red and black colors for the error bars
combined_figS7$color_group <- rep(c("red", "black"), length.out = nrow(combined_figS7))  # Alternating colors for error bars

# Create the forest plot with reduced space between Variable_Group levels
combined_figS7_plot <- ggplot(combined_figS7, aes(y = Variable_Group, x = Effect_Anomaly_Size, xmin = Lower_CI, xmax = Upper_CI, color = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Change to solid line
  geom_errorbarh(position = position_dodge(width = 0.75), aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2, color = combined_figS7$color_group) +  # Error bars in black
  geom_point(position = position_dodge(width = 0.75), size = 2) +  # Increase spacing between points
  facet_grid(. ~ Time_Period, scales = "free_y", switch = "y") +  # Use facet_grid with switch argument to switch orientation
  xlab("Prevalence") +
  scale_color_manual(values = c("CHD" = "red", "Stroke" = "black")) +  # Manual color scale
  scale_shape_manual(values = c("CHD" = 15, "Stroke" = 16)) +  # CHD as square (15) and Stroke as circle (16)
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +  # Reduce space between Variable_Group levels
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 10, angle = 0, hjust = 0.1),  # Center strip text horizontally
    strip.background = element_blank(),  # Remove background of facet labels
    panel.grid = element_blank(),  # Remove grid lines
    axis.ticks.x = element_line(linewidth = 0.5),  # Adjust x-axis ticks
    axis.text.x = element_text(size = 10, color = "black"),  # Adjust x-axis text size
    axis.title.x = element_text(size = 10),  # Adjust x-axis title size
    plot.margin = unit(c(0, 3, 0, 0), "lines"),  # Adjust plot margins
    panel.spacing = unit(1, "lines"),  # Increase spacing between facet panels
    legend.position = c(1.03, 0.5),
    legend.title = element_blank(),
    legend.key.size = unit(0.6, "lines"),  # Reduce the size of legend keys
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(-2, 2.5))  # Set x-axis limits

# Print the forest plot
print(combined_figS7_plot) 

# add header
grid.text("Anomaly Metric", x = unit(0.03, "npc"), y = unit(0.985, "npc"), 
          just = c("left", "top"), gp = gpar(fontsize = 10, fontface = "bold", fontfamily = "Arial"))

#####################################################################################################
###################### The following code creates the supplemental tables #############################
#####################################################################################################
######################################################################################################################
######################################################### Table S3 ##############################################
######## CHD Table S3 ###################
######## calculate mean CHD ########################################################################################
# average CHD
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)
######## CHD 
######## CHD model 1 ####
# Create empty lists for each linear model
coefficients_CHD_1 <- list()
se_CHD_1 <- list()
tvalue_CHD_1 <- list()
lower_bound_1 <- list()
upper_bound_1 <- list()

# model 1  unadjusted 
chd_1 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22
            , data = subset_data)
summary(chd_1)
chd_1_summary <- summary(chd_1)
AIC(chd_1)
BIC(chd_1)
sqrt(mean(chd_1_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_1 <- summary(chd_1)$coefficients[, "Estimate"]
se_CHD_1 <- summary(chd_1)$coefficients[, "Std. Error"]
tvalue_CHD_1 <- summary(chd_1)$coefficients[, "t value"]
lower_bound_1  <-  coefficients_CHD_1 - 1.96 * se_CHD_1 
upper_bound_1  <-  coefficients_CHD_1 + 1.96 * se_CHD_1 

# average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)


## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_1["HI_C_Change70_79_13_22"] / avg_CHD *100

# Combine into a data frame with lower and upper bounds in one column
CHD_1_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_1[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_1[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_1[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_1[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_1_results)

# Round all values in CHD_1_results to 3 decimal places
CHD_1_results$Effect_Size <- round(CHD_1_results$Effect_Size, 2)
CHD_1_results$T_Value <- round(CHD_1_results$T_Value, 2)
CHD_1_results$Percent_Contribution <- round(CHD_1_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_1_results$Confidence_Interval <- as.character(CHD_1_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_1_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_1_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_1_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_1_results or use it further in your analysis
# remove numeric CI
CHD_1_results <- CHD_1_results[, -which(names(CHD_1_results) == "Confidence_Interval")]
# reorder columns
CHD_1_results <- CHD_1_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_1_results)

#write results to csv
write.csv(CHD_1_results, "CHD_S7_model_1.csv", row.names = FALSE)


######## CHD model 2 ##########
coefficients_CHD_2 <- list()
se_CHD_2 <- list()
tvalue_CHD_2 <- list()
lower_bound_2 <- list()
upper_bound_2 <- list()

# model 2 demographic adjustments 
chd_2 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 , data = subset_data)
summary(chd_2)
chd_2_summary <- summary(chd_2)
AIC(chd_2)
BIC(chd_2)
sqrt(mean(chd_2_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_2 <- summary(chd_2)$coefficients[, "Estimate"]
se_CHD_2 <- summary(chd_2)$coefficients[, "Std. Error"]
tvalue_CHD_2 <- summary(chd_2)$coefficients[, "t value"]
lower_bound_2  <-  coefficients_CHD_2 - 1.96 * se_CHD_2 
upper_bound_2  <-  coefficients_CHD_2 + 1.96 * se_CHD_2 

# average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_2["HI_C_Change70_79_13_22"] / avg_CHD *100


# Combine into a data frame with lower and upper bounds in one column
CHD_2_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_2[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_2[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_2[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_2[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_2_results)

# Round all values in CHD_2_results to 3 decimal places
CHD_2_results$Effect_Size <- round(CHD_2_results$Effect_Size, 2)
CHD_2_results$T_Value <- round(CHD_2_results$T_Value, 2)
CHD_2_results$Percent_Contribution <- round(CHD_2_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_2_results$Confidence_Interval <- as.character(CHD_2_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_2_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_2_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_2_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_2_results or use it further in your analysis
# remove numeric CI
CHD_2_results <- CHD_2_results[, -which(names(CHD_2_results) == "Confidence_Interval")]
# reorder columns
CHD_2_results <- CHD_2_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_2_results)

#write results to csv
write.csv(CHD_2_results, "CHD_S7_model_2.csv", row.names = FALSE)

######## CHD model 3 ##########
coefficients_CHD_3 <- list()
se_CHD_3 <- list()
tvalue_CHD_3 <- list()
lower_bound_3 <- list()
upper_bound_3 <- list()
# health adjustments
chd_3 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 , data = subset_data)
summary(chd_3)
chd_3_summary <- summary(chd_3)
AIC(chd_3)
BIC(chd_3)
sqrt(mean(chd_3_summary$residuals^2))

# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_3 <- summary(chd_3)$coefficients[, "Estimate"]
se_CHD_3 <- summary(chd_3)$coefficients[, "Std. Error"]
tvalue_CHD_3 <- summary(chd_3)$coefficients[, "t value"]
lower_bound_3  <-  coefficients_CHD_3 - 1.96 * se_CHD_3 
upper_bound_3  <-  coefficients_CHD_3 + 1.96 * se_CHD_3 

# average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_3["HI_C_Change70_79_13_22"] / avg_CHD *100


# Combine into a data frame with lower and upper bounds in one column
CHD_3_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_3[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_3[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_3[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_3[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_3_results)

# Round all values in CHD_3_results to 3 decimal places
CHD_3_results$Effect_Size <- round(CHD_3_results$Effect_Size, 2)
CHD_3_results$T_Value <- round(CHD_3_results$T_Value, 2)
CHD_3_results$Percent_Contribution <- round(CHD_3_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_3_results$Confidence_Interval <- as.character(CHD_3_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 3 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_3_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_3_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_3_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_3_results or use it further in your analysis
# remove numeric CI
CHD_3_results <- CHD_3_results[, -which(names(CHD_3_results) == "Confidence_Interval")]
# reorder columns
CHD_3_results <- CHD_3_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_3_results)

#write results to csv
write.csv(CHD_3_results, "CHD_S7_model_3.csv", row.names = FALSE)

######## CHD model 4 ##########
coefficients_CHD_4 <- list()
se_CHD_4 <- list()
tvalue_CHD_4 <- list()
lower_bound_4 <- list()
upper_bound_4 <- list()

# health behavior adjustments
chd_4 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
            + CSMOKING_CrudePrev + CHECKUP_CrudePrev, data = subset_data)
summary(chd_4)
chd_4_summary <- summary(chd_4)
AIC(chd_4)
BIC(chd_4)
sqrt(mean(chd_4_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_4 <- summary(chd_4)$coefficients[, "Estimate"]
se_CHD_4 <- summary(chd_4)$coefficients[, "Std. Error"]
tvalue_CHD_4 <- summary(chd_4)$coefficients[, "t value"]
lower_bound_4  <-  coefficients_CHD_4 - 1.96 * se_CHD_4 
upper_bound_4  <-  coefficients_CHD_4 + 1.96 * se_CHD_4 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_4["HI_C_Change70_79_13_22"] / avg_CHD *100

# Combine into a data frame with lower and upper bounds in one column
CHD_4_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_4[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_4[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_4[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_4[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_4_results)

# Round all values in CHD_4_results to 2 decimal places
CHD_4_results$Effect_Size <- round(CHD_4_results$Effect_Size, 2)
CHD_4_results$T_Value <- round(CHD_4_results$T_Value, 2)
CHD_4_results$Percent_Contribution <- round(CHD_4_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_4_results$Confidence_Interval <- as.character(CHD_4_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_4_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_4_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_4_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_4_results or use it further in your analysis
# remove numeric CI
CHD_4_results <- CHD_4_results[, -which(names(CHD_4_results) == "Confidence_Interval")]
# reorder columns
CHD_4_results <- CHD_4_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_4_results)

#write results to csv
write.csv(CHD_4_results, "CHD_S7_model_4.csv", row.names = FALSE)


######## CHD model 5 ##########
coefficients_CHD_5 <- list()
se_CHD_5 <- list()
tvalue_CHD_5 <- list()
lower_bound_5 <- list()
upper_bound_5 <- list()


# environmental adjustments
chd_5 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
            + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces 
            + Temp_C_2013_2022, data = subset_data)
summary(chd_5)
chd_5_summary <- summary(chd_5)
AIC(chd_5)
BIC(chd_5)
sqrt(mean(chd_5_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_5 <- summary(chd_5)$coefficients[, "Estimate"]
se_CHD_5 <- summary(chd_5)$coefficients[, "Std. Error"]
tvalue_CHD_5 <- summary(chd_5)$coefficients[, "t value"]
lower_bound_5  <-  coefficients_CHD_5 - 1.96 * se_CHD_5 
upper_bound_5  <-  coefficients_CHD_5 + 1.96 * se_CHD_5 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_5["HI_C_Change70_79_13_22"] / avg_CHD *100

# Combine into a data frame with lower and upper bounds in one column
CHD_5_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_5[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_5[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_5[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_5[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_5_results)

# Round all values in CHD_5_results to 2 decimal places
CHD_5_results$Effect_Size <- round(CHD_5_results$Effect_Size, 2)
CHD_5_results$T_Value <- round(CHD_5_results$T_Value, 2)
CHD_5_results$Percent_Contribution <- round(CHD_5_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_5_results$Confidence_Interval <- as.character(CHD_5_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 3 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_5_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_5_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_5_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_5_results or use it further in your analysis
# remove numeric CI
CHD_5_results <- CHD_5_results[, -which(names(CHD_5_results) == "Confidence_Interval")]
# reorder columns
CHD_5_results <- CHD_5_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_5_results)

#write results to csv
write.csv(CHD_5_results, "CHD_S7_model_5.csv", row.names = FALSE)

######## CHD model 6 ##########
coefficients_CHD_6 <- list()
se_CHD_6 <- list()
tvalue_CHD_6 <- list()
lower_bound_6 <- list()
upper_bound_6 <- list()
# lc change adjustment
chd_6 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
            + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces 
            + Temp_C_2013_2022 + LCchangeMEAN, data = subset_data)
summary(chd_6)
chd_6_summary <- summary(chd_6)
AIC(chd_6)
BIC(chd_6)
sqrt(mean(chd_6_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_6 <- summary(chd_6)$coefficients[, "Estimate"]
se_CHD_6 <- summary(chd_6)$coefficients[, "Std. Error"]
tvalue_CHD_6 <- summary(chd_6)$coefficients[, "t value"]
lower_bound_6  <-  coefficients_CHD_6 - 1.96 * se_CHD_6 
upper_bound_6  <-  coefficients_CHD_6 + 1.96 * se_CHD_6 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_6["HI_C_Change70_79_13_22"] / avg_CHD *100


# Combine into a data frame with lower and upper bounds in one column
CHD_6_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_6[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_6[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_6[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_6[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_6_results)

# Round all values in CHD_6_results to 2 decimal places
CHD_6_results$Effect_Size <- round(CHD_6_results$Effect_Size, 2)
CHD_6_results$T_Value <- round(CHD_6_results$T_Value, 2)
CHD_6_results$Percent_Contribution <- round(CHD_6_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_6_results$Confidence_Interval <- as.character(CHD_6_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 3 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_6_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_6_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_6_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_6_results or use it further in your analysis
# remove numeric CI
CHD_6_results <- CHD_6_results[, -which(names(CHD_6_results) == "Confidence_Interval")]
# reorder columns
CHD_6_results <- CHD_6_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_6_results)

#write results to csv
write.csv(CHD_6_results, "CHD_S7_model_6.csv", row.names = FALSE)

######## CHD model 7 ##########
coefficients_CHD_7 <- list()
se_CHD_7 <- list()
tvalue_CHD_7 <- list()
lower_bound_7 <- list()
upper_bound_7 <- list()

# mixed effects 
chd_7 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                (1 | CountyFIPS), data = subset_data)
# chd_6 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
#             + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces 
#             + Temp_C_2013_2022 + LCchangeMEAN, data = subset_data)
# chd_5 <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
#             + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces 
#             + Temp_C_2013_2022, data = subset_data)
summary(chd_7)
chd_7_summary <- summary(chd_7)
chd_7_r2 <- r.squaredGLMM(chd_7)
print(chd_7_r2)
AIC(chd_7)
BIC(chd_7)
rmse(chd_7_summary)
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_7 <- summary(chd_7)$coefficients[, "Estimate"]
se_CHD_7 <- summary(chd_7)$coefficients[, "Std. Error"]
tvalue_CHD_7 <- summary(chd_7)$coefficients[, "t value"]
lower_bound_7  <-  coefficients_CHD_7 - 1.96 * se_CHD_7 
upper_bound_7  <-  coefficients_CHD_7 + 1.96 * se_CHD_7 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_7["HI_C_Change70_79_13_22"] / avg_CHD *100


# Combine into a data frame with lower and upper bounds in one column
CHD_7_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_7[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_7[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_7[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_7[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_7_results)

# Round all values in CHD_7_results to 2 decimal places
CHD_7_results$Effect_Size <- round(CHD_7_results$Effect_Size, 2)
CHD_7_results$T_Value <- round(CHD_7_results$T_Value, 2)
CHD_7_results$Percent_Contribution <- round(CHD_7_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_7_results$Confidence_Interval <- as.character(CHD_7_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_7_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_7_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_7_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_7_results or use it further in your analysis
# remove numeric CI
CHD_7_results <- CHD_7_results[, -which(names(CHD_7_results) == "Confidence_Interval")]
# reorder columns
CHD_7_results <- CHD_7_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_7_results)

#write results to csv
write.csv(CHD_7_results, "CHD_S7_model_7.csv", row.names = FALSE)

######## test models for model 8 ################################
## model 8a
library("car")
chd_8 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                windV_diff70_79_13_22 + latent_diff70_79_13_22_P_Mill + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill
              + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                (1 | CountyFIPS), data = subset_data)
summary(chd_8)
chd_8_summary <- summary(chd_8)
vif(chd_8)

## model 8a - removed latent
chd_8 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill
              + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                (1 | CountyFIPS), data = subset_data)
summary(chd_8)
chd_8_summary <- summary(chd_8)
vif(chd_8)

## model 8a - removed sensible 
chd_8 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill 
              + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                (1 | CountyFIPS), data = subset_data)
summary(chd_8)
chd_8_summary <- summary(chd_8)
vif(chd_8)
chd_8_summary <- summary(chd_8)
vif(chd_8)

## model 8a - removed thermal
chd_8 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill  
              + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                (1 | CountyFIPS), data = subset_data)
summary(chd_8)
chd_8_summary <- summary(chd_8)
vif(chd_8)


## model 8b
chd_8b <- lm(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
               windV_diff70_79_13_22 + latent_diff70_79_13_22_P_Mill + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + 
               +  pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000, data = subset_data)
summary(chd_8b)
chd_8b_summary <- summary(chd_8b)
vif(chd_8)

######## CHD model 8 ##########
coefficients_CHD_8 <- list()
se_CHD_8 <- list()
tvalue_CHD_8 <- list()
lower_bound_8 <- list()
upper_bound_8 <- list()

## model 8 final 
chd_8 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill +  
                +  pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 +
                (1 | CountyFIPS), data = subset_data)
summary(chd_8)
chd_8_summary <- summary(chd_8)
chd_8_r2 <- r.squaredGLMM(chd_8)
print(chd_8_r2)
AIC(chd_8)
BIC(chd_8)
rmse(chd_8_summary)
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD_8 <- summary(chd_8)$coefficients[, "Estimate"]
se_CHD_8 <- summary(chd_8)$coefficients[, "Std. Error"]
tvalue_CHD_8 <- summary(chd_8)$coefficients[, "t value"]
lower_bound_8  <-  coefficients_CHD_8 - 1.96 * se_CHD_8 
upper_bound_8  <-  coefficients_CHD_8 + 1.96 * se_CHD_8 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD_8["HI_C_Change70_79_13_22"] / avg_CHD *100

# Combine into a data frame with lower and upper bounds in one column
CHD_8_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_CHD_8[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_CHD_8[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_8[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_8[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(CHD_8_results)

# Round all values in CHD_8_results to 2 decimal places
CHD_8_results$Effect_Size <- round(CHD_8_results$Effect_Size, 2)
CHD_8_results$T_Value <- round(CHD_8_results$T_Value, 2)
CHD_8_results$Percent_Contribution <- round(CHD_8_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
CHD_8_results$Confidence_Interval <- as.character(CHD_8_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 3 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", CHD_8_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", CHD_8_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
CHD_8_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print CHD_8_results or use it further in your analysis
# remove numeric CI
CHD_8_results <- CHD_8_results[, -which(names(CHD_8_results) == "Confidence_Interval")]
# reorder columns
CHD_8_results <- CHD_8_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(CHD_8_results)
#write results to csv
write.csv(CHD_8_results, "CHD_S7_model_8.csv", row.names = FALSE)

######## CHD run performance package ###############################
performance::compare_performance(chd_1, chd_2, chd_3, chd_4, chd_5, chd_6, chd_7, chd_8, rank =  TRUE)

################################################## Stroke 
######## Stroke Table S3 #############################################
# average stroke
avg_stroke <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

######## Stroke model 1 ####
# Create empty lists for each linear model
coefficients_stroke_1 <- list()
se_stroke_1 <- list()
tvalue_stroke_1 <- list()
lower_bound_1 <- list()
upper_bound_1 <- list()

# model 1  unadjusted 
stroke_1 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22
               , data = subset_data)
summary(stroke_1)
stroke_1_summary <- summary(stroke_1)
AIC(stroke_1)
BIC(stroke_1)
sqrt(mean(stroke_1_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_1 <- summary(stroke_1)$coefficients[, "Estimate"]
se_stroke_1 <- summary(stroke_1)$coefficients[, "Std. Error"]
tvalue_stroke_1 <- summary(stroke_1)$coefficients[, "t value"]
lower_bound_1  <-  coefficients_stroke_1 - 1.96 * se_stroke_1 
upper_bound_1  <-  coefficients_stroke_1 + 1.96 * se_stroke_1 

# average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)


## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_1["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_1_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_1[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_1[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_1[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_1[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_1_results)

# Round all values in stroke_1_results to 2 decimal places
stroke_1_results$Effect_Size <- round(stroke_1_results$Effect_Size, 2)
stroke_1_results$T_Value <- round(stroke_1_results$T_Value, 2)
stroke_1_results$Percent_Contribution <- round(stroke_1_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_1_results$Confidence_Interval <- as.character(stroke_1_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_1_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_1_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_1_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_1_results or use it further in your analysis
# remove numeric CI
stroke_1_results <- stroke_1_results[, -which(names(stroke_1_results) == "Confidence_Interval")]
# reorder columns
stroke_1_results <- stroke_1_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_1_results)

#write results to csv
write.csv(stroke_1_results, "stroke_S7_model_1.csv", row.names = FALSE)


######## Stroke model 2 ##########
coefficients_stroke_2 <- list()
se_stroke_2 <- list()
tvalue_stroke_2 <- list()
lower_bound_2 <- list()
upper_bound_2 <- list()

# model 2 demographic adjustments 
stroke_2 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 , data = subset_data)
summary(stroke_2)
stroke_2_summary <- summary(stroke_2)
AIC(stroke_2)
BIC(stroke_2)
sqrt(mean(stroke_2_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_2 <- summary(stroke_2)$coefficients[, "Estimate"]
se_stroke_2 <- summary(stroke_2)$coefficients[, "Std. Error"]
tvalue_stroke_2 <- summary(stroke_2)$coefficients[, "t value"]
lower_bound_2  <-  coefficients_stroke_2 - 1.96 * se_stroke_2 
upper_bound_2  <-  coefficients_stroke_2 + 1.96 * se_stroke_2 

#average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_2["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_2_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_2[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_2[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_2[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_2[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_2_results)

# Round all values in stroke_2_results to 2 decimal places
stroke_2_results$Effect_Size <- round(stroke_2_results$Effect_Size, 2)
stroke_2_results$T_Value <- round(stroke_2_results$T_Value, 2)
stroke_2_results$Percent_Contribution <- round(stroke_2_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_2_results$Confidence_Interval <- as.character(stroke_2_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_2_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_2_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_2_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_2_results or use it further in your analysis
# remove numeric CI
stroke_2_results <- stroke_2_results[, -which(names(stroke_2_results) == "Confidence_Interval")]
# reorder columns
stroke_2_results <- stroke_2_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_2_results)

#write results to csv
write.csv(stroke_2_results, "stroke_S7_model_2.csv", row.names = FALSE)

######## Stroke model 3 ##########
coefficients_stroke_3 <- list()
se_stroke_3 <- list()
tvalue_stroke_3 <- list()
lower_bound_3 <- list()
upper_bound_3 <- list()

# health adjustments
stroke_3 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10, data = subset_data)
summary(stroke_3)
stroke_3_summary <- summary(stroke_3)
AIC(stroke_3)
BIC(stroke_3)
sqrt(mean(stroke_3_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_3 <- summary(stroke_3)$coefficients[, "Estimate"]
se_stroke_3 <- summary(stroke_3)$coefficients[, "Std. Error"]
tvalue_stroke_3 <- summary(stroke_3)$coefficients[, "t value"]
lower_bound_3  <-  coefficients_stroke_3 - 1.96 * se_stroke_3 
upper_bound_3  <-  coefficients_stroke_3 + 1.96 * se_stroke_3 

#average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_3["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_3_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_3[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_3[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_3[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_3[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_3_results)

# Round all values in stroke_2_results to 2 decimal places
stroke_3_results$Effect_Size <- round(stroke_3_results$Effect_Size, 2)
stroke_3_results$T_Value <- round(stroke_3_results$T_Value, 2)
stroke_3_results$Percent_Contribution <- round(stroke_3_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_3_results$Confidence_Interval <- as.character(stroke_3_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_3_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_3_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_3_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_3_results or use it further in your analysis
# remove numeric CI
stroke_3_results <- stroke_3_results[, -which(names(stroke_3_results) == "Confidence_Interval")]
# reorder columns
stroke_3_results <- stroke_3_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_3_results)

#write results to csv
write.csv(stroke_3_results, "stroke_S7_model_3.csv", row.names = FALSE)

######## Stroke model 4 ##########
coefficients_stroke_4 <- list()
se_stroke_4 <- list()
tvalue_stroke_4 <- list()
lower_bound_4 <- list()
upper_bound_4 <- list()

# health behavior adjustments
stroke_4 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
               + CSMOKING_CrudePrev + CHECKUP_CrudePrev, data = subset_data)
summary(stroke_4)
stroke_4_summary <- summary(stroke_4)
AIC(stroke_4)
BIC(stroke_4)
sqrt(mean(stroke_4_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_4 <- summary(stroke_4)$coefficients[, "Estimate"]
se_stroke_4 <- summary(stroke_4)$coefficients[, "Std. Error"]
tvalue_stroke_4 <- summary(stroke_4)$coefficients[, "t value"]
lower_bound_4  <-  coefficients_stroke_4 - 1.96 * se_stroke_4 
upper_bound_4  <-  coefficients_stroke_4 + 1.96 * se_stroke_4 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_4["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_4_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_4[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_4[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_4[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_4[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_4_results)

# Round all values in stroke_4_results to 2 decimal places
stroke_4_results$Effect_Size <- round(stroke_4_results$Effect_Size, 2)
stroke_4_results$T_Value <- round(stroke_4_results$T_Value, 2)
stroke_4_results$Percent_Contribution <- round(stroke_4_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_4_results$Confidence_Interval <- as.character(stroke_4_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_4_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_4_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_4_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_4_results or use it further in your analysis
# remove numeric CI
stroke_4_results <- stroke_4_results[, -which(names(stroke_4_results) == "Confidence_Interval")]
# reorder columns
stroke_4_results <- stroke_4_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_4_results)

#write results to csv
write.csv(stroke_4_results, "stroke_S7_model_4.csv", row.names = FALSE)


######## Stroke model 5 ##########
coefficients_stroke_5 <- list()
se_stroke_5 <- list()
tvalue_stroke_5 <- list()
lower_bound_5 <- list()
upper_bound_5 <- list()


# environmental adjustments
stroke_5 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
               + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces 
               + Temp_C_2013_2022, data = subset_data)
summary(stroke_5)
stroke_5_summary <- summary(stroke_5)
AIC(stroke_5)
BIC(stroke_5)
sqrt(mean(stroke_5_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_5 <- summary(stroke_5)$coefficients[, "Estimate"]
se_stroke_5 <- summary(stroke_5)$coefficients[, "Std. Error"]
tvalue_stroke_5 <- summary(stroke_5)$coefficients[, "t value"]
lower_bound_5  <-  coefficients_stroke_5 - 1.96 * se_stroke_5 
upper_bound_5  <-  coefficients_stroke_5 + 1.96 * se_stroke_5 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_5["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_5_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_5[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_5[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_5[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_5[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_5_results)

# Round all values in stroke_5_results to 2 decimal places
stroke_5_results$Effect_Size <- round(stroke_5_results$Effect_Size, 2)
stroke_5_results$T_Value <- round(stroke_5_results$T_Value, 2)
stroke_5_results$Percent_Contribution <- round(stroke_5_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_5_results$Confidence_Interval <- as.character(stroke_5_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_5_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_5_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_5_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_5_results or use it further in your analysis
# remove numeric CI
stroke_5_results <- stroke_5_results[, -which(names(stroke_5_results) == "Confidence_Interval")]
# reorder columns
stroke_5_results <- stroke_5_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_5_results)

#write results to csv
write.csv(stroke_5_results, "stroke_S7_model_5.csv", row.names = FALSE)

######## Stroke model 6 ##########
coefficients_stroke_6 <- list()
se_stroke_6 <- list()
tvalue_stroke_6 <- list()
lower_bound_6 <- list()
upper_bound_6 <- list()
# lc change adjustment
stroke_6 <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + SPL_THEME1 + EP_AGE65 + OBESITY_CrudePrev_P_div10 
               + CSMOKING_CrudePrev + CHECKUP_CrudePrev +  PCT_ImperviousSurfaces + LCchangeMEAN
               + Temp_C_2013_2022, data = subset_data)
summary(stroke_6)
stroke_6_summary <- summary(stroke_6)
AIC(stroke_6)
BIC(stroke_6)
sqrt(mean(stroke_6_summary$residuals^2))
# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_6 <- summary(stroke_6)$coefficients[, "Estimate"]
se_stroke_6 <- summary(stroke_6)$coefficients[, "Std. Error"]
tvalue_stroke_6 <- summary(stroke_6)$coefficients[, "t value"]
lower_bound_6  <-  coefficients_stroke_6 - 1.96 * se_stroke_6 
upper_bound_6  <-  coefficients_stroke_6 + 1.96 * se_stroke_6 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_6["HI_C_Change70_79_13_22"] / avg_stroke *100


# Combine into a data frame with lower and upper bounds in one column
stroke_6_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_6[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_6[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_6[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_6[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_6_results)

# Round all values in stroke_6_results to 2 decimal places
stroke_6_results$Effect_Size <- round(stroke_6_results$Effect_Size, 2)
stroke_6_results$T_Value <- round(stroke_6_results$T_Value, 2)
stroke_6_results$Percent_Contribution <- round(stroke_6_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_6_results$Confidence_Interval <- as.character(stroke_6_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_6_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_6_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_6_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_6_results or use it further in your analysis
# remove numeric CI
stroke_6_results <- stroke_6_results[, -which(names(stroke_6_results) == "Confidence_Interval")]
# reorder columns
stroke_6_results <- stroke_6_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_6_results)

#write results to csv
write.csv(stroke_6_results, "stroke_S7_model_6.csv", row.names = FALSE)

######## Stroke model 7 ##########
coefficients_stroke_7 <- list()
se_stroke_7 <- list()
tvalue_stroke_7 <- list()
lower_bound_7 <- list()
upper_bound_7 <- list()

# mixed effects 
stroke_7 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_7)
stroke_7_summary <- summary(stroke_7)
stroke_7_r2 <- r.squaredGLMM(stroke_7)
print(stroke_7_r2)
AIC(stroke_7)
BIC(stroke_7)

# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_7 <- summary(stroke_7)$coefficients[, "Estimate"]
se_stroke_7 <- summary(stroke_7)$coefficients[, "Std. Error"]
tvalue_stroke_7 <- summary(stroke_7)$coefficients[, "t value"]
lower_bound_7  <-  coefficients_stroke_7 - 1.96 * se_stroke_7 
upper_bound_7  <-  coefficients_stroke_7 + 1.96 * se_stroke_7 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_7["HI_C_Change70_79_13_22"] / avg_stroke *100

# Combine into a data frame with lower and upper bounds in one column
stroke_7_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_7[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_7[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_7[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_7[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_7_results)

# Round all values in stroke_7_results to 2 decimal places
stroke_7_results$Effect_Size <- round(stroke_7_results$Effect_Size, 2)
stroke_7_results$T_Value <- round(stroke_7_results$T_Value, 2)
stroke_7_results$Percent_Contribution <- round(stroke_7_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_7_results$Confidence_Interval <- as.character(stroke_7_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_7_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_7_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_7_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_7_results or use it further in your analysis
# remove numeric CI
stroke_7_results <- stroke_7_results[, -which(names(stroke_7_results) == "Confidence_Interval")]
# reorder columns
stroke_7_results <- stroke_7_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_7_results)

#write results to csv
write.csv(stroke_7_results, "stroke_S7_model_7.csv", row.names = FALSE)

######## test models for model 8 ################################
## model 8a
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + latent_diff70_79_13_22_P_Mill + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill
                 + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
vif(stroke_8)

## model 8a - removed latent 
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill
                 + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
vif(stroke_8)

## model 8a - removed sensible 
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill 
                 + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
vif(stroke_8)

## model 8a - removed thermal
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill 
                 + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
vif(stroke_8)

## model 8a - removed evaporation
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill 
                 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
vif(stroke_8)

## model 8b
stroke_8b <- lm(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                  windV_diff70_79_13_22 + latent_diff70_79_13_22_P_Mill + solar_diff70_79_13_22_P_Mill + thermal_diff70_79_13_22_P_Mill + 
                  +  pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000, data = subset_data)
summary(stroke_8b)
stroke_8b_summary <- summary(stroke_8b)
vif(stroke_8)

######## Stroke model 8 ##########
coefficients_stroke_8 <- list()
se_stroke_8 <- list()
tvalue_stroke_8 <- list()
lower_bound_8 <- list()
upper_bound_8 <- list()

## model 8 final 
stroke_8 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + windU_diff70_79_13_22 +
                   windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill 
                 + pressure_diff70_79_13_22_P_div10 + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000+
                   (1 | CountyFIPS), data = subset_data)
summary(stroke_8)
stroke_8_summary <- summary(stroke_8)
stroke_8_r2 <- r.squaredGLMM(stroke_8)
print(stroke_8_r2)
AIC(stroke_8)
BIC(stroke_8)

# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke_8 <- summary(stroke_8)$coefficients[, "Estimate"]
se_stroke_8 <- summary(stroke_8)$coefficients[, "Std. Error"]
tvalue_stroke_8 <- summary(stroke_8)$coefficients[, "t value"]
lower_bound_8  <-  coefficients_stroke_8 - 1.96 * se_stroke_8 
upper_bound_8  <-  coefficients_stroke_8 + 1.96 * se_stroke_8 

# anomaly size
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke_8["HI_C_Change70_79_13_22"] / avg_stroke *100

# Combine into a data frame with lower and upper bounds in one column
stroke_8_results <- data.frame(
  Variable = c("Heat Index"),
  Effect_Size = coefficients_stroke_8[c("HI_C_Change70_79_13_22")],
  T_Value = tvalue_stroke_8[c("HI_C_Change70_79_13_22")],
  Confidence_Interval = paste("(", lower_bound_8[c("HI_C_Change70_79_13_22")],
                              ", ", upper_bound_8[c("HI_C_Change70_79_13_22")], ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_79_13_22)
)

# Print or further process the results
print(stroke_8_results)

# Round all values in stroke_8_results to 2 decimal places
stroke_8_results$Effect_Size <- round(stroke_8_results$Effect_Size, 2)
stroke_8_results$T_Value <- round(stroke_8_results$T_Value, 2)
stroke_8_results$Percent_Contribution <- round(stroke_8_results$Percent_Contribution, 2)
# Convert CI column to character to avoid rounding issues
stroke_8_results$Confidence_Interval <- as.character(stroke_8_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", stroke_8_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", stroke_8_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
stroke_8_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# Print stroke_8_results or use it further in your analysis
# remove numeric CI
stroke_8_results <- stroke_8_results[, -which(names(stroke_8_results) == "Confidence_Interval")]
# reorder columns
stroke_8_results <- stroke_8_results %>%
  select(Variable, Effect_Size, T_Value, CI, Percent_Contribution)

print(stroke_8_results)

#write results to csv
write.csv(stroke_8_results, "stroke_S7_model_8.csv", row.names = FALSE)

######## Stroke run performance package to get AIC and BIC ############################################
performance::compare_performance(stroke_1, stroke_2, stroke_3, stroke_4, stroke_5, stroke_6, stroke_7, stroke_8, rank =  TRUE)


################################################# Table S4 ###########################################################
######################################################################################################################
####### join HI 1970-1989, 2003-2022#######################
setwd(working_directory_data)
sensitivity70_89_03_22 <- read.csv("sensitivity_70_89_03_22_07_24_2024.csv", header = TRUE,  na.strings = "NA")
setwd(working_directory_output)
print(sensitivity70_89_03_22)
# Sort frame by CLIMDIV
sensitivity70_89_03_22_sorted <- sensitivity70_89_03_22[order(sensitivity70_89_03_22$CLIMDIV), ]
sensitivity70_89_03_22_sorted <- sensitivity70_89_03_22_sorted %>%
  mutate(CLIMDIV = as.numeric(CLIMDIV))

# Merge results with sensitivity sorted by CLIMDIV
sensitivity70_89_03_22_merged <- merge(subset_data, sensitivity70_89_03_22_sorted, by = "CLIMDIV")
print(sensitivity70_89_03_22_merged)

################################### CHD sensitivity analysis ##########################################
####### CHD mixed effects model 1970-1980 vs 2013-2022 ####################
# HI change
chd_hi_70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(chd_hi_70_79_13_22)
chd_hi_70_79_13_22 <- summary(chd_hi_70_79_13_22)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_hi_70_79_13_22 <- chd_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "Estimate"]
se_chd_hi_70_79_13_22 <- chd_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "Std. Error"]
tvalue_chd_hi_70_79_13_22 <- chd_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "t value"]
lower_bound_chd_hi_70_79_13_22 <- coefficients_chd_hi_70_79_13_22 - 1.96 * se_chd_hi_70_79_13_22
upper_bound_chd_hi_70_79_13_22 <- coefficients_chd_hi_70_79_13_22 + 1.96 * se_chd_hi_70_79_13_22

# Create a data frame for results
chd_hi_70_79_13_22_results <- data.frame(
  Variable = "Heat  Index   1970-1979 vs 2013-2022",
  Effect_Size = coefficients_chd_hi_70_79_13_22,
  Standard_Error = se_chd_hi_70_79_13_22,
  T_Value = tvalue_chd_hi_70_79_13_22,
  Confidence_Interval = paste("(", lower_bound_chd_hi_70_79_13_22, ", ", upper_bound_chd_hi_70_79_13_22, ")", sep = "")
)

print(chd_hi_70_79_13_22_results)

# Round all values in chd_hi_70_79_13_22_results to 2 decimal places
chd_hi_70_79_13_22_results$Effect_Size <- round(chd_hi_70_79_13_22_results$Effect_Size, 2)
chd_hi_70_79_13_22_results$Standard_Error <- round(chd_hi_70_79_13_22_results$Standard_Error, 2)
chd_hi_70_79_13_22_results$T_Value <- round(chd_hi_70_79_13_22_results$T_Value, 2)
# Convert CI column to character to avoid rounding issues
chd_hi_70_79_13_22_results$Confidence_Interval <- as.character(chd_hi_70_79_13_22_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", chd_hi_70_79_13_22_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", chd_hi_70_79_13_22_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 2 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
chd_hi_70_79_13_22_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# remove numeric CI
chd_hi_70_79_13_22_results <- chd_hi_70_79_13_22_results[, -which(names(chd_hi_70_79_13_22_results) == "Confidence_Interval")]
# reorder columns
chd_hi_70_79_13_22_results <- chd_hi_70_79_13_22_results %>%
  dplyr::select(Variable, Effect_Size, Standard_Error, T_Value, CI)

print(chd_hi_70_79_13_22_results)

# Write results to CSV
write.csv(chd_hi_70_79_13_22_results, file = "chd_sensitvity_70_79_13_22.csv", row.names = FALSE)

####### CHD mixed effects model 1970-1989 vs 2003-2022 ###########################
# HI change
chd_hi_70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022.x + LCchangeMEAN +
                             (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(chd_hi_70_89_03_22 )
chd_hi_70_89_03_22_summary <- summary(chd_hi_70_89_03_22)

# Extracting coefficients, standard errors and confidence intervals
coefficients_chd_hi_70_89_03_22 <- chd_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "Estimate"]
se_chd_hi_70_89_03_22  <- chd_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "Std. Error"]
tvalue_chd_hi_70_89_03_22  <- chd_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "t value"]
lower_bound_chd_hi_70_89_03_22  <- coefficients_chd_hi_70_89_03_22  - 1.96 * se_chd_hi_70_89_03_22 
upper_bound_chd_hi_70_89_03_22  <- coefficients_chd_hi_70_89_03_22  + 1.96 * se_chd_hi_70_89_03_22 

# Create a data frame for results
chd_hi_70_89_03_22_results <- data.frame(
  Variable = "Heat  Index 1970-1989 vs 2003-2022",
  Effect_Size = coefficients_chd_hi_70_89_03_22,
  Standard_Error = se_chd_hi_70_89_03_22,
  T_Value = tvalue_chd_hi_70_89_03_22,
  Confidence_Interval = paste("(", lower_bound_chd_hi_70_89_03_22, ", ", upper_bound_chd_hi_70_89_03_22, ")", sep = "")
)

print(chd_hi_70_89_03_22_results)

# Round all values in chd_hi_70_89_03_22_results to 2 decimal places
chd_hi_70_89_03_22_results$Effect_Size <- round(chd_hi_70_89_03_22_results$Effect_Size, 2)
chd_hi_70_89_03_22_results$Standard_Error <- round(chd_hi_70_89_03_22_results$Standard_Error, 2)
chd_hi_70_89_03_22_results$T_Value <- round(chd_hi_70_89_03_22_results$T_Value, 2)
# Convert CI column to character to avoid rounding issues
chd_hi_70_89_03_22_results$Confidence_Interval <- as.character(chd_hi_70_89_03_22_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", chd_hi_70_89_03_22_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", chd_hi_70_89_03_22_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
chd_hi_70_89_03_22_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# remove numeric CI
chd_hi_70_89_03_22_results <- chd_hi_70_89_03_22_results[, -which(names(chd_hi_70_89_03_22_results) == "Confidence_Interval")]
# reorder columns
chd_hi_70_89_03_22_results <- chd_hi_70_89_03_22_results %>%
  dplyr::select(Variable, Effect_Size, Standard_Error, T_Value, CI)

print(chd_hi_70_89_03_22_results)
# Write results to CSV
write.csv(chd_hi_70_89_03_22_results, file = "chd_sensitivity_70_89_03_22_08_27_2024.csv", row.names = FALSE)
################################### Stroke sensitivity analysis ##########################################
####### Stroke mixed effects model 1970-1979 vs 2013-2022 ####################
# HI change
STROKE_hi_70_79_13_22 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)
summary(STROKE_hi_70_79_13_22)
STROKE_hi_70_79_13_22 <- summary(STROKE_hi_70_79_13_22)

# Extracting coefficients, standard errors and confidence intervals
coefficients_STROKE_hi_70_79_13_22 <- STROKE_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "Estimate"]
se_STROKE_hi_70_79_13_22 <- STROKE_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "Std. Error"]
tvalue_STROKE_hi_70_79_13_22 <- STROKE_hi_70_79_13_22$coefficients["HI_C_Change70_79_13_22", "t value"]
lower_bound_STROKE_hi_70_79_13_22 <- coefficients_STROKE_hi_70_79_13_22 - 1.96 * se_STROKE_hi_70_79_13_22
upper_bound_STROKE_hi_70_79_13_22 <- coefficients_STROKE_hi_70_79_13_22 + 1.96 * se_STROKE_hi_70_79_13_22

# Create a data frame for results
STROKE_hi_70_79_13_22_results <- data.frame(
  Variable = "Heat  Index   1970-1979 vs 2013-2022",
  Effect_Size = coefficients_STROKE_hi_70_79_13_22,
  Standard_Error = se_STROKE_hi_70_79_13_22,
  T_Value = tvalue_STROKE_hi_70_79_13_22,
  Confidence_Interval = paste("(", lower_bound_STROKE_hi_70_79_13_22, ", ", upper_bound_STROKE_hi_70_79_13_22, ")", sep = "")
)

print(STROKE_hi_70_79_13_22_results)

# Round all values in STROKE_hi_70_79_13_22_results to 2 decimal places
STROKE_hi_70_79_13_22_results$Effect_Size <- round(STROKE_hi_70_79_13_22_results$Effect_Size, 2)
STROKE_hi_70_79_13_22_results$Standard_Error <- round(STROKE_hi_70_79_13_22_results$Standard_Error, 2)
STROKE_hi_70_79_13_22_results$T_Value <- round(STROKE_hi_70_79_13_22_results$T_Value, 2)
# Convert CI column to character to avoid rounding issues
STROKE_hi_70_79_13_22_results$Confidence_Interval <- as.character(STROKE_hi_70_79_13_22_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", STROKE_hi_70_79_13_22_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", STROKE_hi_70_79_13_22_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
STROKE_hi_70_79_13_22_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# remove numeric CI
STROKE_hi_70_79_13_22_results <- STROKE_hi_70_79_13_22_results[, -which(names(STROKE_hi_70_79_13_22_results) == "Confidence_Interval")]
# reorder columns
STROKE_hi_70_79_13_22_results <- STROKE_hi_70_79_13_22_results %>%
  select(Variable, Effect_Size, Standard_Error, T_Value, CI)

print(STROKE_hi_70_79_13_22_results)
# Write results to CSV
write.csv(STROKE_hi_70_79_13_22_results, file = "STROKE_sensitivity_70_79_13_22_07_08_2024.csv", row.names = FALSE)

####### Stroke mixed effects model 1970-1989 vs 2003-2022 ###########################
# HI change
STROKE_hi_70_89_03_22 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_89_03_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022.x + LCchangeMEAN +
                                (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(STROKE_hi_70_89_03_22 )
STROKE_hi_70_89_03_22_summary <- summary(STROKE_hi_70_89_03_22)

# Extracting coefficients, standard errors and confidence intervals
coefficients_STROKE_hi_70_89_03_22 <- STROKE_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "Estimate"]
se_STROKE_hi_70_89_03_22  <- STROKE_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "Std. Error"]
tvalue_STROKE_hi_70_89_03_22  <- STROKE_hi_70_89_03_22_summary$coefficients["HI_C_Change70_89_03_22", "t value"]
lower_bound_STROKE_hi_70_89_03_22  <- coefficients_STROKE_hi_70_89_03_22  - 1.96 * se_STROKE_hi_70_89_03_22 
upper_bound_STROKE_hi_70_89_03_22  <- coefficients_STROKE_hi_70_89_03_22  + 1.96 * se_STROKE_hi_70_89_03_22 

# Create a data frame for results
STROKE_hi_70_89_03_22_results <- data.frame(
  Variable = "Heat  Index 1970-1989 vs 2003-2022",
  Effect_Size = coefficients_STROKE_hi_70_89_03_22,
  Standard_Error = se_STROKE_hi_70_89_03_22,
  T_Value = tvalue_STROKE_hi_70_89_03_22,
  Confidence_Interval = paste("(", lower_bound_STROKE_hi_70_89_03_22, ", ", upper_bound_STROKE_hi_70_89_03_22, ")", sep = "")
)

print(STROKE_hi_70_89_03_22_results)

# Round all values in STROKE_hi_70_89_03_22_results to 2 decimal places
STROKE_hi_70_89_03_22_results$Effect_Size <- round(STROKE_hi_70_89_03_22_results$Effect_Size, 2)
STROKE_hi_70_89_03_22_results$Standard_Error <- round(STROKE_hi_70_89_03_22_results$Standard_Error, 2)
STROKE_hi_70_89_03_22_results$T_Value <- round(STROKE_hi_70_89_03_22_results$T_Value, 2)
# Convert CI column to character to avoid rounding issues
STROKE_hi_70_89_03_22_results$Confidence_Interval <- as.character(STROKE_hi_70_89_03_22_results$Confidence_Interval)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds <- sub("[(](.*),.*", "\\1", STROKE_hi_70_89_03_22_results$Confidence_Interval)
upper_bounds <- sub(".*, (.*)[)]", "\\1", STROKE_hi_70_89_03_22_results$Confidence_Interval)

# Handle NAs by replacing them with 0
lower_bounds[is.na(lower_bounds)] <- 0
upper_bounds[is.na(upper_bounds)] <- 0

# Round the bounds to 3 decimal places
lower_bounds <- round(as.numeric(lower_bounds), 2)
upper_bounds <- round(as.numeric(upper_bounds), 2)

# Combine the rounded bounds into the CI column
STROKE_hi_70_89_03_22_results$CI <- paste0("(", lower_bounds, ", ", upper_bounds, ")")

# remove numeric CI
STROKE_hi_70_89_03_22_results <- STROKE_hi_70_89_03_22_results[, -which(names(STROKE_hi_70_89_03_22_results) == "Confidence_Interval")]
# reorder columns
STROKE_hi_70_89_03_22_results <- STROKE_hi_70_89_03_22_results %>%
  select(Variable, Effect_Size, Standard_Error, T_Value, CI)

print(STROKE_hi_70_89_03_22_results)
# Write results to CSV
write.csv(STROKE_hi_70_89_03_22_results, file = "STROKE_sensitivity_70_89_03_22_08_27_2024.csv", row.names = FALSE)
#######################################################################################################################
################################################ CHD Table S4 ############################################################
###### join HI 1970-1989, 2003-2022#######################
setwd(working_directory_data)
sensitivity70_89_03_22 <- read.csv("sensitivity_70_89_03_22_07_24_2024.csv", header = TRUE,  na.strings = "NA")
setwd(working_directory_output)
print(sensitivity70_89_03_22)
# Sort frame by CLIMDIV
sensitivity70_89_03_22_sorted <- sensitivity70_89_03_22[order(sensitivity70_89_03_22$CLIMDIV), ]
sensitivity70_89_03_22_sorted <- sensitivity70_89_03_22_sorted %>%
  mutate(CLIMDIV = as.numeric(CLIMDIV))

# Merge results with sensitivity sorted by CLIMDIV
sensitivity70_89_03_22_merged <- merge(subset_data, sensitivity70_89_03_22_sorted, by = "CLIMDIV")
print(sensitivity70_89_03_22_merged)

###### optimized model development #####################################################
# check for colinearity #
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + latent_diff70_89_03_22 + solar_diff70_89_03_22
                       + thermal_diff70_89_03_22 + sensible_diff70_89_03_22 + evap_diff70_89_03_22 + pressure_diff70_89_03_22 
                       + precip_diff70_89_03_22 + transpir_diff70_89_03_22 + LCchangeMEAN
                       + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

# check for colinearity # remove latent
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22
                       + thermal_diff70_89_03_22 + sensible_diff70_89_03_22 + evap_diff70_89_03_22 + pressure_diff70_89_03_22 
                       + precip_diff70_89_03_22 + transpir_diff70_89_03_22 + LCchangeMEAN
                       + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

# check for colinearity # remove sensible
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22
                       + thermal_diff70_89_03_22 + evap_diff70_89_03_22 + pressure_diff70_89_03_22 
                       + precip_diff70_89_03_22 + transpir_diff70_89_03_22 + LCchangeMEAN
                       + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

# check for colinearity # remove thermal
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22
                       + evap_diff70_89_03_22 + pressure_diff70_89_03_22 
                       + precip_diff70_89_03_22 + transpir_diff70_89_03_22 + LCchangeMEAN
                       + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

# remove evaporation
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22
                       + pressure_diff70_89_03_22 
                       + precip_diff70_89_03_22 + transpir_diff70_89_03_22 + LCchangeMEAN
                       + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

###### scale the variables ################
sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale <- sensitivity70_89_03_22_merged$pressure_diff70_89_03_22 /100
sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale <- sensitivity70_89_03_22_merged$solar_diff70_89_03_22 /10000000
sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale <- sensitivity70_89_03_22_merged$precip_diff70_89_03_22 *10
sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale <- sensitivity70_89_03_22_merged$transpir_diff70_89_03_22 *10000

summary(sensitivity70_89_03_22_merged$HI_C_Change70_89_03_22)
summary(sensitivity70_89_03_22_merged$windU_diff70_89_03_22)
summary(sensitivity70_89_03_22_merged$windV_diff70_89_03_22)
summary(sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale)
summary(sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale)
summary(sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale)
summary(sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale)
###### Run optimized model #####################################
# 70-79
coefficients_CHD70_89_03_22 <- list()
se_CHD70_89_03_22 <- list()
tvalue_CHD70_89_03_22 <- list()
lower_bound_CHD70_89_03_22 <- list()
upper_bound_CHD70_89_03_22 <- list()

# final model - optimized for 20 year period
CHD70_89_03_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_89_03_22 + 
                         Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22_scale
                       + pressure_diff70_89_03_22_scale 
                       + precip_diff70_89_03_22_scale + transpir_diff70_89_03_22_scale + LCchangeMEAN
                       + (1| CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(CHD70_89_03_22)
vif(CHD70_89_03_22)

# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD70_89_03_22 <- summary(CHD70_89_03_22)$coefficients[, "Estimate"]
se_CHD70_89_03_22 <- summary(CHD70_89_03_22)$coefficients[, "Std. Error"]
tvalue_CHD70_89_03_22 <- summary(CHD70_89_03_22)$coefficients[, "t value"]
lower_bound_CHD70_89_03_22  <-  coefficients_CHD70_89_03_22 - 1.96 * se_CHD70_89_03_22 
upper_bound_CHD70_89_03_22  <-  coefficients_CHD70_89_03_22 + 1.96 * se_CHD70_89_03_22 

# average CHD
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)

#average anomaly size
# 1970-1980
avg_HI_C_Change70_89_03_22 <- mean(sensitivity70_89_03_22_merged$HI_C_Change70_89_03_22, na.rm = TRUE)
avg_windU_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windU_diff70_89_03_22, na.rm = TRUE)
avg_windV_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windV_diff70_89_03_22, na.rm = TRUE)
avg_solar_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale, na.rm = TRUE)
avg_pressure_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale, na.rm = TRUE)
avg_precip_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale, na.rm = TRUE)
avg_transpir_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale, na.rm = TRUE)


## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_89_03_22 <- avg_HI_C_Change70_89_03_22 * coefficients_CHD70_89_03_22["HI_C_Change70_89_03_22"] / avg_CHD *100
pct_windU_diff70_89_03_22 <- avg_windU_diff70_89_03_22 * coefficients_CHD70_89_03_22["windU_diff70_89_03_22"] / avg_CHD *100
pct_windV_diff70_89_03_22 <- avg_windV_diff70_89_03_22 * coefficients_CHD70_89_03_22["windV_diff70_89_03_22"] / avg_CHD *100
pct_solar_diff70_89_03_22 <- avg_solar_diff70_89_03_22 * coefficients_CHD70_89_03_22["solar_diff70_89_03_22_scale"] / avg_CHD *100
pct_pressure_diff70_89_03_22 <- avg_pressure_diff70_89_03_22 * coefficients_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] / avg_CHD *100
pct_precip_diff70_89_03_22 <- avg_precip_diff70_89_03_22 * coefficients_CHD70_89_03_22["precip_diff70_89_03_22_scale"] / avg_CHD *100
pct_transpir_diff70_89_03_22 <- avg_transpir_diff70_89_03_22 * coefficients_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] / avg_CHD *100

## get the rate of change
avg_HI_C_Change70_89_03_22 <- mean(sensitivity70_89_03_22_merged$HI_C_Change70_89_03_22, na.rm = TRUE)
avg_windU_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windU_diff70_89_03_22, na.rm = TRUE)
avg_windV_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windV_diff70_89_03_22, na.rm = TRUE)
avg_solar_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale, na.rm = TRUE)
avg_pressure_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale, na.rm = TRUE)
avg_precip_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale, na.rm = TRUE)
avg_transpir_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale, na.rm = TRUE)

rate_HI_C_Change70_89_03_22 <- avg_HI_C_Change70_89_03_22 * coefficients_CHD70_89_03_22["HI_C_Change70_89_03_22"]
rate_windU_diff70_89_03_22 <- avg_windU_diff70_89_03_22 * coefficients_CHD70_89_03_22["windU_diff70_89_03_22"] 
rate_windV_diff70_89_03_22 <- avg_windV_diff70_89_03_22 * coefficients_CHD70_89_03_22["windV_diff70_89_03_22"] 
rate_solar_diff70_89_03_22 <- avg_solar_diff70_89_03_22 * coefficients_CHD70_89_03_22["solar_diff70_89_03_22_scale"]
rate_pressure_diff70_89_03_22 <- avg_pressure_diff70_89_03_22 * coefficients_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] 
rate_precip_diff70_89_03_22 <- avg_precip_diff70_89_03_22 * coefficients_CHD70_89_03_22["precip_diff70_89_03_22_scale"]
rate_transpir_diff70_89_03_22 <- avg_transpir_diff70_89_03_22 * coefficients_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

lower_CI_HI_C_Change70_89_03_22 <- rate_HI_C_Change70_89_03_22 - 1.96 * se_CHD70_89_03_22["HI_C_Change70_89_03_22"] 
lower_CI_windU_diff70_89_03_22 <- rate_windU_diff70_89_03_22 - 1.96 * se_CHD70_89_03_22["windU_diff70_89_03_22"] 
lower_CI_windV_diff70_89_03_22 <- rate_windV_diff70_89_03_22 - 1.96 * se_CHD70_89_03_22["windV_diff70_89_03_22"] 
lower_CI_solar_diff70_89_03_22 <- rate_solar_diff70_89_03_22  - 1.96 * se_CHD70_89_03_22["solar_diff70_89_03_22_scale"] 
lower_CI_pressure_diff70_89_03_22 <- rate_pressure_diff70_89_03_22  - 1.96 * se_CHD70_89_03_22["pressure_diff70_89_03_22_scale"]
lower_CI_precip_diff70_89_03_22 <- rate_precip_diff70_89_03_22  - 1.96 * se_CHD70_89_03_22["precip_diff70_89_03_22_scale"] 
lower_CI_transpir_diff70_89_03_22 <- rate_transpir_diff70_89_03_22  - 1.96 * se_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

upper_CI_HI_C_Change70_89_03_22 <- rate_HI_C_Change70_89_03_22 + 1.96 * se_CHD70_89_03_22["HI_C_Change70_89_03_22"] 
upper_CI_windU_diff70_89_03_22 <- rate_windU_diff70_89_03_22 + 1.96 * se_CHD70_89_03_22["windU_diff70_89_03_22"] 
upper_CI_windV_diff70_89_03_22 <- rate_windV_diff70_89_03_22  + 1.96 * se_CHD70_89_03_22["windV_diff70_89_03_22"] 
upper_CI_solar_diff70_89_03_22 <- rate_solar_diff70_89_03_22 + 1.96 * se_CHD70_89_03_22["solar_diff70_89_03_22_scale"] 
upper_CI_pressure_diff70_89_03_22 <- rate_pressure_diff70_89_03_22  + 1.96 * se_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] 
upper_CI_precip_diff70_89_03_22 <- rate_precip_diff70_89_03_22  + 1.96 * se_CHD70_89_03_22["precip_diff70_89_03_22_scale"] 
upper_CI_transpir_diff70_89_03_22 <- rate_transpir_diff70_89_03_22  + 1.96 * se_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

# adjust for population #
# turn subset into a data frame to add columns
new_data <- sensitivity70_89_03_22_merged

### calculate total exposure in each tract
new_data$exposure_hi <- new_data$HI_C_Change70_89_03_22 * new_data$Pop18Over
new_data$exposure_windu <- new_data$windU_diff70_89_03_22 * new_data$Pop18Over
new_data$exposure_windv <- new_data$windV_diff70_89_03_22 * new_data$Pop18Over
new_data$exposure_solar <- new_data$solar_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_press <- new_data$pressure_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_precip <- new_data$precip_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_transpir <- new_data$transpir_diff70_89_03_22_scale * new_data$Pop18Over

## calculation for national level exposure
av_natl_expos_hi <- sum(new_data$exposure_hi)/sum(new_data$Pop18Over)
av_natl_expos_windu <- sum(new_data$exposure_windu)/sum(new_data$Pop18Over)
av_natl_expos_windv <- sum(new_data$exposure_windv)/sum(new_data$Pop18Over)
av_natl_expos_solar <- sum(new_data$exposure_solar)/sum(new_data$Pop18Over)
av_natl_expos_press <- sum(new_data$exposure_press)/sum(new_data$Pop18Over)
av_natl_expos_precip <- sum(new_data$exposure_precip)/sum(new_data$Pop18Over)
av_natl_expos_transpir <- sum(new_data$exposure_transpir)/sum(new_data$Pop18Over)

## calculate population weighted effect size 
popweight_effectsize_hi <- av_natl_expos_hi * coefficients_CHD70_89_03_22["HI_C_Change70_89_03_22"]
popweight_effectsize_windu <- av_natl_expos_windu * coefficients_CHD70_89_03_22["windU_diff70_89_03_22"] 
popweight_effectsize_windv <- av_natl_expos_windv * coefficients_CHD70_89_03_22["windV_diff70_89_03_22"] 
popweight_effectsize_solar <- av_natl_expos_solar * coefficients_CHD70_89_03_22["solar_diff70_89_03_22_scale"] 
popweight_effectsize_press <- av_natl_expos_press * coefficients_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] 
popweight_effectsize_precip <- av_natl_expos_precip * coefficients_CHD70_89_03_22["precip_diff70_89_03_22_scale"] 
popweight_effectsize_transpir <- av_natl_expos_transpir * coefficients_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

# create confidence interval
lower_CI_natl_hi <- popweight_effectsize_hi - 1.96 * se_CHD70_89_03_22["HI_C_Change70_89_03_22"] 
lower_CI_natl_windu <- popweight_effectsize_windu - 1.96 * se_CHD70_89_03_22["windU_diff70_89_03_22"] 
lower_CI_natl_windv <- popweight_effectsize_windv - 1.96 * se_CHD70_89_03_22["windV_diff70_89_03_22"] 
lower_CI_natl_solar <- popweight_effectsize_solar  - 1.96 * se_CHD70_89_03_22["solar_diff70_89_03_22_scale"] 
lower_CI_natl_press <- popweight_effectsize_press  - 1.96 * se_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] 
lower_CI_natl_precip <- popweight_effectsize_precip  - 1.96 * se_CHD70_89_03_22["precip_diff70_89_03_22_scale"] 
lower_CI_natl_transpir <- popweight_effectsize_transpir  - 1.96 * se_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

upper_CI_natl_hi <- popweight_effectsize_hi + 1.96 * se_CHD70_89_03_22["HI_C_Change70_89_03_22"] 
upper_CI_natl_windu <- popweight_effectsize_windu + 1.96 * se_CHD70_89_03_22["windU_diff70_89_03_22"] 
upper_CI_natl_windv <- popweight_effectsize_windv  + 1.96 * se_CHD70_89_03_22["windV_diff70_89_03_22"]  
upper_CI_natl_solar <- popweight_effectsize_solar + 1.96 * se_CHD70_89_03_22["solar_diff70_89_03_22_scale"] 
upper_CI_natl_press <- popweight_effectsize_press  + 1.96 * se_CHD70_89_03_22["pressure_diff70_89_03_22_scale"] 
upper_CI_natl_precip <- popweight_effectsize_precip  + 1.96 * se_CHD70_89_03_22["precip_diff70_89_03_22_scale"]
upper_CI_natl_transpir <- popweight_effectsize_transpir  + 1.96 * se_CHD70_89_03_22["transpir_diff70_89_03_22_scale"] 

#calculate population weighted CHD for % change 
new_data$pop_chd <- new_data$CHD_CrudePrev * new_data$Pop18Over
av_natl_chd <- sum(new_data$pop_chd) / sum(new_data$Pop18Over)

# calculate % change
pct_popweight_effectsize_hi <- popweight_effectsize_hi/av_natl_chd *100
pct_popweight_effectsize_windu <- popweight_effectsize_windu/av_natl_chd *100
pct_popweight_effectsize_windv <- popweight_effectsize_windv/av_natl_chd *100
pct_popweight_effectsize_solar <- popweight_effectsize_solar/av_natl_chd *100
pct_popweight_effectsize_press <- popweight_effectsize_press/av_natl_chd *100
pct_popweight_effectsize_precip <- popweight_effectsize_precip/av_natl_chd *100
pct_popweight_effectsize_transpir <- popweight_effectsize_transpir/av_natl_chd *100


# Combine into a data frame with lower and upper bounds in one column
CHD70_89_results <- data.frame(
  Variable = c("Heat Index anomaly", 
               "Eastward Wind anomaly", "Northward Wind anomaly", "Absorbed Sunlight (*10^-6)", 
               "Surface Pressure anomaly (*10^-2)", "Precipitation anomaly (*10)",
               "Transpiration anomaly (*10^4)"),
  Effect_Size = coefficients_CHD70_89_03_22[c("HI_C_Change70_89_03_22", 
                                              "windU_diff70_89_03_22",
                                              "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                              "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                              "transpir_diff70_89_03_22_scale")],
  Confidence_Interval1 = paste("(", lower_bound_CHD70_89_03_22[c("HI_C_Change70_89_03_22", 
                                                                 "windU_diff70_89_03_22",
                                                                 "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                                                 "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                                                 "transpir_diff70_89_03_22_scale")],
                               ", ", upper_bound_CHD70_89_03_22[c("HI_C_Change70_89_03_22", 
                                                                  "windU_diff70_89_03_22",
                                                                  "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                                                  "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                                                  "transpir_diff70_89_03_22_scale")], ")", sep = ""),
  T_Value = tvalue_CHD70_89_03_22[c("HI_C_Change70_89_03_22", 
                                    "windU_diff70_89_03_22",
                                    "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                    "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                    "transpir_diff70_89_03_22_scale")],
  Anomaly_Size = round(c(avg_HI_C_Change70_89_03_22, 
                         avg_windU_diff70_89_03_22, avg_windV_diff70_89_03_22, 
                         avg_solar_diff70_89_03_22, 
                         avg_pressure_diff70_89_03_22,
                         avg_precip_diff70_89_03_22,  avg_transpir_diff70_89_03_22), 2),
  Effect_Size_Anomaly = round(c(rate_HI_C_Change70_89_03_22, rate_windU_diff70_89_03_22,
                                rate_windV_diff70_89_03_22, rate_solar_diff70_89_03_22, 
                                rate_pressure_diff70_89_03_22, rate_precip_diff70_89_03_22, 
                                rate_transpir_diff70_89_03_22), 2), 
  Confidence_Interval2 = paste("(", c(lower_CI_HI_C_Change70_89_03_22, lower_CI_windU_diff70_89_03_22,
                                      lower_CI_windV_diff70_89_03_22, lower_CI_solar_diff70_89_03_22, 
                                      lower_CI_pressure_diff70_89_03_22, lower_CI_precip_diff70_89_03_22,
                                      lower_CI_transpir_diff70_89_03_22), 
                               ", ", c(upper_CI_HI_C_Change70_89_03_22, upper_CI_windU_diff70_89_03_22,
                                       upper_CI_windV_diff70_89_03_22, upper_CI_solar_diff70_89_03_22, 
                                       upper_CI_pressure_diff70_89_03_22, upper_CI_precip_diff70_89_03_22,
                                       upper_CI_transpir_diff70_89_03_22), ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_89_03_22, 
                           pct_windU_diff70_89_03_22, pct_windV_diff70_89_03_22, 
                           pct_solar_diff70_89_03_22, 
                           pct_pressure_diff70_89_03_22,
                           pct_precip_diff70_89_03_22,  pct_transpir_diff70_89_03_22),
  Average_National_Exposure = round(c(av_natl_expos_hi, av_natl_expos_windu, av_natl_expos_windv,
                                      av_natl_expos_solar, av_natl_expos_press, av_natl_expos_precip,
                                      av_natl_expos_transpir),2),
  Pop_Weighted_Effect_Size = round(c(popweight_effectsize_hi, popweight_effectsize_windu, popweight_effectsize_windv,
                                     popweight_effectsize_solar, popweight_effectsize_press, popweight_effectsize_precip,
                                     popweight_effectsize_transpir),2),
  Confidence_Interval3 = paste("(", c(lower_CI_natl_hi, lower_CI_natl_windu, lower_CI_natl_windv, 
                                      lower_CI_natl_solar, lower_CI_natl_press, 
                                      lower_CI_natl_precip, lower_CI_natl_transpir),
                               ", ", c(upper_CI_natl_hi, upper_CI_natl_windu, upper_CI_natl_windv, 
                                       upper_CI_natl_solar, upper_CI_natl_press,
                                       upper_CI_natl_precip, upper_CI_natl_transpir), ")", sep = ""),
  Pop_Weight_Percent_Contribution = round(c(pct_popweight_effectsize_hi, pct_popweight_effectsize_windu, 
                                            pct_popweight_effectsize_windv, pct_popweight_effectsize_solar, 
                                            pct_popweight_effectsize_press, pct_popweight_effectsize_precip,
                                            pct_popweight_effectsize_transpir),2)
)


# Print or further process the results
print(CHD70_89_results)

# Round all values in CHD70_89_results to 2 decimal places
CHD70_89_results$Effect_Size <- round(CHD70_89_results$Effect_Size, 2)
CHD70_89_results$T_Value <- round(CHD70_89_results$T_Value, 2)
CHD70_89_results$Percent_Contribution <- round(CHD70_89_results$Percent_Contribution, 2)

# Convert CI column to character to avoid rounding issues
CHD70_89_results$Confidence_Interval1 <- as.character(CHD70_89_results$Confidence_Interval1)

CHD70_89_results$Confidence_Interval2 <- as.character(CHD70_89_results$Confidence_Interval2)

CHD70_89_results$Confidence_Interval3 <- as.character(CHD70_89_results$Confidence_Interval3)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds1 <- sub("[(](.*),.*", "\\1", CHD70_89_results$Confidence_Interval1)
upper_bounds1 <- sub(".*, (.*)[)]", "\\1", CHD70_89_results$Confidence_Interval1)

lower_boundS7 <- sub("[(](.*),.*", "\\1", CHD70_89_results$Confidence_Interval2)
upper_boundS7 <- sub(".*, (.*)[)]", "\\1", CHD70_89_results$Confidence_Interval2)

lower_bounds3 <- sub("[(](.*),.*", "\\1", CHD70_89_results$Confidence_Interval3)
upper_bounds3 <- sub(".*, (.*)[)]", "\\1", CHD70_89_results$Confidence_Interval3)

# Handle NAs by replacing them with 0
lower_bounds1[is.na(lower_bounds1)] <- 0
upper_bounds1[is.na(upper_bounds1)] <- 0

lower_boundS7[is.na(lower_boundS7)] <- 0
upper_boundS7[is.na(upper_boundS7)] <- 0

lower_bounds3[is.na(lower_bounds3)] <- 0
upper_bounds3[is.na(upper_bounds3)] <- 0

# Round the bounds to 2 decimal places
lower_bounds1 <- round(as.numeric(lower_bounds1), 2)
upper_bounds1 <- round(as.numeric(upper_bounds1), 2)

lower_boundS7 <- round(as.numeric(lower_boundS7), 2)
upper_boundS7 <- round(as.numeric(upper_boundS7), 2)

lower_bounds3 <- round(as.numeric(lower_bounds3), 2)
upper_bounds3 <- round(as.numeric(upper_bounds3), 2)
# Combine the rounded bounds into the CI column
CHD70_89_results$CI_1 <- paste0("(", lower_bounds1, ", ", upper_bounds1, ")")
CHD70_89_results$CI_2 <- paste0("(", lower_boundS7, ", ", upper_boundS7, ")")
CHD70_89_results$CI_3 <- paste0("(", lower_boundS7, ", ", upper_bounds3, ")")

# remove numeric CI
CHD70_89_results <- CHD70_89_results[, -which(names(CHD70_89_results) == "Confidence_Interval1")]
CHD70_89_results <- CHD70_89_results[, -which(names(CHD70_89_results) == "Confidence_Interval2")]
CHD70_89_results <- CHD70_89_results[, -which(names(CHD70_89_results) == "Confidence_Interval3")]

print(CHD70_89_results)
# reorder columns
CHD70_89_results <- CHD70_89_results %>%
  mutate(Effect_Size_Per_Unit_CI = paste(Effect_Size, CI_1, sep = " ")) %>%
  mutate(Effect_Size_Anomaly_CI = paste(Effect_Size_Anomaly, CI_2, sep = " ")) %>%
  mutate(Pop_Weight_Effect_Size_Anomaly_CI = paste(Pop_Weighted_Effect_Size, CI_3, sep = " ")) %>%
  select(Variable, Effect_Size_Per_Unit_CI, T_Value, Anomaly_Size, Effect_Size_Anomaly_CI, Percent_Contribution,
         Average_National_Exposure, Pop_Weight_Effect_Size_Anomaly_CI, Pop_Weight_Percent_Contribution)


print(CHD70_89_results)
write.csv(CHD70_89_results, "CHDoptimized70_89_03_22_TableS5_08_28_2024.csv", row.names = FALSE)
################################################ Stroke Table S4 #########################################################
###### Run optimized model ###############################################################################################
# avg stroke
avg_STROKE <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

# 70-80
coefficients_stroke70_89_03_22 <- list()
se_stroke70_89_03_22 <- list()
tvalue_stroke70_89_03_22 <- list()
lower_bound_stroke70_89_03_22 <- list()
upper_bound_stroke70_89_03_22 <- list()


stroke70_89_03_22 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_89_03_22 + 
                            Temp_C_2013_2022.x + PCT_ImperviousSurfaces +
                            SPL_THEME1 + EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                            OBESITY_CrudePrev + windU_diff70_89_03_22 + windV_diff70_89_03_22 + solar_diff70_89_03_22_scale
                          + pressure_diff70_89_03_22_scale 
                          + precip_diff70_89_03_22_scale + transpir_diff70_89_03_22_scale + LCchangeMEAN
                          + (1 | CountyFIPS), data = sensitivity70_89_03_22_merged)
summary(stroke70_89_03_22)


# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke70_89_03_22 <- summary(stroke70_89_03_22)$coefficients[, "Estimate"]
se_stroke70_89_03_22 <- summary(stroke70_89_03_22)$coefficients[, "Std. Error"]
tvalue_stroke70_89_03_22 <- summary(stroke70_89_03_22)$coefficients[, "t value"]
lower_bound_stroke70_89_03_22  <-  coefficients_stroke70_89_03_22 - 1.96 * se_stroke70_89_03_22 
upper_bound_stroke70_89_03_22  <-  coefficients_stroke70_89_03_22 + 1.96 * se_stroke70_89_03_22


# average anomaly size
# 1970-1980
avg_HI_C_Change70_89_03_22 <- mean(sensitivity70_89_03_22_merged$HI_C_Change70_89_03_22, na.rm = TRUE)
avg_windU_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windU_diff70_89_03_22, na.rm = TRUE)
avg_windV_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windV_diff70_89_03_22, na.rm = TRUE)
avg_solar_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale, na.rm = TRUE)
avg_pressure_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale, na.rm = TRUE)
avg_precip_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale, na.rm = TRUE)
avg_transpir_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale, na.rm = TRUE)


## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_89_03_22 <- avg_HI_C_Change70_89_03_22 * coefficients_stroke70_89_03_22["HI_C_Change70_89_03_22"] / avg_STROKE *100
pct_windU_diff70_89_03_22 <- avg_windU_diff70_89_03_22 * coefficients_stroke70_89_03_22["windU_diff70_89_03_22"] / avg_STROKE *100
pct_windV_diff70_89_03_22 <- avg_windV_diff70_89_03_22 * coefficients_stroke70_89_03_22["windV_diff70_89_03_22"] / avg_STROKE *100
pct_solar_diff70_89_03_22 <- avg_solar_diff70_89_03_22 * coefficients_stroke70_89_03_22["solar_diff70_89_03_22_scale"] / avg_STROKE *100
pct_pressure_diff70_89_03_22 <- avg_pressure_diff70_89_03_22 * coefficients_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] / avg_STROKE *100
pct_precip_diff70_89_03_22 <- avg_precip_diff70_89_03_22 * coefficients_stroke70_89_03_22["precip_diff70_89_03_22_scale"] / avg_STROKE *100
pct_transpir_diff70_89_03_22 <- avg_transpir_diff70_89_03_22 * coefficients_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] / avg_STROKE *100

## get the rate of change
avg_HI_C_Change70_89_03_22 <- mean(sensitivity70_89_03_22_merged$HI_C_Change70_89_03_22, na.rm = TRUE)
avg_windU_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windU_diff70_89_03_22, na.rm = TRUE)
avg_windV_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$windV_diff70_89_03_22, na.rm = TRUE)
avg_solar_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$solar_diff70_89_03_22_scale, na.rm = TRUE)
avg_pressure_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$pressure_diff70_89_03_22_scale, na.rm = TRUE)
avg_precip_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$precip_diff70_89_03_22_scale, na.rm = TRUE)
avg_transpir_diff70_89_03_22 <- mean(sensitivity70_89_03_22_merged$transpir_diff70_89_03_22_scale, na.rm = TRUE)

rate_HI_C_Change70_89_03_22 <- avg_HI_C_Change70_89_03_22 * coefficients_stroke70_89_03_22["HI_C_Change70_89_03_22"]
rate_windU_diff70_89_03_22 <- avg_windU_diff70_89_03_22 * coefficients_stroke70_89_03_22["windU_diff70_89_03_22"] 
rate_windV_diff70_89_03_22 <- avg_windV_diff70_89_03_22 * coefficients_stroke70_89_03_22["windV_diff70_89_03_22"] 
rate_solar_diff70_89_03_22 <- avg_solar_diff70_89_03_22 * coefficients_stroke70_89_03_22["solar_diff70_89_03_22_scale"]
rate_pressure_diff70_89_03_22 <- avg_pressure_diff70_89_03_22 * coefficients_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
rate_precip_diff70_89_03_22 <- avg_precip_diff70_89_03_22 * coefficients_stroke70_89_03_22["precip_diff70_89_03_22_scale"]
rate_transpir_diff70_89_03_22 <- avg_transpir_diff70_89_03_22 * coefficients_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

lower_CI_HI_C_Change70_89_03_22 <- rate_HI_C_Change70_89_03_22 - 1.96 * se_stroke70_89_03_22["HI_C_Change70_89_03_22"] 
lower_CI_windU_diff70_89_03_22 <- rate_windU_diff70_89_03_22 - 1.96 * se_stroke70_89_03_22["windU_diff70_89_03_22"] 
lower_CI_windV_diff70_89_03_22 <- rate_windV_diff70_89_03_22 - 1.96 * se_stroke70_89_03_22["windV_diff70_89_03_22"] 
lower_CI_solar_diff70_89_03_22 <- rate_solar_diff70_89_03_22  - 1.96 * se_stroke70_89_03_22["solar_diff70_89_03_22_scale"]  
lower_CI_pressure_diff70_89_03_22 <- rate_pressure_diff70_89_03_22  - 1.96 * se_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
lower_CI_precip_diff70_89_03_22 <- rate_precip_diff70_89_03_22  - 1.96 * se_stroke70_89_03_22["precip_diff70_89_03_22_scale"] 
lower_CI_transpir_diff70_89_03_22 <- rate_transpir_diff70_89_03_22  - 1.96 * se_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

upper_CI_HI_C_Change70_89_03_22 <- rate_HI_C_Change70_89_03_22 + 1.96 * se_stroke70_89_03_22["HI_C_Change70_89_03_22"] 
upper_CI_windU_diff70_89_03_22 <- rate_windU_diff70_89_03_22 + 1.96 * se_stroke70_89_03_22["windU_diff70_89_03_22"] 
upper_CI_windV_diff70_89_03_22 <- rate_windV_diff70_89_03_22  + 1.96 * se_stroke70_89_03_22["windV_diff70_89_03_22"] 
upper_CI_solar_diff70_89_03_22 <- rate_solar_diff70_89_03_22 + 1.96 * se_stroke70_89_03_22["solar_diff70_89_03_22_scale"] 
upper_CI_pressure_diff70_89_03_22 <- rate_pressure_diff70_89_03_22  + 1.96 * se_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
upper_CI_precip_diff70_89_03_22 <- rate_precip_diff70_89_03_22  + 1.96 * se_stroke70_89_03_22["precip_diff70_89_03_22_scale"] 
upper_CI_transpir_diff70_89_03_22 <- rate_transpir_diff70_89_03_22  + 1.96 * se_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

# adjust for population #
# turn subset into a data frame to add columns
new_data <- sensitivity70_89_03_22_merged

### calculate total exposure in each tract
new_data$exposure_hi <- new_data$HI_C_Change70_89_03_22 * new_data$Pop18Over
new_data$exposure_windu <- new_data$windU_diff70_89_03_22 * new_data$Pop18Over
new_data$exposure_windv <- new_data$windV_diff70_89_03_22 * new_data$Pop18Over
new_data$exposure_solar <- new_data$solar_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_press <- new_data$pressure_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_precip <- new_data$precip_diff70_89_03_22_scale * new_data$Pop18Over
new_data$exposure_transpir <- new_data$transpir_diff70_89_03_22_scale * new_data$Pop18Over

## calculation for national level exposure
av_natl_expos_hi <- sum(new_data$exposure_hi)/sum(new_data$Pop18Over)
av_natl_expos_windu <- sum(new_data$exposure_windu)/sum(new_data$Pop18Over)
av_natl_expos_windv <- sum(new_data$exposure_windv)/sum(new_data$Pop18Over)
av_natl_expos_solar <- sum(new_data$exposure_solar)/sum(new_data$Pop18Over)
av_natl_expos_press <- sum(new_data$exposure_press)/sum(new_data$Pop18Over)
av_natl_expos_precip <- sum(new_data$exposure_precip)/sum(new_data$Pop18Over)
av_natl_expos_transpir <- sum(new_data$exposure_transpir)/sum(new_data$Pop18Over)

## calculate population weighted effect size 
popweight_effectsize_hi <- av_natl_expos_hi * coefficients_stroke70_89_03_22["HI_C_Change70_89_03_22"]
popweight_effectsize_windu <- av_natl_expos_windu * coefficients_stroke70_89_03_22["windU_diff70_89_03_22"] 
popweight_effectsize_windv <- av_natl_expos_windv * coefficients_stroke70_89_03_22["windV_diff70_89_03_22"] 
popweight_effectsize_solar <- av_natl_expos_solar * coefficients_stroke70_89_03_22["solar_diff70_89_03_22_scale"] 
popweight_effectsize_press <- av_natl_expos_press * coefficients_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
popweight_effectsize_precip <- av_natl_expos_precip * coefficients_stroke70_89_03_22["precip_diff70_89_03_22_scale"] 
popweight_effectsize_transpir <- av_natl_expos_transpir * coefficients_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

# create confidence interval
lower_CI_natl_hi <- popweight_effectsize_hi - 1.96 * se_stroke70_89_03_22["HI_C_Change70_89_03_22"] 
lower_CI_natl_windu <- popweight_effectsize_windu - 1.96 * se_stroke70_89_03_22["windU_diff70_89_03_22"] 
lower_CI_natl_windv <- popweight_effectsize_windv - 1.96 * se_stroke70_89_03_22["windV_diff70_89_03_22"] 
lower_CI_natl_solar <- popweight_effectsize_solar  - 1.96 * se_stroke70_89_03_22["solar_diff70_89_03_22_scale"] 
lower_CI_natl_press <- popweight_effectsize_press  - 1.96 * se_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
lower_CI_natl_precip <- popweight_effectsize_precip  - 1.96 * se_stroke70_89_03_22["precip_diff70_89_03_22_scale"] 
lower_CI_natl_transpir <- popweight_effectsize_transpir  - 1.96 * se_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

upper_CI_natl_hi <- popweight_effectsize_hi + 1.96 * se_stroke70_89_03_22["HI_C_Change70_89_03_22"] 
upper_CI_natl_windu <- popweight_effectsize_windu + 1.96 * se_stroke70_89_03_22["windU_diff70_89_03_22"] 
upper_CI_natl_windv <- popweight_effectsize_windv  + 1.96 * se_stroke70_89_03_22["windV_diff70_89_03_22"]  
upper_CI_natl_solar <- popweight_effectsize_solar + 1.96 * se_stroke70_89_03_22["solar_diff70_89_03_22_scale"] 
upper_CI_natl_press <- popweight_effectsize_press  + 1.96 * se_stroke70_89_03_22["pressure_diff70_89_03_22_scale"] 
upper_CI_natl_precip <- popweight_effectsize_precip  + 1.96 * se_stroke70_89_03_22["precip_diff70_89_03_22_scale"]
upper_CI_natl_transpir <- popweight_effectsize_transpir  + 1.96 * se_stroke70_89_03_22["transpir_diff70_89_03_22_scale"] 

#calculate population weighted stroke for % change 
new_data$pop_stroke <- new_data$STROKE_CrudePrev * new_data$Pop18Over
sum(new_data$pop_stroke)
sum(new_data$Pop18Over)
av_natl_stroke <- sum(new_data$pop_stroke) / sum(new_data$Pop18Over)

# calculate % change
pct_popweight_effectsize_hi <- popweight_effectsize_hi/av_natl_stroke *100
pct_popweight_effectsize_windu <- popweight_effectsize_windu/av_natl_stroke *100
pct_popweight_effectsize_windv <- popweight_effectsize_windv/av_natl_stroke *100
pct_popweight_effectsize_solar <- popweight_effectsize_solar/av_natl_stroke *100
pct_popweight_effectsize_press <- popweight_effectsize_press/av_natl_stroke *100
pct_popweight_effectsize_precip <- popweight_effectsize_precip/av_natl_stroke *100
pct_popweight_effectsize_transpir <- popweight_effectsize_transpir/av_natl_stroke *100

# Combine into a data frame with lower and upper bounds in one column
stroke70_89_results <- data.frame(
  Variable = c("Heat Index anomaly", 
               "Eastward Wind anomaly", "Northward Wind anomaly", "Absorbed Sunlight (*10^-6)", 
               "Surface Pressure anomaly (*10^-2)", "Precipitation anomaly (*10)",
               "Transpiration anomaly (*10^4)"),
  Effect_Size = coefficients_stroke70_89_03_22[c("HI_C_Change70_89_03_22", 
                                                 "windU_diff70_89_03_22",
                                                 "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                                 "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                                 "transpir_diff70_89_03_22_scale")],
  Confidence_Interval1 = paste("(", lower_bound_stroke70_89_03_22[c("HI_C_Change70_89_03_22", 
                                                                    "windU_diff70_89_03_22",
                                                                    "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                                                    "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                                                    "transpir_diff70_89_03_22_scale")],
                               ", ", upper_bound_stroke70_89_03_22[c("HI_C_Change70_89_03_22", 
                                                                     "windU_diff70_89_03_22",
                                                                     "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                                                     "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                                                     "transpir_diff70_89_03_22_scale")], ")", sep = ""),
  T_Value = tvalue_stroke70_89_03_22[c("HI_C_Change70_89_03_22", 
                                       "windU_diff70_89_03_22",
                                       "windV_diff70_89_03_22", "solar_diff70_89_03_22_scale", 
                                       "pressure_diff70_89_03_22_scale", "precip_diff70_89_03_22_scale",
                                       "transpir_diff70_89_03_22_scale")],
  Anomaly_Size = round(c(avg_HI_C_Change70_89_03_22, 
                         avg_windU_diff70_89_03_22, avg_windV_diff70_89_03_22, 
                         avg_solar_diff70_89_03_22, 
                         avg_pressure_diff70_89_03_22,
                         avg_precip_diff70_89_03_22,  avg_transpir_diff70_89_03_22), 2),
  Effect_Size_Anomaly = round(c(rate_HI_C_Change70_89_03_22, rate_windU_diff70_89_03_22,
                                rate_windV_diff70_89_03_22, rate_solar_diff70_89_03_22, 
                                rate_pressure_diff70_89_03_22, rate_precip_diff70_89_03_22, 
                                rate_transpir_diff70_89_03_22), 2), 
  Confidence_Interval2 = paste("(", c(lower_CI_HI_C_Change70_89_03_22, lower_CI_windU_diff70_89_03_22,
                                      lower_CI_windV_diff70_89_03_22, lower_CI_solar_diff70_89_03_22, 
                                      lower_CI_pressure_diff70_89_03_22, lower_CI_precip_diff70_89_03_22,
                                      lower_CI_transpir_diff70_89_03_22), 
                               ", ", c(upper_CI_HI_C_Change70_89_03_22, upper_CI_windU_diff70_89_03_22,
                                       upper_CI_windV_diff70_89_03_22, upper_CI_solar_diff70_89_03_22, 
                                       upper_CI_pressure_diff70_89_03_22, upper_CI_precip_diff70_89_03_22,
                                       upper_CI_transpir_diff70_89_03_22), ")", sep = ""),
  Percent_Contribution = c(pct_HI_C_Change70_89_03_22, 
                           pct_windU_diff70_89_03_22, pct_windV_diff70_89_03_22, 
                           pct_solar_diff70_89_03_22, 
                           pct_pressure_diff70_89_03_22,
                           pct_precip_diff70_89_03_22,  pct_transpir_diff70_89_03_22),
  Average_National_Exposure = round(c(av_natl_expos_hi, av_natl_expos_windu, av_natl_expos_windv,
                                      av_natl_expos_solar, av_natl_expos_press, av_natl_expos_precip,
                                      av_natl_expos_transpir),2),
  Pop_Weighted_Effect_Size = round(c(popweight_effectsize_hi, popweight_effectsize_windu, popweight_effectsize_windv,
                                     popweight_effectsize_solar, popweight_effectsize_press, popweight_effectsize_precip,
                                     popweight_effectsize_transpir),2),
  Confidence_Interval3 = paste("(", c(lower_CI_natl_hi, lower_CI_natl_windu, lower_CI_natl_windv, 
                                      lower_CI_natl_solar, lower_CI_natl_press, 
                                      lower_CI_natl_precip, lower_CI_natl_transpir),
                               ", ", c(upper_CI_natl_hi, upper_CI_natl_windu, upper_CI_natl_windv, 
                                       upper_CI_natl_solar, upper_CI_natl_press,
                                       upper_CI_natl_precip, upper_CI_natl_transpir), ")", sep = ""),
  Pop_Weight_Percent_Contribution = round(c(pct_popweight_effectsize_hi, pct_popweight_effectsize_windu, 
                                            pct_popweight_effectsize_windv, pct_popweight_effectsize_solar, 
                                            pct_popweight_effectsize_press, pct_popweight_effectsize_precip,
                                            pct_popweight_effectsize_transpir),2)
)


# Print or further process the results
print(stroke70_89_results)

# Round all values in stroke70_89_results to 3 decimal places
stroke70_89_results$Effect_Size <- round(stroke70_89_results$Effect_Size, 2)
stroke70_89_results$T_Value <- round(stroke70_89_results$T_Value, 2)
stroke70_89_results$Percent_Contribution <- round(stroke70_89_results$Percent_Contribution, 2)

# Convert CI column to character to avoid rounding issues
stroke70_89_results$Confidence_Interval1 <- as.character(stroke70_89_results$Confidence_Interval1)

stroke70_89_results$Confidence_Interval2 <- as.character(stroke70_89_results$Confidence_Interval2)

stroke70_89_results$Confidence_Interval3 <- as.character(stroke70_89_results$Confidence_Interval3)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds1 <- sub("[(](.*),.*", "\\1", stroke70_89_results$Confidence_Interval1)
upper_bounds1 <- sub(".*, (.*)[)]", "\\1", stroke70_89_results$Confidence_Interval1)

lower_boundS7 <- sub("[(](.*),.*", "\\1", stroke70_89_results$Confidence_Interval2)
upper_boundS7 <- sub(".*, (.*)[)]", "\\1", stroke70_89_results$Confidence_Interval2)

lower_bounds3 <- sub("[(](.*),.*", "\\1", stroke70_89_results$Confidence_Interval3)
upper_bounds3 <- sub(".*, (.*)[)]", "\\1", stroke70_89_results$Confidence_Interval3)

# Handle NAs by replacing them with 0
lower_bounds1[is.na(lower_bounds1)] <- 0
upper_bounds1[is.na(upper_bounds1)] <- 0

lower_boundS7[is.na(lower_boundS7)] <- 0
upper_boundS7[is.na(upper_boundS7)] <- 0

lower_bounds3[is.na(lower_bounds3)] <- 0
upper_bounds3[is.na(upper_bounds3)] <- 0

# Round the bounds to 2 decimal places
lower_bounds1 <- round(as.numeric(lower_bounds1), 2)
upper_bounds1 <- round(as.numeric(upper_bounds1), 2)

lower_boundS7 <- round(as.numeric(lower_boundS7), 2)
upper_boundS7 <- round(as.numeric(upper_boundS7), 2)

lower_bounds3 <- round(as.numeric(lower_bounds3), 2)
upper_bounds3 <- round(as.numeric(upper_bounds3), 2)

# Combine the rounded bounds into the CI column
stroke70_89_results$CI_1 <- paste0("(", lower_bounds1, ", ", upper_bounds1, ")")
stroke70_89_results$CI_2 <- paste0("(", lower_boundS7, ", ", upper_boundS7, ")")
stroke70_89_results$CI_3 <- paste0("(", lower_bounds3, ", ", upper_bounds3, ")")

# remove numeric CI
stroke70_89_results <- stroke70_89_results[, -which(names(stroke70_89_results) == "Confidence_Interval1")]
stroke70_89_results <- stroke70_89_results[, -which(names(stroke70_89_results) == "Confidence_Interval2")]
stroke70_89_results <- stroke70_89_results[, -which(names(stroke70_89_results) == "Confidence_Interval3")]

print(stroke70_89_results)
# reorder columns
stroke70_89_results <- stroke70_89_results %>%
  mutate(Effect_Size_Per_Unit_CI = paste(Effect_Size, CI_1, sep = " ")) %>%
  mutate(Effect_Size_Anomaly_CI = paste(Effect_Size_Anomaly, CI_2, sep = " ")) %>%
  mutate(Pop_Weight_Effect_Size_Anomaly_CI = paste(Pop_Weighted_Effect_Size, CI_3, sep = " ")) %>%
  select(Variable, Effect_Size_Per_Unit_CI, T_Value, Anomaly_Size, Effect_Size_Anomaly_CI, Percent_Contribution,
         Average_National_Exposure, Pop_Weight_Effect_Size_Anomaly_CI, Pop_Weight_Percent_Contribution)


print(stroke70_89_results)

####### Save data to Rdata
setwd(working_directory_data)
save(subset_data, inflow_county_US, inflow_county_US_foreign, 
     outflow_county_US, outflow_county_US_foreign, county_noflow, sensitivity70_89_03_22_merged, merged_data, 
     file = "commondata.Rdata")
setwd(working_directory_output)

#write results to csv
write.csv(stroke70_89_results, "STROKEoptimized_70_89_03_22_TableS5_08_28_2024 .csv", row.names = FALSE)

################################################# Table S1 and S2 ############################################################
#######################################################################################################################
########### create a new subset of data with variables of interest, read and merge csvs #####################
# combine all tracts and climate division data 
setwd(working_directory_data)
unjoined_tracts <- read.csv("Unjoined_Tracts2010_Edited.csv", header = TRUE, na.strings = "NA")
tracts <- read.csv("Tracts2010_CLIMDIV.csv", header = TRUE, na.strings = "NA")
climdiv <- rbind(tracts, unjoined_tracts)

# read csvs
hvi <- read.csv("Climate_Resilience.csv", header = TRUE, na.strings = "NA")
svi <- read.csv("SVI_2018.csv", header = TRUE,  na.strings = "NA")
places <- read.csv("final_result_11_13_2023.csv", header = TRUE,  na.strings = "NA") # averaged places data
sex_employ <- read.csv("Sex_Employment_JoinedtoTracts.csv", header = TRUE, na.strings = "NA")
race <- read.csv("Race_educationalattainment_Percents_TotalNumber.csv", header = TRUE, na.strings = "NA")
hi <- read.csv("decade_HI_C_06_28_2024.csv", header = TRUE, na.strings = "NA")
era5_vars <- read.csv("DecadeMeans_ERA5variables_07_26_2024.csv", header = TRUE, na.strings = "NA")
lc <- read.csv("LandCover_Change_04_15_2024.csv", header = TRUE, na.strings = "NA")
setwd(working_directory_output)
# sort climdiv data
climdiv_sorted <- climdiv[order(climdiv$GEOID10), ]

# change column name to enable merge #
colnames(hvi)[colnames(hvi) == "GEOID"] <-"GEOID10"

# sort HVI data by GEOID10
hvi_sorted <- hvi[order(hvi$GEOID10), ]

# merge the climdiv and hvi data
climdiv_hvi <- merge(climdiv_sorted, hvi_sorted, by = "GEOID10")

# sort svi data
# change column name to enable merge #
colnames(svi)[colnames(svi) == "FIPS"] <-"GEOID10"
svi_sorted <- svi[order(svi$GEOID10), ]

# merge the climdiv and svi data
climdiv_svi <- merge(climdiv_hvi, svi_sorted, by = "GEOID10")

# change column name in places and sort #
colnames(places)[colnames(places) == "TractFIPS"] <- "GEOID10"
places_sorted <- places[order(places$GEOID10), ]

# merge the climdiv and places data
climdiv_places <- merge(climdiv_svi, places_sorted, by = "GEOID10")

# sort sex_employ data by GEOID10
sex_sorted <- sex_employ[order(sex_employ$GEOID10), ]

# merge the climdiv and sex data
climdiv_sex <- merge(climdiv_places, sex_sorted, by = "GEOID10")

# sort race data by GEOID10
race_sorted <- race[order(race$GEOID10), ]

# merge the climdiv and sex data
climdiv_race <- merge(climdiv_sex, race_sorted, by = "GEOID10")

# sort the merged data by CLIMDIV
climdiv_race <- climdiv_race[order(climdiv_race$CLIMDIV), ]

# Sort the hi data frame by CLIMDIV
hi_sorted <- hi[order(hi$CLIMDIV), ]
hi_sorted <- hi_sorted %>%
  mutate(CLIMDIV = as.numeric(CLIMDIV))

# Merge hi_sorted with climdiv_places by CLIMDIV
climdiv_hi <- merge(climdiv_race, hi_sorted, by = "CLIMDIV")

# Sort era5 data frame by CLIMDIV
era5_vars_sorted <- era5_vars[order(era5_vars$CLIMDIV), ]
era5_vars_sorted <-era5_vars_sorted %>%
  mutate(CLIMDIV = as.numeric(CLIMDIV))

# Merge era5_vars_sorted with climdiv by CLIMDIV
climdiv_era5 <- merge(climdiv_hi, era5_vars_sorted, by = "CLIMDIV")

# Sort era5 data frame by CLIMDIV
lc_sorted <- lc[order(lc$CLIMDIV), ]
lc_sorted <- lc_sorted %>%
  mutate(CLIMDIV = as.numeric(CLIMDIV))

# Merge the result with lc_sorted by CLIMDIV
merged_data <- merge(climdiv_era5, lc_sorted, by = "CLIMDIV")

# Convert to NAs and keep as data frame
merged_data <- lapply(merged_data, function(x) ifelse(x == "<Null>", NA, x))
merged_data <- lapply(merged_data, function(x) ifelse(x == -999, NA, x))
merged_data <- data.frame(merged_data)
head(merged_data)

# Add a new column 'All_OtherRaces' with the sum of various race columns
merged_data <- merged_data %>%
  mutate(All_OtherRaces = AmericanIndian_AlaskaNative + Asian + NativeHawaiian + OtherRace)


# subset data for variables of interest
subset_data_s1 <- merged_data[, c("HI_C_Change70_79_13_22","ACCESS2_CrudePrev","ARTHRITIS_CrudePrev", "BINGE_CrudePrev",
                                  "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev", "CASTHMA_CrudePrev_CDC", "CERVICAL_CrudePrev",
                                  "CHD_CrudePrev", "CHECKUP_CrudePrev", "CHOLSCREEN_CrudePrev", "COLON_SCREEN_CrudePrev", "COPD_CrudePrev",
                                  "COREM_CrudePrev", "COREW_CrudePrev", "CSMOKING_CrudePrev", "DIABETES_CrudePrev", "DENTAL_CrudePrev",
                                  "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev", "LPA_CrudePrev", "MAMMOUSE_CrudePrev",
                                  "OBESITY_CrudePrev", "PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev",
                                  "DEPRESSION_CrudePrev", "GHLTH_CrudePrev", "Male_2015_2019", "Female_2015_2019", "Total.population", "White", "Black",
                                  "All_OtherRaces", "TwoOrMoreRaces", "Hispanic", "NotHispanic","Median_Income_2015_2019",  "SPL_THEME1", "EP_AGE65", "LCchangeMEAN",
                                  "PCT_ImperviousSurfaces", "Temp_C_2013_2022"
)]
# divide variables by 10 to scale 
subset_data_s1$OBESITY_CrudePrev_P_div10 <- subset_data_s1$OBESITY_CrudePrev / 10

## Exclude blank values and change to NA
subset_data_s1[subset_data_s1 == "-"] <- NA
subset_data_s1[subset_data_s1 == ""] <- NA

# convert from character to numeric
subset_data_s1$HI_C_Change70_79_13_22 <- as.numeric(as.character(subset_data_s1$HI_C_Change70_79_13_22, na.rm = TRUE))
subset_data_s1$ACCESS7_CrudePrev <- as.numeric(as.character(subset_data_s1$ACCESS2_CrudePrev, na.rm = TRUE))
subset_data_s1$ARTHRITIS_CrudePrev <- as.numeric(as.character(subset_data_s1$ARTHRITIS_CrudePrev, na.rm = TRUE))
subset_data_s1$BINGE_CrudePrev <- as.numeric(as.character(subset_data_s1$BINGE_CrudePrev, na.rm = TRUE))
subset_data_s1$BPHIGH_CrudePrev <- as.numeric(as.character(subset_data_s1$BPHIGH_CrudePrev, na.rm = TRUE))
subset_data_s1$BPMED_CrudePrev <- as.numeric(as.character(subset_data_s1$BPMED_CrudePrev, na.rm = TRUE))
subset_data_s1$CANCER_CrudePrev <- as.numeric(as.character(subset_data_s1$CANCER_CrudePrev, na.rm = TRUE))
subset_data_s1$CASTHMA_CrudePrev <- as.numeric(as.character(subset_data_s1$CASTHMA_CrudePrev, na.rm = TRUE))
subset_data_s1$CERVICAL_CrudePrev <- as.numeric(as.character(subset_data_s1$CERVICAL_CrudePrev, na.rm = TRUE))
subset_data_s1$CHD_CrudePrev <- as.numeric(as.character(subset_data_s1$CHD_CrudePrev, na.rm = TRUE))
subset_data_s1$CHECKUP_CrudePrev <- as.numeric(as.character(subset_data_s1$CHECKUP_CrudePrev, na.rm = TRUE))
subset_data_s1$CHOLSCREEN_CrudePrev <- as.numeric(as.character(subset_data_s1$CHOLSCREEN_CrudePrev, na.rm = TRUE))
subset_data_s1$COLON_SCREEN_CrudePrev <- as.numeric(as.character(subset_data_s1$COLON_SCREEN_CrudePrev, na.rm = TRUE))
subset_data_s1$COPD_CrudePrev <- as.numeric(as.character(subset_data_s1$COPD_CrudePrev, na.rm = TRUE))
subset_data_s1$COREM_CrudePrev <- as.numeric(as.character(subset_data_s1$COREM_CrudePrev, na.rm = TRUE))
subset_data_s1$COREW_CrudePrev <- as.numeric(as.character(subset_data_s1$COREW_CrudePrev, na.rm = TRUE))
subset_data_s1$CSMOKING_CrudePrev <- as.numeric(as.character(subset_data_s1$CSMOKING_CrudePrev, na.rm = TRUE))
subset_data_s1$DIABETES_CrudePrev <- as.numeric(as.character(subset_data_s1$DIABETES_CrudePrev, na.rm = TRUE))
subset_data_s1$DENTAL_CrudePrev <- as.numeric(as.character(subset_data_s1$DENTAL_CrudePrev, na.rm = TRUE))
subset_data_s1$HIGHCHOL_CrudePrev <- as.numeric(as.character(subset_data_s1$HIGHCHOL_CrudePrev, na.rm = TRUE))
subset_data_s1$KIDNEY_CrudePrev <- as.numeric(as.character(subset_data_s1$KIDNEY_CrudePrev, na.rm = TRUE))
subset_data_s1$LPA_CrudePrev <- as.numeric(as.character(subset_data_s1$LPA_CrudePrev, na.rm = TRUE))
subset_data_s1$MAMMOUSE_CrudePrev <- as.numeric(as.character(subset_data_s1$MAMMOUSE_CrudePrev, na.rm = TRUE))
subset_data_s1$OBESITY_CrudePrev <- as.numeric(as.character(subset_data_s1$OBESITY_CrudePrev_P_div10, na.rm = TRUE))
subset_data_s1$PHLTH_CrudePrev <- as.numeric(as.character(subset_data_s1$PHLTH_CrudePrev, na.rm = TRUE))
subset_data_s1$SLEEP_CrudePrev <- as.numeric(as.character(subset_data_s1$SLEEP_CrudePrev, na.rm = TRUE))
subset_data_s1$STROKE_CrudePrev <- as.numeric(as.character(subset_data_s1$STROKE_CrudePrev, na.rm = TRUE))
subset_data_s1$TEETHLOST_CrudePrev <- as.numeric(as.character(subset_data_s1$TEETHLOST_CrudePrev, na.rm = TRUE))
subset_data_s1$DEPRESSION_CrudePrev <- as.numeric(as.character(subset_data_s1$DEPRESSION_CrudePrev, na.rm = TRUE))
subset_data_s1$GHLTH_CrudePrev <- as.numeric(as.character(subset_data_s1$GHLTH_CrudePrev, na.rm = TRUE))
subset_data_s1$Male_2015_2019 <- as.numeric(as.character(subset_data_s1$Male_2015_2019, na.rm = TRUE))
subset_data_s1$Female_2015_2019 <- as.numeric(as.character(subset_data_s1$Female_2015_2019, na.rm = TRUE))
subset_data_s1$Total.population <- as.numeric(as.character(subset_data_s1$Total.population, na.rm = TRUE))
subset_data_s1$White <- as.numeric(as.character(subset_data_s1$White, na.rm = TRUE))
subset_data_s1$Black <- as.numeric(as.character(subset_data_s1$Black, na.rm = TRUE))
subset_data_s1$All_OtherRaces <- as.numeric(as.character(subset_data_s1$All_OtherRaces, na.rm = TRUE))
subset_data_s1$TwoOrMoreRaces <- as.numeric(as.character(subset_data_s1$TwoOrMoreRaces, na.rm = TRUE))
subset_data_s1$Hispanic <- as.numeric(as.character(subset_data_s1$Hispanic, na.rm = TRUE))
subset_data_s1$NotHispanic <- as.numeric(as.character(subset_data_s1$NotHispanic, na.rm = TRUE))
subset_data_s1$Median_Income_2015_2019 <- as.numeric(as.character(subset_data_s1$Median_Income_2015_2019, na.rm = TRUE))
subset_data_s1$SPL_THEME1 <- as.numeric(as.character(subset_data_s1$SPL_THEME1, na.rm = TRUE))
subset_data_s1$EP_AGE65 <- as.numeric(as.character(subset_data_s1$EP_AGE65, na.rm = TRUE))
subset_data_s1$LCchangeMEAN.y <- as.numeric(as.character(subset_data_s1$LCchangeMEAN, na.rm = TRUE))
subset_data_s1$PCT_ImperviousSurfaces <- as.numeric(as.character(subset_data_s1$PCT_ImperviousSurfaces, na.rm = TRUE))
subset_data_s1$Temp_C_2013_2022 <- as.numeric(as.character(subset_data_s1$Temp_C_2013_2022, na.rm = TRUE))

#write.csv(subset_data_s1, "subset_for_tableone.csv", row.names = FALSE)
########### divide HI into tertiles #################################################
# create tertiles
tertiles <- quantile(subset_data_s1$HI_C_Change70_79_13_22, probs = seq(0, 1, by = 1/3), na.rm = TRUE)

# ensures same values are not divided into separate tertiles
subset_data_s1$HI_C_Change_Tertile <- cut(subset_data_s1$HI_C_Change70_79_13_22, breaks = unique(round(tertiles, 6)), labels = c("Low", "Medium", "High"), include.lowest = TRUE)

subset_data_s1$HI_C_Change_Tertile <- factor(subset_data_s1$HI_C_Change_Tertile, levels = c("Low", "Medium", "High"), ordered = TRUE)

########### tableone mean ######################
vars <- c("ACCESS7_CrudePrev","ARTHRITIS_CrudePrev", "BINGE_CrudePrev",
          "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev", "CASTHMA_CrudePrev", "CERVICAL_CrudePrev",
          "CHD_CrudePrev", "CHECKUP_CrudePrev", "CHOLSCREEN_CrudePrev", "COLON_SCREEN_CrudePrev", "COPD_CrudePrev",
          "COREM_CrudePrev", "COREW_CrudePrev", "CSMOKING_CrudePrev", "DIABETES_CrudePrev", "DENTAL_CrudePrev",
          "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev", "LPA_CrudePrev", "MAMMOUSE_CrudePrev",
          "OBESITY_CrudePrev_P_div10", "PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev",
          "DEPRESSION_CrudePrev", "GHLTH_CrudePrev", "Male_2015_2019", "Female_2015_2019", "Total.population", "White", "Black",
          "All_OtherRaces", "TwoOrMoreRaces", "Hispanic", "NotHispanic","Median_Income_2015_2019",  "SPL_THEME1", "EP_AGE65", "LCchangeMEAN.y", 
          "PCT_ImperviousSurfaces",  "Temp_C_2013_2022", "HI_C_Change70_79_13_22")

# Create the TableOne object, specifying medians for continuous variables and stratifying by TC_Quartile
tableone <- CreateTableOne(vars = vars, data = subset_data_s1, strata = "HI_C_Change_Tertile")
# Print Table 1
print(tableone)

# Extract the desired summary statistics from the TableOne object
tableone_summary <- print(tableone, printToggle = FALSE)

# Convert the summary statistics to a dataframe
tableone_df <- as.data.frame(tableone_summary)

# Print the dataframe
print(tableone_df)

# Write the results to a CSV file
write.csv(tableone_df, "Tableone_08_27_2024.csv")

# Calculate total population for each quartile
total_population <- subset_data_s1 %>%
  group_by(HI_C_Change_Tertile) %>%
  summarise(Total.population = sum(Total.population, na.rm = TRUE))

# Print the total population for each quartile
print(total_population)

mean_heat_index <- subset_data_s1 %>%
  group_by(HI_C_Change_Tertile) %>%
  summarise(HI_C_Change70_79_13_22 = mean(HI_C_Change70_79_13_22, na.rm = TRUE))

# Print the the mean heat index for each quartile
print(mean_heat_index)

sd_heat_index <- subset_data_s1 %>%
  group_by(HI_C_Change_Tertile) %>%
  summarise(HI_C_Change70_79_13_22 = sd(HI_C_Change70_79_13_22))
print(sd_heat_index)
