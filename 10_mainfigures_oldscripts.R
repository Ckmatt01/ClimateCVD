###########################################################################################################################
######### This code will run all linear models need to create the tables and figures for the main text#####################
######### install packages and load libraries #############################################################################
library("dplyr")
library("lme4")
library("ggplot2")
library("car")
library("pwr")
library("grid")
library("forestplot")
library("gridExtra")
library("MuMIn")
library("extrafont")
###############################################
#### read and merge csvs ######################
# combine all tracts and climate division data 
setwd(working_directory_data)
unjoined_tracts <- read.csv("Unjoined_Tracts2010_Edited.csv", header = TRUE, na.strings = "NA")
tracts <- read.csv("Tracts2010_CLIMDIV.csv", header = TRUE, na.strings = "NA")
climdiv <- rbind(tracts, unjoined_tracts)

# read and merge all other files
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
print(sum(places$TotalPopulation))
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
subset_data <- merged_data[, c("Pop18Over", "TotalPopulation", "CLIMDIV","HI_C_Change60_69_13_22", 
                               "HI_C_Change70_79_13_22", "HI_C_Change80_89_13_22", "HI_C_Change90_99_13_22", "HI_C_Change2000_2009_13_22",
                               "Temp_C_2013_2022", "Change_Temp_C_70_79_13_22", "Change_Temp_C_80_89_13_22", "Change_Temp_C_90_99_13_22",
                               "Change_Temp_C_2000_2009_13_22", "Change_RH_70_79_13_22", "Change_RH_80_89_13_22", "Change_RH_90_99_13_22",
                               "Change_RH_2000_2009_13_22", "PCT_ImperviousSurfaces",
                               "CHD_CrudePrev", "SPL_THEME1", "BPMED_CrudePrev",
                               "EP_AGE65", "CSMOKING_CrudePrev", "CHECKUP_CrudePrev", "STROKE_CrudePrev",
                               "OBESITY_CrudePrev", "CountyFIPS", "windU_diff70_79_13_22", "windU_diff80_89_13_22", "windU_diff90_99_13_22",
                               "windU_diff2000_2009_13_22", "windV_diff70_79_13_22", "windV_diff80_89_13_22", "windV_diff90_99_13_22", 
                               "windV_diff2000_2009_13_22", "latent_diff70_79_13_22","latent_diff80_89_13_22", "latent_diff90_99_13_22",
                               "latent_diff2000_2009_13_22", "solar_diff70_79_13_22", "solar_diff80_89_13_22", "solar_diff90_99_13_22", 
                               "solar_diff2000_2009_13_22", "thermal_diff70_79_13_22", "thermal_diff80_89_13_22", "thermal_diff90_99_13_22",
                               "thermal_diff2000_2009_13_22", "sensible_diff70_79_13_22", "sensible_diff80_89_13_22", "sensible_diff90_99_13_22", 
                               "sensible_diff2000_2009_13_22", "evap_diff70_79_13_22",  "evap_diff80_89_13_22",  "evap_diff90_99_13_22", 
                               "evap_diff2000_2009_13_22", "pressure_diff70_79_13_22", "pressure_diff80_89_13_22", "pressure_diff90_99_13_22",
                               "pressure_diff2000_2009_13_22", "precip_diff70_79_13_22", "precip_diff80_89_13_22", "precip_diff90_99_13_22", 
                               "precip_diff2000_2009_13_22","transpir_diff70_79_13_22", "transpir_diff80_89_13_22", "transpir_diff90_99_13_22", 
                               "transpir_diff2000_2009_13_22", 
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
#write.csv(merged_data, "All_Climate_Data_06_13_2024.csv")

############################### Scale variables ##############################################
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
#write.csv(subset_data, "Scaled_data_forARCgis_05_01_2024.csv", row.names = FALSE)
#### average change in climate metrics ######
# average anomaly size 
avg_HI_70_79_change <- mean(subset_data$HI_C_Change70_79_13_22)
print(avg_HI_70_79_change)
avg_temp_change <- mean(subset_data$Change_Temp_C_70_79_13_22)
print(avg_temp_change)
avg_rh_change <- mean(subset_data$Change_RH_70_79_13_22)
print(avg_rh_change)
avg_windu_change <- mean(subset_data$windU_diff70_79_13_22)
print(avg_windu_change)
avg_windv_change <- mean(subset_data$windV_diff70_79_13_22)
print(avg_windv_change)
avg_latent_change <- mean(subset_data$latent_diff70_79_13_22_P_Mill)
print(avg_latent_change)
avg_solar_change <- mean(subset_data$solar_diff70_79_13_22_P_Mill)
print(avg_solar_change)
avg_thermal_change <- mean(subset_data$thermal_diff70_79_13_22_P_Mill)
print(avg_thermal_change)
avg_sensible_change <- mean(subset_data$sensible_diff70_79_13_22_P_Mill)
print(avg_sensible_change)
avg_evap_change <- mean(subset_data$evap_diff70_79_13_22_P_x10)
print(avg_evap_change)
avg_press_change <- mean(subset_data$pressure_diff70_79_13_22_P_div10)
print(avg_press_change)
avg_precip_change <- mean(subset_data$precip_diff70_79_13_22_P_x10)
print(avg_precip_change)
avg_transpir_change <- mean(subset_data$transpir_diff70_79_13_22_P_x1000)
print(avg_transpir_change)
avg_downwards_solar_change <- mean(subset_data$downwards_solar_diff70_79_13_22_P_Mill)
print(avg_downwards_solar_change)
#### get complete cases and total population ##############################################################
# Perform listwise deletion
complete_cases <- complete.cases(subset_data)
cleaned_data <- subset_data[complete_cases, ]

# Calculate the sum of the TotalPopulation column in the cleaned data
total_pop <- sum(subset_data$TotalPopulation, na.rm = TRUE)

# Calculate the sum of the E_TOTPOP column in the cleaned data
sample_pop <- sum(cleaned_data$TotalPopulation, na.rm = TRUE)

percent_pop <- sample_pop/total_pop *100

# Display the result
cat("Sum of E_TOTPOP in cleaned data:", percent_pop, "\n")

##########################################################################################################################
####################################################### Figure 2 #####################################################
##########################################################################################################################
####################################################### CHD Figure 2 ###########################
# create empty list to store results from linear models
coefficients_chd <- list()
se_chd <- list()
lower_bound_chd <- list()
upper_bound_chd <- list()
# average CHD
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)

# average anomaly size 
avg_HI_70_79_change <- mean(subset_data$HI_C_Change70_79_13_22)
avg_temp_change <- mean(subset_data$Change_Temp_C_70_79_13_22)
avg_rh_change <- mean(subset_data$Change_RH_70_79_13_22)
avg_windu_change <- mean(subset_data$windU_diff70_79_13_22)
avg_windv_change <- mean(subset_data$windV_diff70_79_13_22)
avg_latent_change <- mean(subset_data$latent_diff70_79_13_22_P_Mill)
avg_solar_change <- mean(subset_data$solar_diff70_79_13_22_P_Mill)
avg_thermal_change <- mean(subset_data$thermal_diff70_79_13_22_P_Mill)
avg_sensible_change <- mean(subset_data$sensible_diff70_79_13_22_P_Mill)
avg_evap_change <- mean(subset_data$evap_diff70_79_13_22_P_x10)
avg_press_change <- mean(subset_data$pressure_diff70_79_13_22_P_div10)
avg_precip_change <- mean(subset_data$precip_diff70_79_13_22_P_x10)
avg_transpir_change <- mean(subset_data$transpir_diff70_79_13_22_P_x1000)
avg_downwards_solar_change <- mean(subset_data$downwards_solar_diff70_79_13_22_P_Mill)

#### ERA5 ############
chd_hi_70_79 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_hi_70_79)
chd_hi_70_79_summary <- summary(chd_hi_70_79)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"]*avg_HI_70_79_change
se_chd$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_chd$chd_hi_70_79 <- coefficients_chd$chd_hi_70_79 - 1.96 * se_chd$chd_hi_70_79
upper_bound_chd$chd_hi_70_79 <- coefficients_chd$chd_hi_70_79 + 1.96 * se_chd$chd_hi_70_79

# temp change
chd_temp <- lmer(CHD_CrudePrev ~  Change_Temp_C_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                   (1 | CountyFIPS), data = subset_data)
summary(chd_temp)
chd_temp_summary <- summary(chd_temp)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_temp <- coef(chd_temp_summary)["Change_Temp_C_70_79_13_22", "Estimate"] * avg_temp_change
se_chd$chd_temp <- coef(chd_temp_summary)["Change_Temp_C_70_79_13_22", "Std. Error"] 
lower_bound_chd$chd_temp <- coefficients_chd$chd_temp - 1.96 * se_chd$chd_temp
upper_bound_chd$chd_temp <- coefficients_chd$chd_temp + 1.96 * se_chd$chd_temp

# relative humidity
chd_rh <- lmer(CHD_CrudePrev ~  Change_RH_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                 (1 | CountyFIPS), data = subset_data)

summary(chd_rh)
chd_rh_summary <- summary(chd_rh)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_rh <- coef(chd_rh_summary)["Change_RH_70_79_13_22", "Estimate"] * avg_rh_change
se_chd$chd_rh <- coef(chd_rh_summary)["Change_RH_70_79_13_22", "Std. Error"]
print(se_chd$chd_rh)
lower_bound_chd$chd_rh <- coefficients_chd$chd_rh - 1.96 * se_chd$chd_rh
upper_bound_chd$chd_rh <- coefficients_chd$chd_rh + 1.96 * se_chd$chd_rh


# wind u
chd_windu <- lmer(CHD_CrudePrev ~  windU_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                    (1 | CountyFIPS), data = subset_data)
summary(chd_windu)
chd_windu_summary <- summary(chd_windu)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_windu <- coef(chd_windu_summary)["windU_diff70_79_13_22", "Estimate"] * avg_windu_change
se_chd$chd_windu <- coef(chd_windu_summary)["windU_diff70_79_13_22", "Std. Error"]
lower_bound_chd$chd_windu <- coefficients_chd$chd_windu - 1.96 * se_chd$chd_windu
upper_bound_chd$chd_windu <- coefficients_chd$chd_windu + 1.96 * se_chd$chd_windu

# wind v
chd_windv <- lmer(CHD_CrudePrev ~  windV_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                    (1 | CountyFIPS), data = subset_data)
summary(chd_windv)
chd_windv_summary <- summary(chd_windv)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_windv <- coef(chd_windv_summary)["windV_diff70_79_13_22", "Estimate"] * avg_windv_change
se_chd$chd_windv <- coef(chd_windv_summary)["windV_diff70_79_13_22", "Std. Error"] 
lower_bound_chd$chd_windv <- coefficients_chd$chd_windv - 1.96 * se_chd$chd_windv 
upper_bound_chd$chd_windv <- coefficients_chd$chd_windv + 1.96 * se_chd$chd_windv 

# latent heat flux 
chd_latent <- lmer(CHD_CrudePrev ~  latent_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                   + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                     (1 | CountyFIPS), data = subset_data)
summary(chd_latent)
chd_latent_summary <- summary(chd_latent)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_latent <- coef(chd_latent_summary)["latent_diff70_79_13_22_P_Mill", "Estimate"] * avg_latent_change
se_chd$chd_latent <- coef(chd_latent_summary)["latent_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_chd$chd_latent <- coefficients_chd$chd_latent - 1.96 * se_chd$chd_latent
upper_bound_chd$chd_latent <- coefficients_chd$chd_latent + 1.96 * se_chd$chd_latent 

# solar radiation
chd_solar <- lmer(CHD_CrudePrev ~  solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                  + Temp_C_2013_2022 + LCchangeMEAN +
                    (1 | CountyFIPS), data = subset_data)
summary(chd_solar)
chd_solar_summary <- summary(chd_solar)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_solar <- coef(chd_solar_summary)["solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_solar_change
se_chd$chd_solar <- coef(chd_solar_summary)["solar_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_chd$chd_solar <- coefficients_chd$chd_solar - 1.96 * se_chd$chd_solar 
upper_bound_chd$chd_solar <- coefficients_chd$chd_solar + 1.96 * se_chd$chd_solar 

# thermal radiation
chd_thermal <- lmer(CHD_CrudePrev ~  thermal_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                    + Temp_C_2013_2022 + LCchangeMEAN +
                      (1 | CountyFIPS), data = subset_data)
summary(chd_thermal)
chd_thermal_summary <- summary(chd_thermal)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_thermal <- coef(chd_thermal_summary)["thermal_diff70_79_13_22_P_Mill", "Estimate"] * avg_thermal_change
se_chd$chd_thermal <- coef(chd_thermal_summary)["thermal_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_chd$chd_thermal <- coefficients_chd$chd_thermal - 1.96 * se_chd$chd_thermal 
upper_bound_chd$chd_thermal <- coefficients_chd$chd_thermal + 1.96 * se_chd$chd_thermal 

# sensible heat flux
chd_sensible <- lmer(CHD_CrudePrev ~  sensible_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_sensible)
chd_sensible_summary <- summary(chd_sensible)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_sensible <- coef(chd_sensible_summary)["sensible_diff70_79_13_22_P_Mill", "Estimate"] * avg_sensible_change
se_chd$chd_sensible <- coef(chd_sensible_summary)["sensible_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_chd$chd_sensible <- coefficients_chd$chd_sensible - 1.96 * se_chd$chd_sensible 
upper_bound_chd$chd_sensible <- coefficients_chd$chd_sensible + 1.96 * se_chd$chd_sensible 

# evaporation
chd_evap <- lmer(CHD_CrudePrev ~  evap_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                 + Temp_C_2013_2022 + LCchangeMEAN +
                   (1 | CountyFIPS), data = subset_data)
summary(chd_evap)
chd_evap_summary <- summary(chd_evap)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_evap <- coef(chd_evap_summary)["evap_diff70_79_13_22_P_x10", "Estimate"] * avg_evap_change
se_chd$chd_evap <- coef(chd_evap_summary)["evap_diff70_79_13_22_P_x10", "Std. Error"] 
lower_bound_chd$chd_evap <- coefficients_chd$chd_evap - 1.96 * se_chd$chd_evap 
upper_bound_chd$chd_evap <- coefficients_chd$chd_evap + 1.96 * se_chd$chd_evap 

# pressure
chd_press <- lmer(CHD_CrudePrev ~  pressure_diff70_79_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                  + Temp_C_2013_2022 + LCchangeMEAN +
                    (1 | CountyFIPS), data = subset_data)
summary(chd_press)
chd_press_summary <- summary(chd_press)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_press <- coef(chd_press_summary)["pressure_diff70_79_13_22_P_div10", "Estimate"] * avg_press_change
se_chd$chd_press <- coef(chd_press_summary)["pressure_diff70_79_13_22_P_div10", "Std. Error"] 
lower_bound_chd$chd_press <- coefficients_chd$chd_press - 1.96 * se_chd$chd_press 
upper_bound_chd$chd_press <- coefficients_chd$chd_press + 1.96 * se_chd$chd_press 

# precipitation
chd_precip <- lmer(CHD_CrudePrev ~  precip_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                   + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                   + Temp_C_2013_2022 + LCchangeMEAN +
                     (1 | CountyFIPS), data = subset_data)
summary(chd_precip)
chd_precip_summary <- summary(chd_precip)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_precip <- coef(chd_precip_summary)["precip_diff70_79_13_22_P_x10", "Estimate"] * avg_precip_change
se_chd$chd_precip <- coef(chd_precip_summary)["precip_diff70_79_13_22_P_x10", "Std. Error"] 
lower_bound_chd$chd_precip <- coefficients_chd$chd_precip - 1.96 * se_chd$chd_precip 
upper_bound_chd$chd_precip <- coefficients_chd$chd_precip + 1.96 * se_chd$chd_precip 

# transpiration
chd_transpir <- lmer(CHD_CrudePrev ~  transpir_diff70_79_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_transpir)
chd_transpir_summary <- summary(chd_transpir)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_transpir <- coef(chd_transpir_summary)["transpir_diff70_79_13_22_P_x1000", "Estimate"] * avg_transpir_change
se_chd$chd_transpir <- coef(chd_transpir_summary)["transpir_diff70_79_13_22_P_x1000", "Std. Error"] 
lower_bound_chd$chd_transpir <- coefficients_chd$chd_transpir - 1.96 * se_chd$chd_transpir 
upper_bound_chd$chd_transpir <- coefficients_chd$chd_transpir + 1.96 * se_chd$chd_transpir 

# downwards solar radiation
chd_downwards_solar <- lmer(CHD_CrudePrev ~  downwards_solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                            + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_downwards_solar)
chd_downwards_solar_summary <- summary(chd_downwards_solar)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_downwards_solar <- coef(chd_downwards_solar_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_downwards_solar_change
se_chd$chd_downwards_solar <- coef(chd_downwards_solar_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_chd$chd_downwards_solar <- coefficients_chd$chd_downwards_solar - 1.96 * se_chd$chd_downwards_solar 
upper_bound_chd$chd_downwards_solar <- coefficients_chd$chd_downwards_solar + 1.96 * se_chd$chd_downwards_solar 

#### Combine the lists into a data frame ######################
chd_fig2 <- data.frame(
  Variable = c("Heat Index",
               "Air Temperature", "Humidity", 
               "Eastward Wind", "Northward Wind", "Latent Heat Flux", "Surface-Absorbed Sunlight", 
               "Thermal Radiation", "Sensible Heat Flux", "Evaporation", 
               "Surface Pressure", "Precipitation", "Transpiration", "Sunlight"),
  SE = unlist(se_chd),
  Effect_Anomaly_Size = unlist(coefficients_chd),
  Lower_CI = unlist(lower_bound_chd),
  Upper_CI = unlist(upper_bound_chd)
)

chd_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", 
                                chd_fig2$Lower_CI,
                                chd_fig2$Upper_CI)

####################################################### Stroke Figure 2 ##################################################
################################################################################################
# Create empty lists to store results from linear models
coefficients_stroke <- list()
se_stroke <- list()
pvalue_stroke <- list()
lower_bound_stroke <- list()
upper_bound_stroke <- list()

# average stroke
avg_STROKE <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

# average anomaly size 
avg_HI_70_79_change <- mean(subset_data$HI_C_Change70_79_13_22)
avg_temp_change <- mean(subset_data$Change_Temp_C_70_79_13_22)
avg_rh_change <- mean(subset_data$Change_RH_70_79_13_22)
avg_windu_change <- mean(subset_data$windU_diff70_79_13_22)
avg_windv_change <- mean(subset_data$windV_diff70_79_13_22)
avg_latent_change <- mean(subset_data$latent_diff70_79_13_22_P_Mill)
avg_solar_change <- mean(subset_data$solar_diff70_79_13_22_P_Mill)
avg_thermal_change <- mean(subset_data$thermal_diff70_79_13_22_P_Mill)
avg_sensible_change <- mean(subset_data$sensible_diff70_79_13_22_P_Mill)
avg_evap_change <- mean(subset_data$evap_diff70_79_13_22_P_x10)
avg_press_change <- mean(subset_data$pressure_diff70_79_13_22_P_div10)
avg_precip_change <- mean(subset_data$precip_diff70_79_13_22_P_x10)
avg_transpir_change <- mean(subset_data$transpir_diff70_79_13_22_P_x1000)
avg_downwards_solar_change <- mean(subset_data$downwards_solar_diff70_79_13_22_P_Mill)

#### ERA  #############
# HI change 70-80
stroke_hi_70_79 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_70_79)
stroke_hi_70_79_summary <- summary(stroke_hi_70_79)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"] * avg_HI_70_79_change
se_stroke$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_stroke$stroke_hi_70_79 <- coefficients_stroke$stroke_hi_70_79 - 1.96 * se_stroke$stroke_hi_70_79
upper_bound_stroke$stroke_hi_70_79 <- coefficients_stroke$stroke_hi_70_79 + 1.96 * se_stroke$stroke_hi_70_79

# temp change for STROKE
stroke_temp <- lmer(STROKE_CrudePrev ~  Change_Temp_C_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                      (1 | CountyFIPS), data = subset_data)
summary(stroke_temp)
stroke_temp_summary <- summary(stroke_temp)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_temp <- coef(stroke_temp_summary)["Change_Temp_C_70_79_13_22", "Estimate"] * avg_temp_change
se_stroke$stroke_temp <- coef(stroke_temp_summary)["Change_Temp_C_70_79_13_22", "Std. Error"] 
lower_bound_stroke$stroke_temp <- coefficients_stroke$stroke_temp - 1.96 * se_stroke$stroke_temp
upper_bound_stroke$stroke_temp <- coefficients_stroke$stroke_temp + 1.96 * se_stroke$stroke_temp

# relative humidity for STROKE
stroke_rh <- lmer(STROKE_CrudePrev ~  Change_RH_70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                  + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                    (1 | CountyFIPS), data = subset_data)

summary(stroke_rh)
stroke_rh_summary <- summary(stroke_rh)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_rh <- coef(stroke_rh_summary)["Change_RH_70_79_13_22", "Estimate"] * avg_rh_change
se_stroke$stroke_rh <- coef(stroke_rh_summary)["Change_RH_70_79_13_22", "Std. Error"]
lower_bound_stroke$stroke_rh <- coefficients_stroke$stroke_rh - 1.96 * se_stroke$stroke_rh
upper_bound_stroke$stroke_rh <- coefficients_stroke$stroke_rh + 1.96 * se_stroke$stroke_rh


# wind u for STROKE
stroke_windu <- lmer(STROKE_CrudePrev ~  windU_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(stroke_windu)
stroke_windu_summary <- summary(stroke_windu)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_windu <- coef(stroke_windu_summary)["windU_diff70_79_13_22", "Estimate"] * avg_windu_change
se_stroke$stroke_windu <- coef(stroke_windu_summary)["windU_diff70_79_13_22", "Std. Error"]
lower_bound_stroke$stroke_windu <- coefficients_stroke$stroke_windu - 1.96 * se_stroke$stroke_windu
upper_bound_stroke$stroke_windu <- coefficients_stroke$stroke_windu + 1.96 * se_stroke$stroke_windu

# wind v for STROKE
stroke_windv <- lmer(STROKE_CrudePrev ~  windV_diff70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(stroke_windv)
stroke_windv_summary <- summary(stroke_windv)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_windv <- coef(stroke_windv_summary)["windV_diff70_79_13_22", "Estimate"] * avg_windv_change
se_stroke$stroke_windv <- coef(stroke_windv_summary)["windV_diff70_79_13_22", "Std. Error"] 
lower_bound_stroke$stroke_windv <- coefficients_stroke$stroke_windv - 1.96 * se_stroke$stroke_windv 
upper_bound_stroke$stroke_windv <- coefficients_stroke$stroke_windv + 1.96 * se_stroke$stroke_windv 

# Latent heat flux for STROKE
stroke_latent <- lmer(STROKE_CrudePrev ~  latent_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                      + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                        (1 | CountyFIPS), data = subset_data)
summary(stroke_latent)
stroke_latent_summary <- summary(stroke_latent)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_latent <- coef(stroke_latent_summary)["latent_diff70_79_13_22_P_Mill", "Estimate"] * avg_latent_change
se_stroke$stroke_latent <- coef(stroke_latent_summary)["latent_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_stroke$stroke_latent <- coefficients_stroke$stroke_latent - 1.96 * se_stroke$stroke_latent
upper_bound_stroke$stroke_latent <- coefficients_stroke$stroke_latent + 1.96 * se_stroke$stroke_latent 

# Solar radiation for STROKE
stroke_solar <- lmer(STROKE_CrudePrev ~  solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(stroke_solar)
stroke_solar_summary <- summary(stroke_solar)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_solar <- coef(stroke_solar_summary)["solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_solar_change
se_stroke$stroke_solar <- coef(stroke_solar_summary)["solar_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_stroke$stroke_solar <- coefficients_stroke$stroke_solar - 1.96 * se_stroke$stroke_solar 
upper_bound_stroke$stroke_solar <- coefficients_stroke$stroke_solar + 1.96 * se_stroke$stroke_solar 

# Thermal radiation for STROKE
stroke_thermal <- lmer(STROKE_CrudePrev ~  thermal_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                       + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)
summary(stroke_thermal)
stroke_thermal_summary <- summary(stroke_thermal)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_thermal <- coef(stroke_thermal_summary)["thermal_diff70_79_13_22_P_Mill", "Estimate"] * avg_thermal_change
se_stroke$stroke_thermal <- coef(stroke_thermal_summary)["thermal_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_stroke$stroke_thermal <- coefficients_stroke$stroke_thermal - 1.96 * se_stroke$stroke_thermal 
upper_bound_stroke$stroke_thermal <- coefficients_stroke$stroke_thermal + 1.96 * se_stroke$stroke_thermal 

# Sensible heat flux for STROKE
stroke_sensible <- lmer(STROKE_CrudePrev ~  sensible_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_sensible)
stroke_sensible_summary <- summary(stroke_sensible)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_sensible <- coef(stroke_sensible_summary)["sensible_diff70_79_13_22_P_Mill", "Estimate"] * avg_sensible_change
se_stroke$stroke_sensible <- coef(stroke_sensible_summary)["sensible_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_stroke$stroke_sensible <- coefficients_stroke$stroke_sensible - 1.96 * se_stroke$stroke_sensible 
upper_bound_stroke$stroke_sensible <- coefficients_stroke$stroke_sensible + 1.96 * se_stroke$stroke_sensible 

# Evaporation for STROKE
stroke_evap <- lmer(STROKE_CrudePrev ~  evap_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                    + Temp_C_2013_2022 + LCchangeMEAN +
                      (1 | CountyFIPS), data = subset_data)
summary(stroke_evap)
stroke_evap_summary <- summary(stroke_evap)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_evap <- coef(stroke_evap_summary)["evap_diff70_79_13_22_P_x10", "Estimate"] * avg_evap_change
se_stroke$stroke_evap <- coef(stroke_evap_summary)["evap_diff70_79_13_22_P_x10", "Std. Error"] 
lower_bound_stroke$stroke_evap <- coefficients_stroke$stroke_evap - 1.96 * se_stroke$stroke_evap 
upper_bound_stroke$stroke_evap <- coefficients_stroke$stroke_evap + 1.96 * se_stroke$stroke_evap 

# Pressure for STROKE
stroke_press <- lmer(STROKE_CrudePrev ~  pressure_diff70_79_13_22_P_div10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                     + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(stroke_press)
stroke_press_summary <- summary(stroke_press)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_press <- coef(stroke_press_summary)["pressure_diff70_79_13_22_P_div10", "Estimate"] * avg_press_change
se_stroke$stroke_press <- coef(stroke_press_summary)["pressure_diff70_79_13_22_P_div10", "Std. Error"] 
lower_bound_stroke$stroke_press <- coefficients_stroke$stroke_press - 1.96 * se_stroke$stroke_press 
upper_bound_stroke$stroke_press <- coefficients_stroke$stroke_press + 1.96 * se_stroke$stroke_press 

# Precipitation for STROKE
stroke_precip <- lmer(STROKE_CrudePrev ~  precip_diff70_79_13_22_P_x10 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                      + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                      + Temp_C_2013_2022 + LCchangeMEAN +
                        (1 | CountyFIPS), data = subset_data)
summary(stroke_precip)
stroke_precip_summary <- summary(stroke_precip)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_precip <- coef(stroke_precip_summary)["precip_diff70_79_13_22_P_x10", "Estimate"] * avg_precip_change
se_stroke$stroke_precip <- coef(stroke_precip_summary)["precip_diff70_79_13_22_P_x10", "Std. Error"] 
lower_bound_stroke$stroke_precip <- coefficients_stroke$stroke_precip - 1.96 * se_stroke$stroke_precip 
upper_bound_stroke$stroke_precip <- coefficients_stroke$stroke_precip + 1.96 * se_stroke$stroke_precip 

# Transpiration for STROKE
stroke_transpir <- lmer(STROKE_CrudePrev ~  transpir_diff70_79_13_22_P_x1000 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                        + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_transpir)
stroke_transpir_summary <- summary(stroke_transpir)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_transpir <- coef(stroke_transpir_summary)["transpir_diff70_79_13_22_P_x1000", "Estimate"] * avg_transpir_change
se_stroke$stroke_transpir <- coef(stroke_transpir_summary)["transpir_diff70_79_13_22_P_x1000", "Std. Error"] 
lower_bound_stroke$stroke_transpir <- coefficients_stroke$stroke_transpir - 1.96 * se_stroke$stroke_transpir 
upper_bound_stroke$stroke_transpir <- coefficients_stroke$stroke_transpir + 1.96 * se_stroke$stroke_transpir

# Downwards solar radation for STROKE
stroke_down_solar <- lmer(STROKE_CrudePrev ~  downwards_solar_diff70_79_13_22_P_Mill + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 
                          + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(stroke_down_solar)
stroke_down_solar_summary <- summary(stroke_down_solar)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_down_solar <- coef(stroke_down_solar_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Estimate"] * avg_downwards_solar_change
se_stroke$stroke_down_solar <- coef(stroke_down_solar_summary)["downwards_solar_diff70_79_13_22_P_Mill", "Std. Error"] 
lower_bound_stroke$stroke_down_solar <- coefficients_stroke$stroke_down_solar - 1.96 * se_stroke$stroke_down_solar 
upper_bound_stroke$stroke_down_solar <- coefficients_stroke$stroke_down_solar + 1.96 * se_stroke$stroke_down_solar

#### Combine the lists into a data frame ######################
stroke_fig2 <- data.frame(
  Variable = c("Heat Index",
               "Air Temperature", "Humidity", 
               "Eastward Wind", "Northward Wind", "Latent Heat Flux", "Surface-Absorbed Sunlight", 
               "Thermal Radiation", "Sensible Heat Flux", "Evaporation", 
               "Surface Pressure", "Precipitation", "Transpiration", "Sunlight"),
  SE = unlist(se_stroke),
  Effect_Anomaly_Size = unlist(coefficients_stroke),
  Lower_CI = unlist(lower_bound_stroke),
  Upper_CI = unlist(upper_bound_stroke)
)

stroke_fig2$Combined_CI <- sprintf("(%.2f, %.2f)", 
                                   stroke_fig2$Lower_CI,
                                   stroke_fig2$Upper_CI)

combined_fig2 <- cbind(chd_fig2$Variable, chd_fig2$Effect_Anomaly_Size, chd_fig2$Combined_CI, stroke_fig2$Effect_Anomaly_Size, stroke_fig2$Combined_CI)
write.csv(combined_fig2, "figure2_figureS5.csv", row.names = FALSE)

####################################################### Figure 2 forest plot #########################################
# Combine the datasets 
combined_fig2 <- rbind(
  cbind(chd_fig2, Outcome = "CHD"),
  cbind(stroke_fig2, Outcome = "Stroke")
)

# Extract variable names and create groups based on their prefixes
combined_fig2$Variable_Group <- sub("_.*", "", combined_fig2$Variable)

# To check the new grouping
table(combined_fig2$Variable_Group)

# Define the custom order for Variable_Group
custom_order <- c("Heat Index", "Air Temperature", "Humidity", "Surface-Absorbed Sunlight", 
                  "Transpiration", "Eastward Wind", "Sunlight", 
                  "Sensible Heat Flux", "Precipitation", "Northward Wind", 
                  "Thermal Radiation", "Latent Heat Flux", 
                  "Evaporation", "Surface Pressure")

# Reverse the order of the custom_order
reversed_order <- custom_order

# Convert Variable_Group to a factor with the reversed custom order
combined_fig2$Variable_Group <- factor(combined_fig2$Variable_Group, levels = reversed_order)

# Sort the data by Variable_Group, Outcome, and Effect_Anomaly_Size
combined_fig2$Outcome <- factor(combined_fig2$Outcome, levels = c("Stroke", "CHD"))

# Sort the data by Variable_Group (reverse order) and then by Outcome (Stroke first)
combined_fig2 <- combined_fig2[order(combined_fig2$Variable_Group, combined_fig2$Outcome), ]

# Create a combined variable that includes both the Variable and Outcome for plotting
combined_fig2 <- combined_fig2 %>%
  mutate(Variable_Outcome = paste(Variable, Outcome, sep = " - "))

# Ensure that Variable_Outcome follows the same order as in the sorted combined_fig2
combined_fig2$Variable_Outcome <- factor(combined_fig2$Variable_Outcome, levels = unique(combined_fig2$Variable_Outcome))
# write data to csv for figure 
write.csv(combined_fig2, "Figure2_Data_09_16_2024.csv")


#font_import()          # This might take a few minutes, and you'll need to do this once
#loadfonts(device = "win")  # For Windows users; skip for other systems

# Plotting with ggplot2
fig2 <- ggplot(combined_fig2, aes(x = as.numeric(Effect_Anomaly_Size), y = Variable_Outcome, color = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Add black vertical line at x = 0
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray") +  # Add black vertical line at x = 0.5
  geom_vline(xintercept = -1, linetype = "dashed", color = "gray") +  # Add black vertical line at x = -1
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # Add black vertical line at x = 0.5
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +  # Add black vertical line at x = 1
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray") +  # Add black vertical line at x = 1.5
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray") +  # Add black vertical line at x = 2
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = as.numeric(Lower_CI), xmax = as.numeric(Upper_CI)), height = 0.7) +
  scale_x_continuous(limits = c(min(combined_fig2$Lower_CI), max(combined_fig2$Upper_CI))) +
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
    panel.grid.minor = element_blank(),   # Remove minor grid lines
  )

plot(fig2)

# Save the plot with a specific resolution
ggsave("Figure2.png", plot = fig2, width = 3.6, height = 5, dpi = 600)

####################################################### CHD Figure 3 ###########################
# create empty list to store results from linear models
coefficients_chd <- list()
se_chd <- list()
lower_bound_chd <- list()
upper_bound_chd <- list()

# average anomaly size 
avg_HI_70_79_change <- mean(subset_data$HI_C_Change70_79_13_22)

avg_daysAbove32 <- mean(subset_data$Anomaly_DaysAbove32_1322)
#avg_daysAbove34 <- mean(subset_data$Anomaly_DaysAbove34_1322)
avg_daysAbove36 <- mean(subset_data$Anomaly_DaysAbove36_1322)
#avg_daysAbove38 <- mean(subset_data$Anomaly_DaysAbove38_1322)
avg_daysAbove41 <- mean(subset_data$Anomaly_DaysAbove41_1322)


avg_hoursAbove32 <- mean(subset_data$Anomaly_HoursAbove32_1322)
#avg_hoursAbove34 <- mean(subset_data$Anomaly_HoursAbove34_1322)
avg_hoursAbove36 <- mean(subset_data$Anomaly_HoursAbove36_1322)
#avg_hoursAbove38 <- mean(subset_data$Anomaly_HoursAbove38_1322)
avg_hoursAbove41 <- mean(subset_data$Anomaly_HoursAbove41_1322)

avg_summerMeanHIAnomaly <- mean(subset_data$Anomaly_SummerMeanHI_1322)
avg_winterMeanHIAnomaly <- mean(subset_data$Anomaly_WinterMeanHI_1322)

avg_nightHIother <- mean(subset_data$Anomaly_MeanNighttimeHI_Other_1322)
avg_nightHIsummer <- mean(subset_data$Anomaly_MeanNighttimeHI_Summer_1322)
avg_nightHIwinter <- mean(subset_data$Anomaly_MeanNighttimeHI_Winter_1322)
avg_nightHIannual <- mean(subset_data$Anomaly_MeanNighttimeHI_Annual_1322)

avg_dayMeanHIother <- mean(subset_data$Anomaly_MeanDaytimeHI_Other_1322)
avg_dayMeanHIsummer <- mean(subset_data$Anomaly_MeanDaytimeHI_Summer_1322)
avg_dayMeanHIwinter <- mean(subset_data$Anomaly_MeanDaytimeHI_Winter_1322)
avg_dayMeanHIannual <- mean(subset_data$Anomaly_MeanDaytimeHI_Annual_1322)

avg_medianSummerHImax <- mean(subset_data$Anomaly_MedianSummerMaxHI_1322)
avg_percentile95SummerHImax <- mean(subset_data$Anomaly_Percentile95SummerMaxHI_1322)
avg_meanSummerHImax <- mean(subset_data$Anomaly_MeanSummerMaxHI_1322)

#### seasonal ###########
chd_hi_70_79 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                     + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                       (1 | CountyFIPS), data = subset_data)
summary(chd_hi_70_79)
chd_hi_70_79_summary <- summary(chd_hi_70_79)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"]  * avg_HI_70_79_change
se_chd$chd_hi_70_79 <- coef(chd_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_chd$chd_hi_70_79 <- coefficients_chd$chd_hi_70_79 - 1.96 * se_chd$chd_hi_70_79
upper_bound_chd$chd_hi_70_79 <- coefficients_chd$chd_hi_70_79 + 1.96 * se_chd$chd_hi_70_79

# daysAbove32 
chd_daysAbove32 <- lmer(CHD_CrudePrev ~  Anomaly_DaysAbove32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_daysAbove32)
chd_daysAbove32_summary <- summary(chd_daysAbove32)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_daysAbove32 <- coef(chd_daysAbove32_summary)["Anomaly_DaysAbove32_1322", "Estimate"] * avg_daysAbove32
se_chd$chd_daysAbove32 <- coef(chd_daysAbove32_summary)["Anomaly_DaysAbove32_1322", "Std. Error"]
lower_bound_chd$chd_daysAbove32 <- coefficients_chd$chd_daysAbove32 - 1.96 * se_chd$chd_daysAbove32
upper_bound_chd$chd_daysAbove32 <- coefficients_chd$chd_daysAbove32 + 1.96 * se_chd$chd_daysAbove32

# daysAbove34 
#chd_daysAbove34 <- lmer(CHD_CrudePrev ~  Anomaly_DaysAbove34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(chd_daysAbove34)
#chd_daysAbove34_summary <- summary(chd_daysAbove34)

# Extracting coefficients, standard errors, and p-values
#coefficients_chd$chd_daysAbove34 <- coef(chd_daysAbove34_summary)["Anomaly_DaysAbove34_1322", "Estimate"] * avg_daysAbove34
#se_chd$chd_daysAbove34 <- coef(chd_daysAbove34_summary)["Anomaly_DaysAbove34_1322", "Std. Error"]
#lower_bound_chd$chd_daysAbove34 <- coefficients_chd$chd_daysAbove34 - 1.96 * se_chd$chd_daysAbove34
#upper_bound_chd$chd_daysAbove34 <- coefficients_chd$chd_daysAbove34 + 1.96 * se_chd$chd_daysAbove34

# daysAbove36 
chd_daysAbove36 <- lmer(CHD_CrudePrev ~  Anomaly_DaysAbove36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_daysAbove36)
chd_daysAbove36_summary <- summary(chd_daysAbove36)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_daysAbove36 <- coef(chd_daysAbove36_summary)["Anomaly_DaysAbove36_1322", "Estimate"] * avg_daysAbove36
se_chd$chd_daysAbove36 <- coef(chd_daysAbove36_summary)["Anomaly_DaysAbove36_1322", "Std. Error"]
lower_bound_chd$chd_daysAbove36 <- coefficients_chd$chd_daysAbove36 - 1.96 * se_chd$chd_daysAbove36
upper_bound_chd$chd_daysAbove36 <- coefficients_chd$chd_daysAbove36 + 1.96 * se_chd$chd_daysAbove36

# daysAbove38 
#chd_daysAbove38 <- lmer(CHD_CrudePrev ~  Anomaly_DaysAbove38_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(chd_daysAbove38)
#chd_daysAbove38_summary <- summary(chd_daysAbove38)

# Extracting coefficients, standard errors, and p-values
#coefficients_chd$chd_daysAbove38 <- coef(chd_daysAbove38_summary)["Anomaly_DaysAbove38_1322", "Estimate"] * avg_daysAbove38
#se_chd$chd_daysAbove38 <- coef(chd_daysAbove38_summary)["Anomaly_DaysAbove38_1322", "Std. Error"]
#lower_bound_chd$chd_daysAbove38 <- coefficients_chd$chd_daysAbove38 - 1.96 * se_chd$chd_daysAbove38
#upper_bound_chd$chd_daysAbove38 <- coefficients_chd$chd_daysAbove38 + 1.96 * se_chd$chd_daysAbove38

# daysAbove41 
chd_daysAbove41 <- lmer(CHD_CrudePrev ~  Anomaly_DaysAbove41_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_daysAbove41)
chd_daysAbove41_summary <- summary(chd_daysAbove41)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_daysAbove41 <- coef(chd_daysAbove41_summary)["Anomaly_DaysAbove41_1322", "Estimate"] * avg_daysAbove41
se_chd$chd_daysAbove41 <- coef(chd_daysAbove41_summary)["Anomaly_DaysAbove41_1322", "Std. Error"]
lower_bound_chd$chd_daysAbove41 <- coefficients_chd$chd_daysAbove41 - 1.96 * se_chd$chd_daysAbove41
upper_bound_chd$chd_daysAbove41 <- coefficients_chd$chd_daysAbove41 + 1.96 * se_chd$chd_daysAbove41

# hoursAbove32 
chd_hoursAbove32 <- lmer(CHD_CrudePrev ~  Anomaly_HoursAbove32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_hoursAbove32)
chd_hoursAbove32_summary <- summary(chd_hoursAbove32)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_hoursAbove32 <- coef(chd_hoursAbove32_summary)["Anomaly_HoursAbove32_1322", "Estimate"] * avg_hoursAbove32
se_chd$chd_hoursAbove32 <- coef(chd_hoursAbove32_summary)["Anomaly_HoursAbove32_1322", "Std. Error"]
lower_bound_chd$chd_hoursAbove32 <- coefficients_chd$chd_hoursAbove32 - 1.96 * se_chd$chd_hoursAbove32
upper_bound_chd$chd_hoursAbove32 <- coefficients_chd$chd_hoursAbove32 + 1.96 * se_chd$chd_hoursAbove32

# hoursAbove34 
#chd_hoursAbove34 <- lmer(CHD_CrudePrev ~  Anomaly_HoursAbove34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(chd_hoursAbove34)
#chd_hoursAbove34_summary <- summary(chd_hoursAbove34)

# Extracting coefficients, standard errors, and p-values
#coefficients_chd$chd_hoursAbove34 <- coef(chd_hoursAbove34_summary)["Anomaly_HoursAbove34_1322", "Estimate"] * avg_hoursAbove34
#se_chd$chd_hoursAbove34 <- coef(chd_hoursAbove34_summary)["Anomaly_HoursAbove34_1322", "Std. Error"]
#lower_bound_chd$chd_hoursAbove34 <- coefficients_chd$chd_hoursAbove34 - 1.96 * se_chd$chd_hoursAbove34
#upper_bound_chd$chd_hoursAbove34 <- coefficients_chd$chd_hoursAbove34 + 1.96 * se_chd$chd_hoursAbove34

# hoursAbove36 
chd_hoursAbove36 <- lmer(CHD_CrudePrev ~  Anomaly_HoursAbove36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_hoursAbove36)
chd_hoursAbove36_summary <- summary(chd_hoursAbove36)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_hoursAbove36 <- coef(chd_hoursAbove36_summary)["Anomaly_HoursAbove36_1322", "Estimate"] * avg_hoursAbove36
se_chd$chd_hoursAbove36 <- coef(chd_hoursAbove36_summary)["Anomaly_HoursAbove36_1322", "Std. Error"]
lower_bound_chd$chd_hoursAbove36 <- coefficients_chd$chd_hoursAbove36 - 1.96 * se_chd$chd_hoursAbove36
upper_bound_chd$chd_hoursAbove36 <- coefficients_chd$chd_hoursAbove36 + 1.96 * se_chd$chd_hoursAbove36

# hoursAbove38 
#chd_hoursAbove38 <- lmer(CHD_CrudePrev ~  Anomaly_HoursAbove38_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(chd_hoursAbove38)
#chd_hoursAbove38_summary <- summary(chd_hoursAbove38)

# Extracting coefficients, standard errors, and p-values
#coefficients_chd$chd_hoursAbove38 <- coef(chd_hoursAbove38_summary)["Anomaly_HoursAbove38_1322", "Estimate"] * avg_hoursAbove38
#se_chd$chd_hoursAbove38 <- coef(chd_hoursAbove38_summary)["Anomaly_HoursAbove38_1322", "Std. Error"]
#lower_bound_chd$chd_hoursAbove38 <- coefficients_chd$chd_hoursAbove38 - 1.96 * se_chd$chd_hoursAbove38
#upper_bound_chd$chd_hoursAbove38 <- coefficients_chd$chd_hoursAbove38 + 1.96 * se_chd$chd_hoursAbove38

# hoursAbove41 
chd_hoursAbove41 <- lmer(CHD_CrudePrev ~  Anomaly_HoursAbove41_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_hoursAbove41)
chd_hoursAbove41_summary <- summary(chd_hoursAbove41)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_hoursAbove41 <- coef(chd_hoursAbove41_summary)["Anomaly_HoursAbove41_1322", "Estimate"] * avg_hoursAbove41
se_chd$chd_hoursAbove41 <- coef(chd_hoursAbove41_summary)["Anomaly_HoursAbove41_1322", "Std. Error"]
lower_bound_chd$chd_hoursAbove41 <- coefficients_chd$chd_hoursAbove41 - 1.96 * se_chd$chd_hoursAbove41
upper_bound_chd$chd_hoursAbove41 <- coefficients_chd$chd_hoursAbove41 + 1.96 * se_chd$chd_hoursAbove41

# summer HI anomaly 
chd_summerHIAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_SummerMeanHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_summerHIAnomaly)
chd_summerHIAnomaly_summary <- summary(chd_summerHIAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_summerHIAnomaly <- coef(chd_summerHIAnomaly_summary)["Anomaly_SummerMeanHI_1322", "Estimate"] * avg_summerMeanHIAnomaly
se_chd$chd_summerHIAnomaly <- coef(chd_summerHIAnomaly_summary)["Anomaly_SummerMeanHI_1322", "Std. Error"]
lower_bound_chd$chd_summerHIAnomaly <- coefficients_chd$chd_summerHIAnomaly - 1.96 * se_chd$chd_summerHIAnomaly
upper_bound_chd$chd_summerHIAnomaly <- coefficients_chd$chd_summerHIAnomaly + 1.96 * se_chd$chd_summerHIAnomaly

# winter HI anomaly 
chd_winterHIAnomaly <- lmer(CHD_CrudePrev ~  Anomaly_WinterMeanHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_winterHIAnomaly)
chd_winterHIAnomaly_summary <- summary(chd_winterHIAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_winterHIAnomaly <- coef(chd_winterHIAnomaly_summary)["Anomaly_WinterMeanHI_1322", "Estimate"] * avg_winterMeanHIAnomaly
se_chd$chd_winterHIAnomaly <- coef(chd_winterHIAnomaly_summary)["Anomaly_WinterMeanHI_1322", "Std. Error"]
lower_bound_chd$chd_winterHIAnomaly <- coefficients_chd$chd_winterHIAnomaly - 1.96 * se_chd$chd_winterHIAnomaly
upper_bound_chd$chd_winterHIAnomaly <- coefficients_chd$chd_winterHIAnomaly + 1.96 * se_chd$chd_winterHIAnomaly

# night HI fall/spring 
chd_nightHIother <- lmer(CHD_CrudePrev ~  Anomaly_MeanNighttimeHI_Other_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                           (1 | CountyFIPS), data = subset_data)
summary(chd_nightHIother)
chd_nightHIother_summary <- summary(chd_nightHIother)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_nightHIother <- coef(chd_nightHIother_summary)["Anomaly_MeanNighttimeHI_Other_1322", "Estimate"] * avg_nightHIother
se_chd$chd_nightHIother <- coef(chd_nightHIother_summary)["Anomaly_MeanNighttimeHI_Other_1322", "Std. Error"]
lower_bound_chd$chd_nightHIother <- coefficients_chd$chd_nightHIother - 1.96 * se_chd$chd_nightHIother
upper_bound_chd$chd_nightHIother <- coefficients_chd$chd_nightHIother + 1.96 * se_chd$chd_nightHIother


# night HI summer 
chd_nightHIsummer <- lmer(CHD_CrudePrev ~  Anomaly_MeanNighttimeHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_nightHIsummer)
chd_nightHIsummer_summary <- summary(chd_nightHIsummer)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_nightHIsummer <- coef(chd_nightHIsummer_summary)["Anomaly_MeanNighttimeHI_Summer_1322", "Estimate"] * avg_nightHIsummer
se_chd$chd_nightHIsummer <- coef(chd_nightHIsummer_summary)["Anomaly_MeanNighttimeHI_Summer_1322", "Std. Error"]
lower_bound_chd$chd_nightHIsummer <- coefficients_chd$chd_nightHIsummer - 1.96 * se_chd$chd_nightHIsummer
upper_bound_chd$chd_nightHIsummer <- coefficients_chd$chd_nightHIsummer + 1.96 * se_chd$chd_nightHIsummer

# night HI winter 
chd_nightHIwinter <- lmer(CHD_CrudePrev ~  Anomaly_MeanNighttimeHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_nightHIwinter)
chd_nightHIwinter_summary <- summary(chd_nightHIwinter)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_nightHIwinter <- coef(chd_nightHIwinter_summary)["Anomaly_MeanNighttimeHI_Winter_1322", "Estimate"] * avg_nightHIwinter
se_chd$chd_nightHIwinter <- coef(chd_nightHIwinter_summary)["Anomaly_MeanNighttimeHI_Winter_1322", "Std. Error"]
lower_bound_chd$chd_nightHIwinter <- coefficients_chd$chd_nightHIwinter - 1.96 * se_chd$chd_nightHIwinter
upper_bound_chd$chd_nightHIwinter <- coefficients_chd$chd_nightHIwinter + 1.96 * se_chd$chd_nightHIwinter

# night HI annual 
chd_nightHIannual <- lmer(CHD_CrudePrev ~  Anomaly_MeanNighttimeHI_Annual_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(chd_nightHIannual)
chd_nightHIannual_summary <- summary(chd_nightHIannual)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_nightHIannual <- coef(chd_nightHIannual_summary)["Anomaly_MeanNighttimeHI_Annual_1322", "Estimate"] * avg_nightHIannual
se_chd$chd_nightHIannual <- coef(chd_nightHIannual_summary)["Anomaly_MeanNighttimeHI_Annual_1322", "Std. Error"]
lower_bound_chd$chd_nightHIannual <- coefficients_chd$chd_nightHIannual - 1.96 * se_chd$chd_nightHIannual
upper_bound_chd$chd_nightHIannual <- coefficients_chd$chd_nightHIannual + 1.96 * se_chd$chd_nightHIannual

# day HI fall/spring 
chd_dayHIother <- lmer(CHD_CrudePrev ~  Anomaly_MeanDaytimeHI_Other_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                         (1 | CountyFIPS), data = subset_data)
summary(chd_dayHIother)
chd_dayHIother_summary <- summary(chd_dayHIother)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_dayHIother <- coef(chd_dayHIother_summary)["Anomaly_MeanDaytimeHI_Other_1322", "Estimate"] * avg_dayMeanHIother
se_chd$chd_dayHIother <- coef(chd_dayHIother_summary)["Anomaly_MeanDaytimeHI_Other_1322", "Std. Error"]
lower_bound_chd$chd_dayHIother <- coefficients_chd$chd_dayHIother - 1.96 * se_chd$chd_dayHIother
upper_bound_chd$chd_dayHIother <- coefficients_chd$chd_dayHIother + 1.96 * se_chd$chd_dayHIother


# day HI summer 
chd_dayHIsummer <- lmer(CHD_CrudePrev ~  Anomaly_MeanDaytimeHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_dayHIsummer)
chd_dayHIsummer_summary <- summary(chd_dayHIsummer)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_dayHIsummer <- coef(chd_dayHIsummer_summary)["Anomaly_MeanDaytimeHI_Summer_1322", "Estimate"] * avg_dayMeanHIsummer
se_chd$chd_dayHIsummer <- coef(chd_dayHIsummer_summary)["Anomaly_MeanDaytimeHI_Summer_1322", "Std. Error"]
lower_bound_chd$chd_dayHIsummer <- coefficients_chd$chd_dayHIsummer - 1.96 * se_chd$chd_dayHIsummer
upper_bound_chd$chd_dayHIsummer <- coefficients_chd$chd_dayHIsummer + 1.96 * se_chd$chd_dayHIsummer

# day HI winter 
chd_dayHIwinter <- lmer(CHD_CrudePrev ~  Anomaly_MeanDaytimeHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_dayHIwinter)
chd_dayHIwinter_summary <- summary(chd_dayHIwinter)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_dayHIwinter <- coef(chd_dayHIwinter_summary)["Anomaly_MeanDaytimeHI_Winter_1322", "Estimate"] * avg_dayMeanHIwinter
se_chd$chd_dayHIwinter <- coef(chd_dayHIwinter_summary)["Anomaly_MeanDaytimeHI_Winter_1322", "Std. Error"]
lower_bound_chd$chd_dayHIwinter <- coefficients_chd$chd_dayHIwinter - 1.96 * se_chd$chd_dayHIwinter
upper_bound_chd$chd_dayHIwinter <- coefficients_chd$chd_dayHIwinter + 1.96 * se_chd$chd_dayHIwinter

# day HI annual 
chd_dayHIannual <- lmer(CHD_CrudePrev ~  Anomaly_MeanDaytimeHI_Annual_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(chd_dayHIannual)
chd_dayHIannual_summary <- summary(chd_dayHIannual)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_dayHIannual <- coef(chd_dayHIannual_summary)["Anomaly_MeanDaytimeHI_Annual_1322", "Estimate"] * avg_dayMeanHIannual
se_chd$chd_dayHIannual <- coef(chd_dayHIannual_summary)["Anomaly_MeanDaytimeHI_Annual_1322", "Std. Error"]
lower_bound_chd$chd_dayHIannual <- coefficients_chd$chd_dayHIannual - 1.96 * se_chd$chd_dayHIannual
upper_bound_chd$chd_dayHIannual <- coefficients_chd$chd_dayHIannual + 1.96 * se_chd$chd_dayHIannual


# median summer max HI
chd_medianSummerHImax <- lmer(CHD_CrudePrev ~  Anomaly_MedianSummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                              + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                (1 | CountyFIPS), data = subset_data)
summary(chd_medianSummerHImax)
chd_medianSummerHImax_summary <- summary(chd_medianSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_medianSummerHImax <- coef(chd_medianSummerHImax_summary)["Anomaly_MedianSummerMaxHI_1322", "Estimate"] * avg_medianSummerHImax
se_chd$chd_medianSummerHImax <- coef(chd_medianSummerHImax_summary)["Anomaly_MedianSummerMaxHI_1322", "Std. Error"]
lower_bound_chd$chd_medianSummerHImax <- coefficients_chd$chd_medianSummerHImax - 1.96 * se_chd$chd_medianSummerHImax
upper_bound_chd$chd_medianSummerHImax <- coefficients_chd$chd_medianSummerHImax + 1.96 * se_chd$chd_medianSummerHImax

# mean summer max HI
chd_meanSummerHImax <- lmer(CHD_CrudePrev ~  Anomaly_MeanSummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(chd_meanSummerHImax)
chd_meanSummerHImax_summary <- summary(chd_meanSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_meanSummerHImax <- coef(chd_meanSummerHImax_summary)["Anomaly_MeanSummerMaxHI_1322", "Estimate"] * avg_meanSummerHImax
se_chd$chd_meanSummerHImax <- coef(chd_meanSummerHImax_summary)["Anomaly_MeanSummerMaxHI_1322", "Std. Error"]
lower_bound_chd$chd_meanSummerHImax <- coefficients_chd$chd_meanSummerHImax - 1.96 * se_chd$chd_meanSummerHImax
upper_bound_chd$chd_meanSummerHImax <- coefficients_chd$chd_meanSummerHImax + 1.96 * se_chd$chd_meanSummerHImax

# 95th percentile summer max HI
chd_percentile95SummerHImax <- lmer(CHD_CrudePrev ~  Anomaly_Percentile95SummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                                    + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                      (1 | CountyFIPS), data = subset_data)
summary(chd_percentile95SummerHImax)
chd_percentile95SummerHImax_summary <- summary(chd_percentile95SummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_chd$chd_percentile95SummerHImax <- coef(chd_percentile95SummerHImax_summary)["Anomaly_Percentile95SummerMaxHI_1322", "Estimate"] * avg_percentile95SummerHImax
se_chd$chd_percentile95SummerHImax <- coef(chd_percentile95SummerHImax_summary)["Anomaly_Percentile95SummerMaxHI_1322", "Std. Error"]
lower_bound_chd$chd_percentile95SummerHImax <- coefficients_chd$chd_percentile95SummerHImax - 1.96 * se_chd$chd_percentile95SummerHImax
upper_bound_chd$chd_percentile95SummerHImax <- coefficients_chd$chd_percentile95SummerHImax + 1.96 * se_chd$chd_percentile95SummerHImax

####################################################### CHD Figure 3 forest plot ###########################################
#### Combine the lists into a data frame #####################
chd_fig3 <- data.frame(
  Variable = c("Mean Heat Index", "Days ≥ 32", #"Days ≥ 34", 
               "Days ≥ 36",
               #"Days ≥ 38", 
               "Days ≥ 41", "Hours ≥ 32", 
               #"Hours ≥ 34", 
               "Hours ≥ 36",
               #"Hours ≥ 38", 
               "Hours ≥ 41", "Summer Mean", "Winter Mean", "Night Mean Winter",  "Night Mean Annual", 
               "Night Mean Fall/Spring", "Night Mean Summer",  
               "Day Mean Fall/Spring", "Day Mean Summer", "Day Mean Winter", "Day Mean Annual", "Median Summer Max", "Mean Summer Max",
               "95th %ile Summer Max"),
  Effect_Anomaly_Size = unlist(coefficients_chd),
  SE = unlist(se_chd),
  Lower_CI = unlist(lower_bound_chd),
  Upper_CI = unlist(upper_bound_chd)
)
##########################################################################################################################
####################### ############################### Stroke Figure 3###############################################
# Create empty lists to store results from linear models
coefficients_stroke <- list()
se_stroke <- list()
pvalue_stroke <- list()
lower_bound_stroke <- list()
upper_bound_stroke <- list()

# average anomaly size 
avg_HI_70_79_change <- mean(subset_data$HI_C_Change70_79_13_22)

avg_daysAbove32 <- mean(subset_data$Anomaly_DaysAbove32_1322)
#avg_daysAbove34 <- mean(subset_data$Anomaly_DaysAbove34_1322)
avg_daysAbove36 <- mean(subset_data$Anomaly_DaysAbove36_1322)
#avg_daysAbove38 <- mean(subset_data$Anomaly_DaysAbove38_1322)
avg_daysAbove41 <- mean(subset_data$Anomaly_DaysAbove41_1322)


avg_hoursAbove32 <- mean(subset_data$Anomaly_HoursAbove32_1322)
#avg_hoursAbove34 <- mean(subset_data$Anomaly_HoursAbove34_1322)
avg_hoursAbove36 <- mean(subset_data$Anomaly_HoursAbove36_1322)
#avg_hoursAbove38 <- mean(subset_data$Anomaly_HoursAbove38_1322)
avg_hoursAbove41 <- mean(subset_data$Anomaly_HoursAbove41_1322)

avg_summerMeanHIAnomaly <- mean(subset_data$Anomaly_SummerMeanHI_1322)
avg_winterMeanHIAnomaly <- mean(subset_data$Anomaly_WinterMeanHI_1322)

avg_nightHIother <- mean(subset_data$Anomaly_MeanNighttimeHI_Other_1322)
avg_nightHIsummer <- mean(subset_data$Anomaly_MeanNighttimeHI_Summer_1322)
avg_nightHIwinter <- mean(subset_data$Anomaly_MeanNighttimeHI_Winter_1322)
avg_nightHIannual <- mean(subset_data$Anomaly_MeanNighttimeHI_Annual_1322)

avg_dayMeanHIother <- mean(subset_data$Anomaly_MeanDaytimeHI_Other_1322)
avg_dayMeanHIsummer <- mean(subset_data$Anomaly_MeanDaytimeHI_Summer_1322)
avg_dayMeanHIwinter <- mean(subset_data$Anomaly_MeanDaytimeHI_Winter_1322)
avg_dayMeanHIannual <- mean(subset_data$Anomaly_MeanDaytimeHI_Annual_1322)

avg_medianSummerHImax <- mean(subset_data$Anomaly_MedianSummerMaxHI_1322)
avg_percentile95SummerHImax <- mean(subset_data$Anomaly_Percentile95SummerMaxHI_1322)
avg_meanSummerHImax <- mean(subset_data$Anomaly_MeanSummerMaxHI_1322)

#### seasonal  ########
# HI change 70-80
stroke_hi_70_79 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                          (1 | CountyFIPS), data = subset_data)
summary(stroke_hi_70_79)
stroke_hi_70_79_summary <- summary(stroke_hi_70_79)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Estimate"] * avg_HI_70_79_change
se_stroke$stroke_hi_70_79 <- coef(stroke_hi_70_79_summary)["HI_C_Change70_79_13_22", "Std. Error"]
lower_bound_stroke$stroke_hi_70_79 <- coefficients_stroke$stroke_hi_70_79 - 1.96 * se_stroke$stroke_hi_70_79
upper_bound_stroke$stroke_hi_70_79 <- coefficients_stroke$stroke_hi_70_79 + 1.96 * se_stroke$stroke_hi_70_79

# daysAbove32 
stroke_daysAbove32 <- lmer(STROKE_CrudePrev ~  Anomaly_DaysAbove32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_daysAbove32)
stroke_daysAbove32_summary <- summary(stroke_daysAbove32)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_daysAbove32 <- coef(stroke_daysAbove32_summary)["Anomaly_DaysAbove32_1322", "Estimate"] * avg_daysAbove32
se_stroke$stroke_daysAbove32 <- coef(stroke_daysAbove32_summary)["Anomaly_DaysAbove32_1322", "Std. Error"]
lower_bound_stroke$stroke_daysAbove32 <- coefficients_stroke$stroke_daysAbove32 - 1.96 * se_stroke$stroke_daysAbove32
upper_bound_stroke$stroke_daysAbove32 <- coefficients_stroke$stroke_daysAbove32 + 1.96 * se_stroke$stroke_daysAbove32

# daysAbove34 
#stroke_daysAbove34 <- lmer(STROKE_CrudePrev ~  Anomaly_DaysAbove34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(stroke_daysAbove34)
#stroke_daysAbove34_summary <- summary(stroke_daysAbove34)

# Extracting coefficients, standard errors, and p-values
#coefficients_stroke$stroke_daysAbove34 <- coef(stroke_daysAbove34_summary)["Anomaly_DaysAbove34_1322", "Estimate"] * avg_daysAbove34
#se_stroke$stroke_daysAbove34 <- coef(stroke_daysAbove34_summary)["Anomaly_DaysAbove34_1322", "Std. Error"]
#lower_bound_stroke$stroke_daysAbove34 <- coefficients_stroke$stroke_daysAbove34 - 1.96 * se_stroke$stroke_daysAbove34
#upper_bound_stroke$stroke_daysAbove34 <- coefficients_stroke$stroke_daysAbove34 + 1.96 * se_stroke$stroke_daysAbove34

# daysAbove36 
stroke_daysAbove36 <- lmer(STROKE_CrudePrev ~  Anomaly_DaysAbove36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_daysAbove36)
stroke_daysAbove36_summary <- summary(stroke_daysAbove36)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_daysAbove36 <- coef(stroke_daysAbove36_summary)["Anomaly_DaysAbove36_1322", "Estimate"] * avg_daysAbove36
se_stroke$stroke_daysAbove36 <- coef(stroke_daysAbove36_summary)["Anomaly_DaysAbove36_1322", "Std. Error"]
lower_bound_stroke$stroke_daysAbove36 <- coefficients_stroke$stroke_daysAbove36 - 1.96 * se_stroke$stroke_daysAbove36
upper_bound_stroke$stroke_daysAbove36 <- coefficients_stroke$stroke_daysAbove36 + 1.96 * se_stroke$stroke_daysAbove36

# daysAbove38 
#stroke_daysAbove38 <- lmer(STROKE_CrudePrev ~  Anomaly_DaysAbove38_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                        + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                          (1 | CountyFIPS), data = subset_data)
#summary(stroke_daysAbove38)
#stroke_daysAbove38_summary <- summary(stroke_daysAbove38)

# Extracting coefficients, standard errors, and p-values
#coefficients_stroke$stroke_daysAbove38 <- coef(stroke_daysAbove38_summary)["Anomaly_DaysAbove38_1322", "Estimate"] * avg_daysAbove38
#se_stroke$stroke_daysAbove38 <- coef(stroke_daysAbove38_summary)["Anomaly_DaysAbove38_1322", "Std. Error"]
#lower_bound_stroke$stroke_daysAbove38 <- coefficients_stroke$stroke_daysAbove38 - 1.96 * se_stroke$stroke_daysAbove38
#upper_bound_stroke$stroke_daysAbove38 <- coefficients_stroke$stroke_daysAbove38 + 1.96 * se_stroke$stroke_daysAbove38

# daysAbove41 
stroke_daysAbove41 <- lmer(STROKE_CrudePrev ~  Anomaly_DaysAbove41_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_daysAbove41)
stroke_daysAbove41_summary <- summary(stroke_daysAbove41)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_daysAbove41 <- coef(stroke_daysAbove41_summary)["Anomaly_DaysAbove41_1322", "Estimate"] *avg_daysAbove41
se_stroke$stroke_daysAbove41 <- coef(stroke_daysAbove41_summary)["Anomaly_DaysAbove41_1322", "Std. Error"]
lower_bound_stroke$stroke_daysAbove41 <- coefficients_stroke$stroke_daysAbove41 - 1.96 * se_stroke$stroke_daysAbove41
upper_bound_stroke$stroke_daysAbove41 <- coefficients_stroke$stroke_daysAbove41 + 1.96 * se_stroke$stroke_daysAbove41

# hoursAbove32 
stroke_hoursAbove32 <- lmer(STROKE_CrudePrev ~  Anomaly_HoursAbove32_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_hoursAbove32)
stroke_hoursAbove32_summary <- summary(stroke_hoursAbove32)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_hoursAbove32 <- coef(stroke_hoursAbove32_summary)["Anomaly_HoursAbove32_1322", "Estimate"] * avg_hoursAbove32
se_stroke$stroke_hoursAbove32 <- coef(stroke_hoursAbove32_summary)["Anomaly_HoursAbove32_1322", "Std. Error"]
lower_bound_stroke$stroke_hoursAbove32 <- coefficients_stroke$stroke_hoursAbove32 - 1.96 * se_stroke$stroke_hoursAbove32
upper_bound_stroke$stroke_hoursAbove32 <- coefficients_stroke$stroke_hoursAbove32 + 1.96 * se_stroke$stroke_hoursAbove32

# hoursAbove34 
#stroke_hoursAbove34 <- lmer(STROKE_CrudePrev ~  Anomaly_HoursAbove34_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                           (1 | CountyFIPS), data = subset_data)
#summary(stroke_hoursAbove34)
#stroke_hoursAbove34_summary <- summary(stroke_hoursAbove34)

# Extracting coefficients, standard errors, and p-values
#coefficients_stroke$stroke_hoursAbove34 <- coef(stroke_hoursAbove34_summary)["Anomaly_HoursAbove34_1322", "Estimate"] * avg_hoursAbove34
#se_stroke$stroke_hoursAbove34 <- coef(stroke_hoursAbove34_summary)["Anomaly_HoursAbove34_1322", "Std. Error"]
#lower_bound_stroke$stroke_hoursAbove34 <- coefficients_stroke$stroke_hoursAbove34 - 1.96 * se_stroke$stroke_hoursAbove34
#upper_bound_stroke$stroke_hoursAbove34 <- coefficients_stroke$stroke_hoursAbove34 + 1.96 * se_stroke$stroke_hoursAbove34

# hoursAbove36 
stroke_hoursAbove36 <- lmer(STROKE_CrudePrev ~  Anomaly_HoursAbove36_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_hoursAbove36)
stroke_hoursAbove36_summary <- summary(stroke_hoursAbove36)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_hoursAbove36 <- coef(stroke_hoursAbove36_summary)["Anomaly_HoursAbove36_1322", "Estimate"] * avg_hoursAbove36
se_stroke$stroke_hoursAbove36 <- coef(stroke_hoursAbove36_summary)["Anomaly_HoursAbove36_1322", "Std. Error"]
lower_bound_stroke$stroke_hoursAbove36 <- coefficients_stroke$stroke_hoursAbove36 - 1.96 * se_stroke$stroke_hoursAbove36
upper_bound_stroke$stroke_hoursAbove36 <- coefficients_stroke$stroke_hoursAbove36 + 1.96 * se_stroke$stroke_hoursAbove36

# hoursAbove38 
#stroke_hoursAbove38 <- lmer(STROKE_CrudePrev ~  Anomaly_HoursAbove38_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
#                         + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
#                           (1 | CountyFIPS), data = subset_data)
#summary(stroke_hoursAbove38)
#stroke_hoursAbove38_summary <- summary(stroke_hoursAbove38)

# Extracting coefficients, standard errors, and p-values
#coefficients_stroke$stroke_hoursAbove38 <- coef(stroke_hoursAbove38_summary)["Anomaly_HoursAbove38_1322", "Estimate"] * avg_hoursAbove38
#se_stroke$stroke_hoursAbove38 <- coef(stroke_hoursAbove38_summary)["Anomaly_HoursAbove38_1322", "Std. Error"]
#lower_bound_stroke$stroke_hoursAbove38 <- coefficients_stroke$stroke_hoursAbove38 - 1.96 * se_stroke$stroke_hoursAbove38
#upper_bound_stroke$stroke_hoursAbove38 <- coefficients_stroke$stroke_hoursAbove38 + 1.96 * se_stroke$stroke_hoursAbove38

# hoursAbove41 
stroke_hoursAbove41 <- lmer(STROKE_CrudePrev ~  Anomaly_HoursAbove41_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_hoursAbove41)
stroke_hoursAbove41_summary <- summary(stroke_hoursAbove41)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_hoursAbove41 <- coef(stroke_hoursAbove41_summary)["Anomaly_HoursAbove41_1322", "Estimate"] * avg_hoursAbove41
se_stroke$stroke_hoursAbove41 <- coef(stroke_hoursAbove41_summary)["Anomaly_HoursAbove41_1322", "Std. Error"]
lower_bound_stroke$stroke_hoursAbove41 <- coefficients_stroke$stroke_hoursAbove41 - 1.96 * se_stroke$stroke_hoursAbove41
upper_bound_stroke$stroke_hoursAbove41 <- coefficients_stroke$stroke_hoursAbove41 + 1.96 * se_stroke$stroke_hoursAbove41

# summer HI anomaly 
stroke_summerHIAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_SummerMeanHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_summerHIAnomaly)
stroke_summerHIAnomaly_summary <- summary(stroke_summerHIAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_summerHIAnomaly <- coef(stroke_summerHIAnomaly_summary)["Anomaly_SummerMeanHI_1322", "Estimate"] * avg_summerMeanHIAnomaly
se_stroke$stroke_summerHIAnomaly <- coef(stroke_summerHIAnomaly_summary)["Anomaly_SummerMeanHI_1322", "Std. Error"]
lower_bound_stroke$stroke_summerHIAnomaly <- coefficients_stroke$stroke_summerHIAnomaly - 1.96 * se_stroke$stroke_summerHIAnomaly
upper_bound_stroke$stroke_summerHIAnomaly <- coefficients_stroke$stroke_summerHIAnomaly + 1.96 * se_stroke$stroke_summerHIAnomaly

# winter HI anomaly 
stroke_winterHIAnomaly <- lmer(STROKE_CrudePrev ~  Anomaly_WinterMeanHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_winterHIAnomaly)
stroke_winterHIAnomaly_summary <- summary(stroke_winterHIAnomaly)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_winterHIAnomaly <- coef(stroke_winterHIAnomaly_summary)["Anomaly_WinterMeanHI_1322", "Estimate"] * avg_winterMeanHIAnomaly
se_stroke$stroke_winterHIAnomaly <- coef(stroke_winterHIAnomaly_summary)["Anomaly_WinterMeanHI_1322", "Std. Error"]
lower_bound_stroke$stroke_winterHIAnomaly <- coefficients_stroke$stroke_winterHIAnomaly - 1.96 * se_stroke$stroke_winterHIAnomaly
upper_bound_stroke$stroke_winterHIAnomaly <- coefficients_stroke$stroke_winterHIAnomaly + 1.96 * se_stroke$stroke_winterHIAnomaly


# night HI other 
stroke_nightHIother <- lmer(STROKE_CrudePrev ~  Anomaly_MeanNighttimeHI_Other_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                            + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                              (1 | CountyFIPS), data = subset_data)
summary(stroke_nightHIother)
stroke_nightHIother_summary <- summary(stroke_nightHIother)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_nightHIother <- coef(stroke_nightHIother_summary)["Anomaly_MeanNighttimeHI_Other_1322", "Estimate"] * avg_nightHIother
se_stroke$stroke_nightHIother <- coef(stroke_nightHIother_summary)["Anomaly_MeanNighttimeHI_Other_1322", "Std. Error"]
lower_bound_stroke$stroke_nightHIother <- coefficients_stroke$stroke_nightHIother - 1.96 * se_stroke$stroke_nightHIother
upper_bound_stroke$stroke_nightHIother <- coefficients_stroke$stroke_nightHIother + 1.96 * se_stroke$stroke_nightHIother

# night HI summer 
stroke_nightHIsummer <- lmer(STROKE_CrudePrev ~  Anomaly_MeanNighttimeHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_nightHIsummer)
stroke_nightHIsummer_summary <- summary(stroke_nightHIsummer)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_nightHIsummer <- coef(stroke_nightHIsummer_summary)["Anomaly_MeanNighttimeHI_Summer_1322", "Estimate"] * avg_nightHIsummer
se_stroke$stroke_nightHIsummer <- coef(stroke_nightHIsummer_summary)["Anomaly_MeanNighttimeHI_Summer_1322", "Std. Error"]
lower_bound_stroke$stroke_nightHIsummer <- coefficients_stroke$stroke_nightHIsummer - 1.96 * se_stroke$stroke_nightHIsummer
upper_bound_stroke$stroke_nightHIsummer <- coefficients_stroke$stroke_nightHIsummer + 1.96 * se_stroke$stroke_nightHIsummer

# night HI winter 
stroke_nightHIwinter <- lmer(STROKE_CrudePrev ~  Anomaly_MeanNighttimeHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_nightHIwinter)
stroke_nightHIwinter_summary <- summary(stroke_nightHIwinter)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_nightHIwinter <- coef(stroke_nightHIwinter_summary)["Anomaly_MeanNighttimeHI_Winter_1322", "Estimate"] * avg_nightHIwinter
se_stroke$stroke_nightHIwinter <- coef(stroke_nightHIwinter_summary)["Anomaly_MeanNighttimeHI_Winter_1322", "Std. Error"]
lower_bound_stroke$stroke_nightHIwinter <- coefficients_stroke$stroke_nightHIwinter - 1.96 * se_stroke$stroke_nightHIwinter
upper_bound_stroke$stroke_nightHIwinter <- coefficients_stroke$stroke_nightHIwinter + 1.96 * se_stroke$stroke_nightHIwinter

# night HI annual 
stroke_nightHIannual <- lmer(STROKE_CrudePrev ~  Anomaly_MeanNighttimeHI_Annual_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                             + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                               (1 | CountyFIPS), data = subset_data)
summary(stroke_nightHIannual)
stroke_nightHIannual_summary <- summary(stroke_nightHIannual)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_nightHIannual <- coef(stroke_nightHIannual_summary)["Anomaly_MeanNighttimeHI_Annual_1322", "Estimate"] * avg_nightHIannual
se_stroke$stroke_nightHIannual <- coef(stroke_nightHIannual_summary)["Anomaly_MeanNighttimeHI_Annual_1322", "Std. Error"]
lower_bound_stroke$stroke_nightHIannual <- coefficients_stroke$stroke_nightHIannual - 1.96 * se_stroke$stroke_nightHIannual
upper_bound_stroke$stroke_nightHIannual <- coefficients_stroke$stroke_nightHIannual + 1.96 * se_stroke$stroke_nightHIannual

# day HI other 
stroke_dayHIother <- lmer(STROKE_CrudePrev ~  Anomaly_MeanDaytimeHI_Other_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                          + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                            (1 | CountyFIPS), data = subset_data)
summary(stroke_dayHIother)
stroke_dayHIother_summary <- summary(stroke_dayHIother)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_dayHIother <- coef(stroke_dayHIother_summary)["Anomaly_MeanDaytimeHI_Other_1322", "Estimate"] * avg_dayMeanHIother
se_stroke$stroke_dayHIother <- coef(stroke_dayHIother_summary)["Anomaly_MeanDaytimeHI_Other_1322", "Std. Error"]
lower_bound_stroke$stroke_dayHIother <- coefficients_stroke$stroke_dayHIother - 1.96 * se_stroke$stroke_dayHIother
upper_bound_stroke$stroke_dayHIother <- coefficients_stroke$stroke_dayHIother + 1.96 * se_stroke$stroke_dayHIother

# day HI summer 
stroke_dayHIsummer <- lmer(STROKE_CrudePrev ~  Anomaly_MeanDaytimeHI_Summer_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_dayHIsummer)
stroke_dayHIsummer_summary <- summary(stroke_dayHIsummer)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_dayHIsummer <- coef(stroke_dayHIsummer_summary)["Anomaly_MeanDaytimeHI_Summer_1322", "Estimate"] * avg_dayMeanHIsummer
se_stroke$stroke_dayHIsummer <- coef(stroke_dayHIsummer_summary)["Anomaly_MeanDaytimeHI_Summer_1322", "Std. Error"]
lower_bound_stroke$stroke_dayHIsummer <- coefficients_stroke$stroke_dayHIsummer - 1.96 * se_stroke$stroke_dayHIsummer
upper_bound_stroke$stroke_dayHIsummer <- coefficients_stroke$stroke_dayHIsummer + 1.96 * se_stroke$stroke_dayHIsummer

# day HI winter 
stroke_dayHIwinter <- lmer(STROKE_CrudePrev ~  Anomaly_MeanDaytimeHI_Winter_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_dayHIwinter)
stroke_dayHIwinter_summary <- summary(stroke_dayHIwinter)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_dayHIwinter <- coef(stroke_dayHIwinter_summary)["Anomaly_MeanDaytimeHI_Winter_1322", "Estimate"] * avg_dayMeanHIwinter
se_stroke$stroke_dayHIwinter <- coef(stroke_dayHIwinter_summary)["Anomaly_MeanDaytimeHI_Winter_1322", "Std. Error"]
lower_bound_stroke$stroke_dayHIwinter <- coefficients_stroke$stroke_dayHIwinter - 1.96 * se_stroke$stroke_dayHIwinter
upper_bound_stroke$stroke_dayHIwinter <- coefficients_stroke$stroke_dayHIwinter + 1.96 * se_stroke$stroke_dayHIwinter

# day HI annual 
stroke_dayHIannual <- lmer(STROKE_CrudePrev ~  Anomaly_MeanDaytimeHI_Annual_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                           + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                             (1 | CountyFIPS), data = subset_data)
summary(stroke_dayHIannual)
stroke_dayHIannual_summary <- summary(stroke_dayHIannual)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_dayHIannual <- coef(stroke_dayHIannual_summary)["Anomaly_MeanDaytimeHI_Annual_1322", "Estimate"] * avg_dayMeanHIannual
se_stroke$stroke_dayHIannual <- coef(stroke_dayHIannual_summary)["Anomaly_MeanDaytimeHI_Annual_1322", "Std. Error"]
lower_bound_stroke$stroke_dayHIannual <- coefficients_stroke$stroke_dayHIannual - 1.96 * se_stroke$stroke_dayHIannual
upper_bound_stroke$stroke_dayHIannual <- coefficients_stroke$stroke_dayHIannual + 1.96 * se_stroke$stroke_dayHIannual

# median summer max HI
stroke_medianSummerHImax <- lmer(STROKE_CrudePrev ~  Anomaly_MedianSummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                                 + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                   (1 | CountyFIPS), data = subset_data)
summary(stroke_medianSummerHImax)
stroke_medianSummerHImax_summary <- summary(stroke_medianSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_medianSummerHImax <- coef(stroke_medianSummerHImax_summary)["Anomaly_MedianSummerMaxHI_1322", "Estimate"] * avg_medianSummerHImax
se_stroke$stroke_medianSummerHImax <- coef(stroke_medianSummerHImax_summary)["Anomaly_MedianSummerMaxHI_1322", "Std. Error"]
lower_bound_stroke$stroke_medianSummerHImax <- coefficients_stroke$stroke_medianSummerHImax - 1.96 * se_stroke$stroke_medianSummerHImax
upper_bound_stroke$stroke_medianSummerHImax <- coefficients_stroke$stroke_medianSummerHImax + 1.96 * se_stroke$stroke_medianSummerHImax

# mean summer max HI
stroke_meanSummerHImax <- lmer(STROKE_CrudePrev ~  Anomaly_MeanSummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                               + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                 (1 | CountyFIPS), data = subset_data)
summary(stroke_meanSummerHImax)
stroke_meanSummerHImax_summary <- summary(stroke_meanSummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_meanSummerHImax <- coef(stroke_meanSummerHImax_summary)["Anomaly_MeanSummerMaxHI_1322", "Estimate"] * avg_meanSummerHImax
se_stroke$stroke_meanSummerHImax <- coef(stroke_meanSummerHImax_summary)["Anomaly_MeanSummerMaxHI_1322", "Std. Error"]
lower_bound_stroke$stroke_meanSummerHImax <- coefficients_stroke$stroke_meanSummerHImax - 1.96 * se_stroke$stroke_meanSummerHImax
upper_bound_stroke$stroke_meanSummerHImax <- coefficients_stroke$stroke_meanSummerHImax + 1.96 * se_stroke$stroke_meanSummerHImax

# 95th percentile summer max HI
stroke_percentile95SummerHImax <- lmer(STROKE_CrudePrev ~  Anomaly_Percentile95SummerMaxHI_1322 + OBESITY_CrudePrev_P_div10 + EP_AGE65 +  PCT_ImperviousSurfaces 
                                       + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN +
                                         (1 | CountyFIPS), data = subset_data)
summary(stroke_percentile95SummerHImax)
stroke_percentile95SummerHImax_summary <- summary(stroke_percentile95SummerHImax)

# Extracting coefficients, standard errors, and p-values
coefficients_stroke$stroke_percentile95SummerHImax <- coef(stroke_percentile95SummerHImax_summary)["Anomaly_Percentile95SummerMaxHI_1322", "Estimate"] * avg_percentile95SummerHImax
se_stroke$stroke_percentile95SummerHImax <- coef(stroke_percentile95SummerHImax_summary)["Anomaly_Percentile95SummerMaxHI_1322", "Std. Error"]
lower_bound_stroke$stroke_percentile95SummerHImax <- coefficients_stroke$stroke_percentile95SummerHImax - 1.96 * se_stroke$stroke_percentile95SummerHImax
upper_bound_stroke$stroke_percentile95SummerHImax <- coefficients_stroke$stroke_percentile95SummerHImax + 1.96 * se_stroke$stroke_percentile95SummerHImax


####################################################### Stroke Figure 3 forest plot #########################################
#### Combine the lists into a data frame ##################
stroke_fig3 <- data.frame(
  Variable = c("Mean Heat Index", "Days ≥ 32", #"Days ≥ 34", 
               "Days ≥ 36",
               #"Days ≥ 38", 
               "Days ≥ 41", "Hours ≥ 32", 
               #"Hours ≥ 34", 
               "Hours ≥ 36",
               #"Hours ≥ 38", 
               "Hours ≥ 41", "Summer Mean", "Winter Mean", "Night Mean Fall/Spring", "Night Mean Summer", "Night Mean Winter", "Night Mean Annual", 
               "Day Mean Fall/Spring", "Day Mean Summer", "Day Mean Winter", "Day Mean Annual", "Median Summer Max", "Mean Summer Max",
               "95th %ile Summer Max"),
  Effect_Anomaly_Size = unlist(coefficients_stroke),
  SE = unlist(se_stroke),
  Lower_CI = unlist(lower_bound_stroke),
  Upper_CI = unlist(upper_bound_stroke)
)

#### Combined revised forest plot ######################
# Combine the datasets for CHD and Stroke, retaining both outcomes for each variable
combined_fig3 <- rbind(
  cbind(chd_fig3, Outcome = "CHD"),
  cbind(stroke_fig3, Outcome = "Stroke")
)
# Extract variable names and create groups based on their prefixes
combined_fig3$Variable_Group <- sub("_.*", "", combined_fig3$Variable)

combined_fig3$Combined_CI <- sprintf("(%.2f, %.2f)", 
                                     combined_fig3$Lower_CI,
                                     combined_fig3$Upper_CI)

# To check the new grouping
table(combined_fig3$Variable_Group)

# Define the custom order for Variable_Group
custom_order <- c("Mean Heat Index", "Night Mean Annual", "Day Mean Annual",
                  "Night Mean Summer", "Day Mean Summer", "Summer Mean", 
                  "Median Summer Max", "Mean Summer Max",
                  "Night Mean Winter", "Day Mean Winter", "Winter Mean",
                  "Night Mean Fall/Spring", "Day Mean Fall/Spring", "Hours ≥ 41", #"Hours ≥ 38", 
                  "Hours ≥ 36", #"Hours ≥ 34", 
                  "Hours ≥ 32", 
                  "Days ≥ 41", #"Days ≥ 38", 
                  "Days ≥ 36", #"Days ≥ 34", 
                  "Days ≥ 32", "95th %ile Summer Max")                


# Reverse the order of the custom_order
reversed_order <- custom_order

# Convert Variable_Group to a factor with the reversed custom order
combined_fig3$Variable_Group <- factor(combined_fig3$Variable_Group, levels = reversed_order)

# Convert Outcome to factor with Stroke first
combined_fig3$Outcome <- factor(combined_fig3$Outcome, levels = c("Stroke", "CHD"))

# Sort the data by Variable_Group (reverse order) and then by Outcome (Stroke first)
combined_fig3 <- combined_fig3[order(combined_fig3$Variable_Group, combined_fig3$Outcome), ]

# Create a combined variable that includes both the Variable and Outcome for plotting
combined_fig3 <- combined_fig3 %>%
  mutate(Variable_Outcome = paste(Variable, Outcome, sep = " - "))

# Ensure that Variable_Outcome follows the same order as in the sorted combined_fig3
combined_fig3$Variable_Outcome <- factor(combined_fig3$Variable_Outcome, levels = unique(combined_fig3$Variable_Outcome))

# write data to csv for figure 
write.csv(combined_fig3, "Figure3_Data_09_16_2024.csv")

fig3 <- ggplot(combined_fig3, aes(x = as.numeric(Effect_Anomaly_Size), y = Variable_Outcome, color = Outcome, shape = Outcome)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +  # Add black vertical line at x = 0
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = -0.5
  geom_vline(xintercept = -1, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = -1
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 0.5
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 1
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 1.5
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray") +  # Add dashed vertical line at x = 2
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = as.numeric(Lower_CI), xmax = as.numeric(Upper_CI)), height = 0.7) +
  scale_x_continuous(limits = c(-1.5, 2.5)) +  # Set x-axis limits from -1.5 to 2.5
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

plot(fig3)


# Save the plot with a specific resolution
ggsave("Figure3.png", plot = fig3, width = 5.4, height = 6.75, dpi = 600)

##########################################################################################################################
#########################################################################################################################
################################################ CHD Table 1 ############################################################
#########################################################################################################################
#### check for colinearity ###############
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + Change_Temp_C_70_79_13_22 + Change_RH_70_79_13_22 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + latent_diff70_79_13_22_P_Mill + solar_diff70_79_13_22_P_Mill
                       + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + downwards_solar_diff70_79_13_22_P_Mill + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)



# check for colinearity # removed latent
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + Change_Temp_C_70_79_13_22 + Change_RH_70_79_13_22 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + thermal_diff70_79_13_22_P_Mill + sensible_diff70_79_13_22_P_Mill + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + downwards_solar_diff70_79_13_22_P_Mill + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)

# check for colinearity # removed sensible
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + Change_Temp_C_70_79_13_22 + Change_RH_70_79_13_22 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + thermal_diff70_79_13_22_P_Mill + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + downwards_solar_diff70_79_13_22_P_Mill + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)

# check for colinearity # removed temp
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + Change_RH_70_79_13_22 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + thermal_diff70_79_13_22_P_Mill + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + downwards_solar_diff70_79_13_22_P_Mill + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)

# check for colinearity # removed thermal
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + Change_RH_70_79_13_22 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + evap_diff70_79_13_22_P_x10 + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + downwards_solar_diff70_79_13_22_P_Mill + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)

# removed evaporation, relative humidity, and downwards solar radiation
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
vif(CHD70_79_13_22)
CHD70_79_13_22_summary <- summary(CHD70_79_13_22)
CHD70_79_13_22_r2 <- r.squaredGLMM(CHD70_79_13_22)
print(CHD70_79_13_22_r2)
#### Create empty lists for each linear model #############
# 70-79
coefficients_CHD70_79_13_22 <- list()
se_CHD70_79_13_22 <- list()
tvalue_CHD70_79_13_22 <- list()
lower_bound_CHD70_79_13_22 <- list()
upper_bound_CHD70_79_13_22 <- list()


# average CHD
avg_CHD <- mean(subset_data$CHD_CrudePrev, na.rm = TRUE)

# model 
CHD70_79_13_22 <- lmer(CHD_CrudePrev ~  HI_C_Change70_79_13_22 + 
                         Temp_C_2013_2022 + PCT_ImperviousSurfaces +
                         SPL_THEME1 +
                         EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                         OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                       + pressure_diff70_79_13_22_P_div10 
                       + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + LCchangeMEAN
                       + (1 | CountyFIPS), data = subset_data)
summary(CHD70_79_13_22)
CHD70_79_13_22_summary <- summary(CHD70_79_13_22)

# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_CHD70_79_13_22 <- summary(CHD70_79_13_22)$coefficients[, "Estimate"]
se_CHD70_79_13_22 <- summary(CHD70_79_13_22)$coefficients[, "Std. Error"]
tvalue_CHD70_79_13_22 <- summary(CHD70_79_13_22)$coefficients[, "t value"]
lower_bound_CHD70_79_13_22  <-  coefficients_CHD70_79_13_22 - 1.96 * se_CHD70_79_13_22 
upper_bound_CHD70_79_13_22  <-  coefficients_CHD70_79_13_22 + 1.96 * se_CHD70_79_13_22 

#average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)
avg_windU_diff70_79_13_22 <- mean(subset_data$windU_diff70_79_13_22, na.rm = TRUE)
avg_windV_diff70_79_13_22 <- mean(subset_data$windV_diff70_79_13_22, na.rm = TRUE)
avg_solar_diff70_79_13_22 <- mean(subset_data$solar_diff70_79_13_22_P_Mill, na.rm = TRUE)
avg_pressure_diff70_79_13_22 <- mean(subset_data$pressure_diff70_79_13_22_P_div10, na.rm = TRUE)
avg_precip_diff70_79_13_22 <- mean(subset_data$precip_diff70_79_13_22_P_x10, na.rm = TRUE)
avg_transpir_diff70_79_13_22 <- mean(subset_data$transpir_diff70_79_13_22_P_x1000, na.rm = TRUE)

# get effect size of anomaly 
rate_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD70_79_13_22["HI_C_Change70_79_13_22"]
rate_windU_diff70_79_13_22 <- avg_windU_diff70_79_13_22 * coefficients_CHD70_79_13_22["windU_diff70_79_13_22"] 
rate_windV_diff70_79_13_22 <- avg_windV_diff70_79_13_22 * coefficients_CHD70_79_13_22["windV_diff70_79_13_22"] 
rate_solar_diff70_79_13_22 <- avg_solar_diff70_79_13_22 * coefficients_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
rate_pressure_diff70_79_13_22 <- avg_pressure_diff70_79_13_22 * coefficients_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
rate_precip_diff70_79_13_22 <- avg_precip_diff70_79_13_22 * coefficients_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] 
rate_transpir_diff70_79_13_22 <- avg_transpir_diff70_79_13_22 * coefficients_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

# create confidence interval
lower_CI_HI_C_Change70_79_13_22 <- rate_HI_C_Change70_79_13_22 - 1.96 * se_CHD70_79_13_22["HI_C_Change70_79_13_22"] 
lower_CI_windU_diff70_79_13_22 <- rate_windU_diff70_79_13_22 - 1.96 * se_CHD70_79_13_22["windU_diff70_79_13_22"]
lower_CI_windV_diff70_79_13_22 <- rate_windV_diff70_79_13_22 - 1.96 * se_CHD70_79_13_22["windV_diff70_79_13_22"] 
lower_CI_solar_diff70_79_13_22 <- rate_solar_diff70_79_13_22  - 1.96 * se_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
lower_CI_pressure_diff70_79_13_22 <- rate_pressure_diff70_79_13_22  - 1.96 * se_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
lower_CI_precip_diff70_79_13_22 <- rate_precip_diff70_79_13_22  - 1.96 * se_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] 
lower_CI_transpir_diff70_79_13_22 <- rate_transpir_diff70_79_13_22  - 1.96 * se_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

upper_CI_HI_C_Change70_79_13_22 <- rate_HI_C_Change70_79_13_22 + 1.96 * se_CHD70_79_13_22["HI_C_Change70_79_13_22"] 
upper_CI_windU_diff70_79_13_22 <- rate_windU_diff70_79_13_22 + 1.96 * se_CHD70_79_13_22["windU_diff70_79_13_22"] 
upper_CI_windV_diff70_79_13_22 <- rate_windV_diff70_79_13_22  + 1.96 * se_CHD70_79_13_22["windV_diff70_79_13_22"] 
upper_CI_solar_diff70_79_13_22 <- rate_solar_diff70_79_13_22 + 1.96 * se_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
upper_CI_pressure_diff70_79_13_22 <- rate_pressure_diff70_79_13_22  + 1.96 * se_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
upper_CI_precip_diff70_79_13_22 <- rate_precip_diff70_79_13_22  + 1.96 * se_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] 
upper_CI_transpir_diff70_79_13_22 <- rate_transpir_diff70_79_13_22  + 1.96 * se_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_CHD70_79_13_22["HI_C_Change70_79_13_22"] / avg_CHD *100
pct_windU_diff70_79_13_22 <- avg_windU_diff70_79_13_22 * coefficients_CHD70_79_13_22["windU_diff70_79_13_22"] / avg_CHD *100
pct_windV_diff70_79_13_22 <- avg_windV_diff70_79_13_22 * coefficients_CHD70_79_13_22["windV_diff70_79_13_22"] / avg_CHD *100
pct_solar_diff70_79_13_22 <- avg_solar_diff70_79_13_22 * coefficients_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] / avg_CHD *100
pct_pressure_diff70_79_13_22 <- avg_pressure_diff70_79_13_22 * coefficients_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] / avg_CHD *100
pct_precip_diff70_79_13_22 <- avg_precip_diff70_79_13_22 * coefficients_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] / avg_CHD *100
pct_transpir_diff70_79_13_22 <- avg_transpir_diff70_79_13_22 * coefficients_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] / avg_CHD *100


# adjust for population #
# turn subset into a data frame to add columns
new_data <- subset_data

### calculate total exposure in each tract
new_data$exposure_hi <- new_data$HI_C_Change70_79_13_22 * new_data$Pop18Over
new_data$exposure_windu <- new_data$windU_diff70_79_13_22 * new_data$Pop18Over
new_data$exposure_windv <- new_data$windV_diff70_79_13_22 * new_data$Pop18Over
new_data$exposure_solar <- new_data$solar_diff70_79_13_22_P_Mill * new_data$Pop18Over
new_data$exposure_press <- new_data$pressure_diff70_79_13_22_P_div10 * new_data$Pop18Over
new_data$exposure_precip <- new_data$precip_diff70_79_13_22_P_x10 * new_data$Pop18Over
new_data$exposure_transpir <- new_data$transpir_diff70_79_13_22_P_x1000 * new_data$Pop18Over

## calculation for national level exposure
av_natl_expos_hi <- sum(new_data$exposure_hi)/sum(new_data$Pop18Over)
av_natl_expos_windu <- sum(new_data$exposure_windu)/sum(new_data$Pop18Over)
av_natl_expos_windv <- sum(new_data$exposure_windv)/sum(new_data$Pop18Over)
av_natl_expos_solar <- sum(new_data$exposure_solar)/sum(new_data$Pop18Over)
av_natl_expos_press <- sum(new_data$exposure_press)/sum(new_data$Pop18Over)
av_natl_expos_precip <- sum(new_data$exposure_precip)/sum(new_data$Pop18Over)
av_natl_expos_transpir <- sum(new_data$exposure_transpir)/sum(new_data$Pop18Over)

## calculate population weighted effect size 
popweight_effectsize_hi <- av_natl_expos_hi * coefficients_CHD70_79_13_22["HI_C_Change70_79_13_22"] 
popweight_effectsize_windu <- av_natl_expos_windu * coefficients_CHD70_79_13_22["windU_diff70_79_13_22"] 
popweight_effectsize_windv <- av_natl_expos_windv * coefficients_CHD70_79_13_22["windV_diff70_79_13_22"] 
popweight_effectsize_solar <- av_natl_expos_solar * coefficients_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
popweight_effectsize_press <- av_natl_expos_press * coefficients_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
popweight_effectsize_precip <- av_natl_expos_precip * coefficients_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] 
popweight_effectsize_transpir <- av_natl_expos_transpir * coefficients_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 
print(coefficients_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] )
print(av_natl_expos_solar)

# create confidence interval
lower_CI_natl_hi <- popweight_effectsize_hi - 1.96 * se_CHD70_79_13_22["HI_C_Change70_79_13_22"] 
lower_CI_natl_windu <- popweight_effectsize_windu - 1.96 * se_CHD70_79_13_22["windU_diff70_79_13_22"] 
lower_CI_natl_windv <- popweight_effectsize_windv - 1.96 * se_CHD70_79_13_22["windV_diff70_79_13_22"] 
lower_CI_natl_solar <- popweight_effectsize_solar  - 1.96 * se_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
lower_CI_natl_press <- popweight_effectsize_press  - 1.96 * se_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
lower_CI_natl_precip <- popweight_effectsize_precip  - 1.96 * se_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"] 
lower_CI_natl_transpir <- popweight_effectsize_transpir  - 1.96 * se_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

upper_CI_natl_hi <- popweight_effectsize_hi + 1.96 * se_CHD70_79_13_22["HI_C_Change70_79_13_22"] 
upper_CI_natl_windu <- popweight_effectsize_windu + 1.96 * se_CHD70_79_13_22["windU_diff70_79_13_22"] 
upper_CI_natl_windv <- popweight_effectsize_windv  + 1.96 * se_CHD70_79_13_22["windV_diff70_79_13_22"]  
upper_CI_natl_solar <- popweight_effectsize_solar + 1.96 * se_CHD70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
upper_CI_natl_press <- popweight_effectsize_press  + 1.96 * se_CHD70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
upper_CI_natl_precip <- popweight_effectsize_precip  + 1.96 * se_CHD70_79_13_22["precip_diff70_79_13_22_P_x10"]
upper_CI_natl_transpir <- popweight_effectsize_transpir  + 1.96 * se_CHD70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

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

#### Create the table ###########
# Combine into a data frame with lower and upper bounds in one column
CHD70_79_results <- data.frame(
  Variable = c("Heat Index Anomaly", 
               "Eastward Wind Anomaly", "Northward Wind Anomaly", "Surface-absorbed sunlight (*10^-6)", 
               "Surface Pressure Anomaly (*10^-1)", "Precipitation Anomaly (*10)",
               "Transpiration Anomaly (*10^3)"),
  Effect_Size = coefficients_CHD70_79_13_22[c("HI_C_Change70_79_13_22", 
                                              "windU_diff70_79_13_22",
                                              "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                              "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                              "transpir_diff70_79_13_22_P_x1000")],
  Confidence_Interval1 = paste("(", lower_bound_CHD70_79_13_22[c("HI_C_Change70_79_13_22", 
                                                                 "windU_diff70_79_13_22",
                                                                 "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                                                 "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                                                 "transpir_diff70_79_13_22_P_x1000")],
                               ", ", upper_bound_CHD70_79_13_22[c("HI_C_Change70_79_13_22", 
                                                                  "windU_diff70_79_13_22",
                                                                  "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                                                  "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                                                  "transpir_diff70_79_13_22_P_x1000")], ")", sep = ""),
  T_Value = tvalue_CHD70_79_13_22[c("HI_C_Change70_79_13_22", 
                                    "windU_diff70_79_13_22",
                                    "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                    "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                    "transpir_diff70_79_13_22_P_x1000")],
  Anomaly_Size = round(c(avg_HI_C_Change70_79_13_22, 
                         avg_windU_diff70_79_13_22, avg_windV_diff70_79_13_22, 
                         avg_solar_diff70_79_13_22, 
                         avg_pressure_diff70_79_13_22,
                         avg_precip_diff70_79_13_22,  avg_transpir_diff70_79_13_22),2),
  Effect_Size_Anomaly = round(c(rate_HI_C_Change70_79_13_22, rate_windU_diff70_79_13_22,
                                rate_windV_diff70_79_13_22, rate_solar_diff70_79_13_22, 
                                rate_pressure_diff70_79_13_22, rate_precip_diff70_79_13_22, 
                                rate_transpir_diff70_79_13_22),2), 
  Confidence_Interval2 = paste("(", c(lower_CI_HI_C_Change70_79_13_22, lower_CI_windU_diff70_79_13_22,
                                      lower_CI_windV_diff70_79_13_22, lower_CI_solar_diff70_79_13_22, 
                                      lower_CI_pressure_diff70_79_13_22, lower_CI_precip_diff70_79_13_22,
                                      lower_CI_transpir_diff70_79_13_22), 
                               ", ", c(upper_CI_HI_C_Change70_79_13_22, upper_CI_windU_diff70_79_13_22,
                                       upper_CI_windV_diff70_79_13_22, upper_CI_solar_diff70_79_13_22, 
                                       upper_CI_pressure_diff70_79_13_22, upper_CI_precip_diff70_79_13_22,
                                       upper_CI_transpir_diff70_79_13_22), ")", sep = ""),
  Percent_Contribution = round(c(pct_HI_C_Change70_79_13_22, 
                                 pct_windU_diff70_79_13_22, pct_windV_diff70_79_13_22, 
                                 pct_solar_diff70_79_13_22, 
                                 pct_pressure_diff70_79_13_22,
                                 pct_precip_diff70_79_13_22,  pct_transpir_diff70_79_13_22),2), 
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
print(CHD70_79_results)

# Round all values in CHD70_79_results to 2 decimal places
CHD70_79_results$Effect_Size <- round(CHD70_79_results$Effect_Size, 2)
CHD70_79_results$T_Value <- round(CHD70_79_results$T_Value, 2)


# Convert CI column to character to avoid rounding issues
CHD70_79_results$Confidence_Interval1 <- as.character(CHD70_79_results$Confidence_Interval1)

CHD70_79_results$Confidence_Interval2 <- as.character(CHD70_79_results$Confidence_Interval2)

CHD70_79_results$Confidence_Interval3 <- as.character(CHD70_79_results$Confidence_Interval3)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds1 <- sub("[(](.*),.*", "\\1", CHD70_79_results$Confidence_Interval1)
upper_bounds1 <- sub(".*, (.*)[)]", "\\1", CHD70_79_results$Confidence_Interval1)

lower_bounds2 <- sub("[(](.*),.*", "\\1", CHD70_79_results$Confidence_Interval2)
upper_bounds2 <- sub(".*, (.*)[)]", "\\1", CHD70_79_results$Confidence_Interval2)

lower_bounds3 <- sub("[(](.*),.*", "\\1", CHD70_79_results$Confidence_Interval3)
upper_bounds3 <- sub(".*, (.*)[)]", "\\1", CHD70_79_results$Confidence_Interval3)

# Handle NAs by replacing them with 0
lower_bounds1[is.na(lower_bounds1)] <- 0
upper_bounds1[is.na(upper_bounds1)] <- 0

lower_bounds2[is.na(lower_bounds2)] <- 0
upper_bounds2[is.na(upper_bounds2)] <- 0

lower_bounds3[is.na(lower_bounds3)] <- 0
upper_bounds3[is.na(upper_bounds3)] <- 0

# Round the bounds to 2 decimal places
lower_bounds1 <- round(as.numeric(lower_bounds1), 2)
upper_bounds1 <- round(as.numeric(upper_bounds1), 2)

lower_bounds2 <- round(as.numeric(lower_bounds2), 2)
upper_bounds2 <- round(as.numeric(upper_bounds2), 2)

lower_bounds3 <- round(as.numeric(lower_bounds3), 2)
upper_bounds3 <- round(as.numeric(upper_bounds3), 2)

# Combine the rounded bounds into the CI column
CHD70_79_results$CI_1 <- paste0("(", lower_bounds1, ", ", upper_bounds1, ")")
CHD70_79_results$CI_2 <- paste0("(", lower_bounds2, ", ", upper_bounds2, ")")
CHD70_79_results$CI_3 <- paste0("(", lower_bounds3, ", ", upper_bounds3, ")")

# remove numeric CI
CHD70_79_results <- CHD70_79_results[, -which(names(CHD70_79_results) == "Confidence_Interval1")]
CHD70_79_results <- CHD70_79_results[, -which(names(CHD70_79_results) == "Confidence_Interval2")]
CHD70_79_results <- CHD70_79_results[, -which(names(CHD70_79_results) == "Confidence_Interval3")]

## add new columns and sort
CHD70_79_results <- CHD70_79_results %>%
  mutate(Effect_Size_Per_Unit_CI = paste(Effect_Size, CI_1, sep = " ")) %>%
  mutate(Effect_Size_Anomaly_CI = paste(Effect_Size_Anomaly, CI_2, sep = " ")) %>%
  mutate(Pop_Weight_Effect_Size_Anomaly_CI = paste(Pop_Weighted_Effect_Size, CI_3, sep = " ")) %>%
  select(Variable, Effect_Size_Per_Unit_CI, T_Value, Anomaly_Size, Effect_Size_Anomaly_CI, Percent_Contribution,
         Average_National_Exposure, Pop_Weight_Effect_Size_Anomaly_CI, Pop_Weight_Percent_Contribution)


print(CHD70_79_results)

#write results to csv
write.csv(CHD70_79_results, "CHD70_79_13_22_Table1_09_20_2024.csv", row.names = FALSE)

########## 80-90

########## 90-2000

######### 2000-2010


#########################################################################################################################
################################################ Stroke Table 1 #########################################################
#### Create empty lists for each linear model #####################################################################
# avg stroke
avg_STROKE <- mean(subset_data$STROKE_CrudePrev, na.rm = TRUE)

# 70-79
coefficients_stroke70_79_13_22 <- list()
se_stroke70_79_13_22 <- list()
tvalue_stroke70_79_13_22 <- list()
lower_bound_stroke70_79_13_22 <- list()
upper_bound_stroke70_79_13_22 <- list()

stroke70_79_13_22 <- lmer(STROKE_CrudePrev ~  HI_C_Change70_79_13_22 + 
                            Temp_C_2013_2022 + PCT_ImperviousSurfaces +
                            SPL_THEME1 +
                            EP_AGE65 + CSMOKING_CrudePrev + CHECKUP_CrudePrev +
                            OBESITY_CrudePrev_P_div10 + windU_diff70_79_13_22 + windV_diff70_79_13_22 + solar_diff70_79_13_22_P_Mill
                          + pressure_diff70_79_13_22_P_div10 
                          + precip_diff70_79_13_22_P_x10 + transpir_diff70_79_13_22_P_x1000 + LCchangeMEAN
                          + (1 | CountyFIPS), data = subset_data)
summary(stroke70_79_13_22)


# Extract coefficients, standard errors, p-values, and calculate 95% confidence intervals
coefficients_stroke70_79_13_22 <- summary(stroke70_79_13_22)$coefficients[, "Estimate"]
se_stroke70_79_13_22 <- summary(stroke70_79_13_22)$coefficients[, "Std. Error"]
tvalue_stroke70_79_13_22 <- summary(stroke70_79_13_22)$coefficients[, "t value"]
lower_bound_stroke70_79_13_22  <-  coefficients_stroke70_79_13_22 - 1.96 * se_stroke70_79_13_22 
upper_bound_stroke70_79_13_22  <-  coefficients_stroke70_79_13_22 + 1.96 * se_stroke70_79_13_22

# average anomaly size
# 1970-1980
avg_HI_C_Change70_79_13_22 <- mean(subset_data$HI_C_Change70_79_13_22, na.rm = TRUE)
avg_windU_diff70_79_13_22 <- mean(subset_data$windU_diff70_79_13_22, na.rm = TRUE)
avg_windV_diff70_79_13_22 <- mean(subset_data$windV_diff70_79_13_22, na.rm = TRUE)
avg_solar_diff70_79_13_22 <- mean(subset_data$solar_diff70_79_13_22_P_Mill, na.rm = TRUE)
avg_pressure_diff70_79_13_22 <- mean(subset_data$pressure_diff70_79_13_22_P_div10, na.rm = TRUE)
avg_precip_diff70_79_13_22 <- mean(subset_data$precip_diff70_79_13_22_P_x10, na.rm = TRUE)
avg_transpir_diff70_79_13_22 <- mean(subset_data$transpir_diff70_79_13_22_P_x1000, na.rm = TRUE)

## get the rate of change 
rate_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 *coefficients_stroke70_79_13_22["HI_C_Change70_79_13_22"]
rate_windU_diff70_79_13_22 <- avg_windU_diff70_79_13_22 * coefficients_stroke70_79_13_22["windU_diff70_79_13_22"] 
rate_windV_diff70_79_13_22 <- avg_windV_diff70_79_13_22 * coefficients_stroke70_79_13_22["windV_diff70_79_13_22"] 
rate_solar_diff70_79_13_22 <- avg_solar_diff70_79_13_22 * coefficients_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
rate_pressure_diff70_79_13_22 <- avg_pressure_diff70_79_13_22 * coefficients_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
rate_precip_diff70_79_13_22 <- avg_precip_diff70_79_13_22 * coefficients_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
rate_transpir_diff70_79_13_22 <- avg_transpir_diff70_79_13_22 * coefficients_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

lower_CI_HI_C_Change70_79_13_22 <- rate_HI_C_Change70_79_13_22 - 1.96 * se_stroke70_79_13_22["HI_C_Change70_79_13_22"] 
lower_CI_windU_diff70_79_13_22 <- rate_windU_diff70_79_13_22 - 1.96 * se_stroke70_79_13_22["windU_diff70_79_13_22"] 
lower_CI_windV_diff70_79_13_22 <- rate_windV_diff70_79_13_22 - 1.96 * se_stroke70_79_13_22["windV_diff70_79_13_22"] 
lower_CI_solar_diff70_79_13_22 <- rate_solar_diff70_79_13_22  - 1.96 * se_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
lower_CI_pressure_diff70_79_13_22 <- rate_pressure_diff70_79_13_22  - 1.96 * se_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
lower_CI_precip_diff70_79_13_22 <- rate_precip_diff70_79_13_22  - 1.96 * se_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
lower_CI_transpir_diff70_79_13_22 <- rate_transpir_diff70_79_13_22  - 1.96 * se_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

upper_CI_HI_C_Change70_79_13_22 <- rate_HI_C_Change70_79_13_22 + 1.96 * se_stroke70_79_13_22["HI_C_Change70_79_13_22"] 
upper_CI_windU_diff70_79_13_22 <- rate_windU_diff70_79_13_22 + 1.96 * se_stroke70_79_13_22["windU_diff70_79_13_22"] 
upper_CI_windV_diff70_79_13_22 <- rate_windV_diff70_79_13_22  + 1.96 * se_stroke70_79_13_22["windV_diff70_79_13_22"]  
upper_CI_solar_diff70_79_13_22 <- rate_solar_diff70_79_13_22 + 1.96 * se_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
upper_CI_pressure_diff70_79_13_22 <- rate_pressure_diff70_79_13_22  + 1.96 * se_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
upper_CI_precip_diff70_79_13_22 <- rate_precip_diff70_79_13_22  + 1.96 * se_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
upper_CI_transpir_diff70_79_13_22 <- rate_transpir_diff70_79_13_22  + 1.96 * se_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

## get percent contribution for each variable 
# Calculate average anomaly size * coefficient for each variable
pct_HI_C_Change70_79_13_22 <- avg_HI_C_Change70_79_13_22 * coefficients_stroke70_79_13_22["HI_C_Change70_79_13_22"] / avg_STROKE *100
pct_windU_diff70_79_13_22 <- avg_windU_diff70_79_13_22 * coefficients_stroke70_79_13_22["windU_diff70_79_13_22"] / avg_STROKE *100
pct_windV_diff70_79_13_22 <- avg_windV_diff70_79_13_22 * coefficients_stroke70_79_13_22["windV_diff70_79_13_22"] / avg_STROKE *100
pct_solar_diff70_79_13_22 <- avg_solar_diff70_79_13_22 * coefficients_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] / avg_STROKE *100
pct_pressure_diff70_79_13_22 <- avg_pressure_diff70_79_13_22 * coefficients_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] / avg_STROKE *100
pct_precip_diff70_79_13_22 <- avg_precip_diff70_79_13_22 * coefficients_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] / avg_STROKE *100
pct_transpir_diff70_79_13_22 <- avg_transpir_diff70_79_13_22 * coefficients_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] / avg_STROKE *100

# adjust for population #
# turn subset into a data frame to add columns
new_data <- subset_data

### calculate total exposure in each tract
new_data$exposure_hi <- new_data$HI_C_Change70_79_13_22 * new_data$Pop18Over
new_data$exposure_windu <- new_data$windU_diff70_79_13_22 * new_data$Pop18Over
new_data$exposure_windv <- new_data$windV_diff70_79_13_22 * new_data$Pop18Over
new_data$exposure_solar <- new_data$solar_diff70_79_13_22_P_Mill * new_data$Pop18Over
new_data$exposure_press <- new_data$pressure_diff70_79_13_22_P_div10 * new_data$Pop18Over
new_data$exposure_precip <- new_data$precip_diff70_79_13_22_P_x10 * new_data$Pop18Over
new_data$exposure_transpir <- new_data$transpir_diff70_79_13_22_P_x1000 * new_data$Pop18Over

## calculation for national level exposure
av_natl_expos_hi <- sum(new_data$exposure_hi)/sum(new_data$Pop18Over)
av_natl_expos_windu <- sum(new_data$exposure_windu)/sum(new_data$Pop18Over)
av_natl_expos_windv <- sum(new_data$exposure_windv)/sum(new_data$Pop18Over)
av_natl_expos_solar <- sum(new_data$exposure_solar)/sum(new_data$Pop18Over)
av_natl_expos_press <- sum(new_data$exposure_press)/sum(new_data$Pop18Over)
av_natl_expos_precip <- sum(new_data$exposure_precip)/sum(new_data$Pop18Over)
av_natl_expos_transpir <- sum(new_data$exposure_transpir)/sum(new_data$Pop18Over)

## calculate population weighted effect size 
popweight_effectsize_hi <- av_natl_expos_hi * coefficients_stroke70_79_13_22["HI_C_Change70_79_13_22"] 
popweight_effectsize_windu <- av_natl_expos_windu * coefficients_stroke70_79_13_22["windU_diff70_79_13_22"] 
popweight_effectsize_windv <- av_natl_expos_windv * coefficients_stroke70_79_13_22["windV_diff70_79_13_22"] 
popweight_effectsize_solar <- av_natl_expos_solar * coefficients_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
popweight_effectsize_press <- av_natl_expos_press * coefficients_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
popweight_effectsize_precip <- av_natl_expos_precip * coefficients_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
popweight_effectsize_transpir <- av_natl_expos_transpir * coefficients_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

# create confidence interval
lower_CI_natl_hi <- popweight_effectsize_hi - 1.96 * se_stroke70_79_13_22["HI_C_Change70_79_13_22"] 
lower_CI_natl_windu <- popweight_effectsize_windu - 1.96 * se_stroke70_79_13_22["windU_diff70_79_13_22"] 
lower_CI_natl_windv <- popweight_effectsize_windv - 1.96 * se_stroke70_79_13_22["windV_diff70_79_13_22"] 
lower_CI_natl_solar <- popweight_effectsize_solar  - 1.96 * se_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
lower_CI_natl_press <- popweight_effectsize_press  - 1.96 * se_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
lower_CI_natl_precip <- popweight_effectsize_precip  - 1.96 * se_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
lower_CI_natl_transpir <- popweight_effectsize_transpir  - 1.96 * se_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

upper_CI_natl_hi <- popweight_effectsize_hi + 1.96 * se_stroke70_79_13_22["HI_C_Change70_79_13_22"] 
upper_CI_natl_windu <- popweight_effectsize_windu + 1.96 * se_stroke70_79_13_22["windU_diff70_79_13_22"] 
upper_CI_natl_windv <- popweight_effectsize_windv  + 1.96 * se_stroke70_79_13_22["windV_diff70_79_13_22"]  
upper_CI_natl_solar <- popweight_effectsize_solar + 1.96 * se_stroke70_79_13_22["solar_diff70_79_13_22_P_Mill"] 
upper_CI_natl_press <- popweight_effectsize_press  + 1.96 * se_stroke70_79_13_22["pressure_diff70_79_13_22_P_div10"] 
upper_CI_natl_precip <- popweight_effectsize_precip  + 1.96 * se_stroke70_79_13_22["precip_diff70_79_13_22_P_x10"] 
upper_CI_natl_transpir <- popweight_effectsize_transpir  + 1.96 * se_stroke70_79_13_22["transpir_diff70_79_13_22_P_x1000"] 

#calculate population weighted stroke for % change 
new_data$pop_stroke <- new_data$STROKE_CrudePrev * new_data$Pop18Over
av_natl_stroke <- sum(new_data$pop_stroke) / sum(new_data$Pop18Over)

# calculate % change
pct_popweight_effectsize_hi <- popweight_effectsize_hi/av_natl_stroke *100
pct_popweight_effectsize_windu <- popweight_effectsize_windu/av_natl_stroke *100
pct_popweight_effectsize_windv <- popweight_effectsize_windv/av_natl_stroke *100
pct_popweight_effectsize_solar <- popweight_effectsize_solar/av_natl_stroke *100
pct_popweight_effectsize_press <- popweight_effectsize_press/av_natl_stroke *100
pct_popweight_effectsize_precip <- popweight_effectsize_precip/av_natl_stroke *100
pct_popweight_effectsize_transpir <- popweight_effectsize_transpir/av_natl_stroke *100

#### Create the table #####################
# Combine into a data frame with lower and upper bounds in one column
stroke70_79_results <- data.frame(
  Variable = c("Heat Index Anomaly", 
               "Eastward Wind Anomaly", "Northward Wind Anomaly", "Surface-absorbed sunlight (*10^-6)", 
               "Surface Pressure Anomaly (*10^-1)", "Precipitation Anomaly (*10)",
               "Transpiration Anomaly (*10^3)"),
  Effect_Size = coefficients_stroke70_79_13_22[c("HI_C_Change70_79_13_22", 
                                                 "windU_diff70_79_13_22",
                                                 "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                                 "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                                 "transpir_diff70_79_13_22_P_x1000")],
  Confidence_Interval1 = paste("(", lower_bound_stroke70_79_13_22[c("HI_C_Change70_79_13_22", 
                                                                    "windU_diff70_79_13_22",
                                                                    "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                                                    "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                                                    "transpir_diff70_79_13_22_P_x1000")],
                               ", ", upper_bound_stroke70_79_13_22[c("HI_C_Change70_79_13_22", 
                                                                     "windU_diff70_79_13_22",
                                                                     "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                                                     "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                                                     "transpir_diff70_79_13_22_P_x1000")], ")", sep = ""),
  T_Value = tvalue_stroke70_79_13_22[c("HI_C_Change70_79_13_22", 
                                       "windU_diff70_79_13_22",
                                       "windV_diff70_79_13_22", "solar_diff70_79_13_22_P_Mill", 
                                       "pressure_diff70_79_13_22_P_div10", "precip_diff70_79_13_22_P_x10",
                                       "transpir_diff70_79_13_22_P_x1000")],
  Anomaly_Size = round(c(avg_HI_C_Change70_79_13_22, 
                         avg_windU_diff70_79_13_22, avg_windV_diff70_79_13_22, 
                         avg_solar_diff70_79_13_22, 
                         avg_pressure_diff70_79_13_22,
                         avg_precip_diff70_79_13_22,  avg_transpir_diff70_79_13_22),2),
  Effect_Size_Anomaly = round(c(rate_HI_C_Change70_79_13_22, rate_windU_diff70_79_13_22,
                                rate_windV_diff70_79_13_22, rate_solar_diff70_79_13_22, 
                                rate_pressure_diff70_79_13_22, rate_precip_diff70_79_13_22, 
                                rate_transpir_diff70_79_13_22),2), 
  Confidence_Interval2 = paste("(", c(lower_CI_HI_C_Change70_79_13_22, lower_CI_windU_diff70_79_13_22,
                                      lower_CI_windV_diff70_79_13_22, lower_CI_solar_diff70_79_13_22, 
                                      lower_CI_pressure_diff70_79_13_22, lower_CI_precip_diff70_79_13_22,
                                      lower_CI_transpir_diff70_79_13_22), 
                               ", ", c(upper_CI_HI_C_Change70_79_13_22, upper_CI_windU_diff70_79_13_22,
                                       upper_CI_windV_diff70_79_13_22, upper_CI_solar_diff70_79_13_22, 
                                       upper_CI_pressure_diff70_79_13_22, upper_CI_precip_diff70_79_13_22,
                                       upper_CI_transpir_diff70_79_13_22), ")", sep = ""),
  Percent_Contribution = round(c(pct_HI_C_Change70_79_13_22, 
                                 pct_windU_diff70_79_13_22, pct_windV_diff70_79_13_22, 
                                 pct_solar_diff70_79_13_22, 
                                 pct_pressure_diff70_79_13_22,
                                 pct_precip_diff70_79_13_22,  pct_transpir_diff70_79_13_22),2), 
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
print(stroke70_79_results)

# Round all values in stroke70_79_results to 2 decimal places
stroke70_79_results$Effect_Size <- round(stroke70_79_results$Effect_Size, 2)
stroke70_79_results$T_Value <- round(stroke70_79_results$T_Value, 2)
stroke70_79_results$Percent_Contribution <- round(stroke70_79_results$Percent_Contribution, 2)

# Convert CI column to character to avoid rounding issues
stroke70_79_results$Confidence_Interval1 <- as.character(stroke70_79_results$Confidence_Interval1)

stroke70_79_results$Confidence_Interval2 <- as.character(stroke70_79_results$Confidence_Interval2)

# Extract lower and upper bounds from CI, handle NAs, and round them to 2 decimal places
lower_bounds1 <- sub("[(](.*),.*", "\\1", stroke70_79_results$Confidence_Interval1)
upper_bounds1 <- sub(".*, (.*)[)]", "\\1", stroke70_79_results$Confidence_Interval1)

lower_bounds2 <- sub("[(](.*),.*", "\\1", stroke70_79_results$Confidence_Interval2)
upper_bounds2 <- sub(".*, (.*)[)]", "\\1", stroke70_79_results$Confidence_Interval2)

lower_bounds3 <- sub("[(](.*),.*", "\\1", stroke70_79_results$Confidence_Interval3)
upper_bounds3 <- sub(".*, (.*)[)]", "\\1", stroke70_79_results$Confidence_Interval3)

# Handle NAs by replacing them with 0
lower_bounds1[is.na(lower_bounds1)] <- 0
upper_bounds1[is.na(upper_bounds1)] <- 0

lower_bounds2[is.na(lower_bounds2)] <- 0
upper_bounds2[is.na(upper_bounds2)] <- 0

lower_bounds3[is.na(lower_bounds3)] <- 0
upper_bounds3[is.na(upper_bounds3)] <- 0

# Round the bounds to 3 decimal places
lower_bounds1 <- round(as.numeric(lower_bounds1), 2)
upper_bounds1 <- round(as.numeric(upper_bounds1), 2)

lower_bounds2 <- round(as.numeric(lower_bounds2), 2)
upper_bounds2 <- round(as.numeric(upper_bounds2), 2)

lower_bounds3 <- round(as.numeric(lower_bounds3), 2)
upper_bounds3 <- round(as.numeric(upper_bounds3), 2)

# Combine the rounded bounds into the CI column
stroke70_79_results$CI_1 <- paste0("(", lower_bounds1, ", ", upper_bounds1, ")")
stroke70_79_results$CI_2 <- paste0("(", lower_bounds2, ", ", upper_bounds2, ")")
stroke70_79_results$CI_3 <- paste0("(", lower_bounds3, ", ", upper_bounds3, ")")

# remove numeric CI
stroke70_79_results <- stroke70_79_results[, -which(names(stroke70_79_results) == "Confidence_Interval1")]
stroke70_79_results <- stroke70_79_results[, -which(names(stroke70_79_results) == "Confidence_Interval2")]
stroke70_79_results <- stroke70_79_results[, -which(names(stroke70_79_results) == "Confidence_Interval3")]

# R^2
Stroke70_79_13_22_r2 <- r.squaredGLMM(stroke70_79_13_22)

# reorder columns
stroke70_79_results <- stroke70_79_results %>%
  mutate(Effect_Size_Per_Unit_CI = paste(Effect_Size, CI_1, sep = " ")) %>%
  mutate(Effect_Size_Anomaly_CI = paste(Effect_Size_Anomaly, CI_2, sep = " ")) %>%
  mutate(Pop_Weight_Effect_Size_Anomaly_CI = paste(Pop_Weighted_Effect_Size, CI_3, sep = " ")) %>%
  select(Variable, Effect_Size_Per_Unit_CI, T_Value, Anomaly_Size, Effect_Size_Anomaly_CI, Percent_Contribution,
         Average_National_Exposure, Pop_Weight_Effect_Size_Anomaly_CI, Pop_Weight_Percent_Contribution)
print(stroke70_79_results)

#write results to csv
write.csv(stroke70_79_results, "STROKE70_79_13_22_Table1_09_20_2024.csv", row.names = FALSE)



############### PCA at the Climate Division Level

library(dplyr)

# Define the anomaly columns

anomaly_columns_revised <- c(
  "CLIMDIV",
  "HI_C_Change70_79_13_22",
  "Change_Temp_C_70_79_13_22",
  "Change_RH_70_79_13_22",
  "windU_diff70_79_13_22",
  "windV_diff70_79_13_22",
  "latent_diff70_79_13_22",
  "solar_diff70_79_13_22",
  "thermal_diff70_79_13_22",
  "sensible_diff70_79_13_22",
  "evap_diff70_79_13_22",
  "pressure_diff70_79_13_22",
  "precip_diff70_79_13_22",
  # "transpir_diff70_79_13_22", # <-- This variable is now excluded
  "downwards_solar_diff70_79_13_22"
)

# Group by CLIMDIV and get the unique anomaly value for each division.

division_anomalies_revised <- merged_data %>%
  select(all_of(anomaly_columns_revised)) %>%
  group_by(CLIMDIV) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Scale the data and prepare for PCA

pca_data_division_revised <- division_anomalies_revised %>%
  select(-CLIMDIV) %>%
  mutate(
    latent_diff70_79_13_22 = latent_diff70_79_13_22 / 1000000,
    solar_diff70_79_13_22 = solar_diff70_79_13_22 / 1000000,
    thermal_diff70_79_13_22 = thermal_diff70_79_13_22 / 1000000,
    sensible_diff70_79_13_22 = sensible_diff70_79_13_22 / 1000000,
    downwards_solar_diff70_79_13_22 = downwards_solar_diff70_79_13_22 / 1000000,
    pressure_diff70_79_13_22 = pressure_diff70_79_13_22 / 10,
    evap_diff70_79_13_22 = evap_diff70_79_13_22 * 10,
    precip_diff70_79_13_22 = precip_diff70_79_13_22 * 10
    # The scaling for transpiration is now removed
  )

# Run the PCA with the revised data

pca_division_results_revised <- prcomp(pca_data_division_revised,
                                       center = TRUE,
                                       scale. = TRUE,
                                       rank. = 8)
# View the new results

cat("--- PCA Summary (Excluding Transpiration) ---\n")
print(summary(pca_division_results_revised))

cat("\n--- PCA Loadings (Excluding Transpiration) ---\n")
print(pca_division_results_revised$rotation)

############### Start: Code for PCA Sensitivity Analysis  ##################

# Load required libraries
library(lme4)
library(dplyr)
# Extract PC Scores and merge with CLIMDIV key
# The scores (pca_results$x) are in the same order as the division_anomalies_revised
pc_scores <- as.data.frame(pca_division_results_revised$x)
climdiv_keys <- division_anomalies_revised$CLIMDIV
pc_scores_with_key <- cbind(CLIMDIV = climdiv_keys, pc_scores)

# Merge PC scores into the main tract-level subset_data
# The left_join uses the existing 'subset_data' and adds the PC columns

subset_data_with_pca <- left_join(subset_data, pc_scores_with_key, by = "CLIMDIV")
# Create PCA Summary Table

pca_summary <- summary(pca_division_results_revised)
pca_importance <- as.data.frame(t(pca_summary$importance))

# Calculate Eigenvalues (Variance = SD^2)

pca_importance$Eigenvalue <- pca_importance$"Standard deviation"^2
pca_importance$Component <- rownames(pca_importance)
pca_summary_table <- pca_importance[, c("Component", "Eigenvalue", "Proportion of Variance", "Cumulative Proportion")]

cat("\n--- PCA Summary Table (Climate Division Level) ---\n")
print(pca_summary_table)

# Run models for each component and outcome
# Define the covariates from your Figure 2 models
model_covariates <- "OBESITY_CrudePrev_P_div10 + EP_AGE65 + PCT_ImperviousSurfaces + CSMOKING_CrudePrev + CHECKUP_CrudePrev + SPL_THEME1 + Temp_C_2013_2022 + LCchangeMEAN + (1 | CountyFIPS)"

# Get component names (PC1, PC2, ..., PC8)
pc_names <- colnames(pc_scores)
results_list <- list()
# Loop for CHD

cat("\nRunning models for CHD...\n")

for (pc in pc_names) {
  formula_chd <- as.formula(paste("CHD_CrudePrev ~", pc, "+", model_covariates))
  # Run model, suppressing potential convergence warnings
  mod_chd <- suppressMessages(lmer(formula_chd, data = subset_data_with_pca))
  # Extract coefficient for the PC term
  s_chd <- summary(mod_chd)$coefficients
  if (pc %in% rownames(s_chd)) {
    estimate <- s_chd[pc, "Estimate"]
    std_error <- s_chd[pc, "Std. Error"]
    results_list[[paste("CHD", pc)]] <- data.frame(
      Outcome = "CHD",
      Component = pc,
      Estimate = estimate,
      Std.Error = std_error,
      Lower_CI = estimate - 1.96 * std_error,
      Upper_CI = estimate + 1.96 * std_error,
      row.names = NULL
    )
  }
}

# Loop for Stroke

cat("Running models for Stroke...\n")

for (pc in pc_names) {
  
  formula_stroke <- as.formula(paste("STROKE_CrudePrev ~", pc, "+", model_covariates))
  mod_stroke <- suppressMessages(lmer(formula_stroke, data = subset_data_with_pca))
  s_stroke <- summary(mod_stroke)$coefficients
  if (pc %in% rownames(s_stroke)) {
    estimate <- s_stroke[pc, "Estimate"]
    std_error <- s_stroke[pc, "Std. Error"]
    results_list[[paste("Stroke", pc)]] <- data.frame(
      Outcome = "Stroke",
      Component = pc,
      Estimate = estimate,
      Std.Error = std_error,
      Lower_CI = estimate - 1.96 * std_error,
      Upper_CI = estimate + 1.96 * std_error,
      row.names = NULL
    )
  }
}

# Combine all results into one data frame

final_model_results <- do.call(rbind, results_list)

rownames(final_model_results) <- NULL

# --- NEW: Join model results with PCA summary statistics ---

final_table_with_pca_stats <- left_join(
  final_model_results,
  pca_summary_table,
  by = "Component"
)

# Reorder columns for clarity

final_table_with_pca_stats <- final_table_with_pca_stats %>%
  
  select(
    Outcome,
    Component,
    Eigenvalue,
    `Proportion of Variance`,
    `Cumulative Proportion`,
    Estimate,
    Std.Error,
    Lower_CI,
    Upper_CI
  )

cat("\n--- Model Results Table (Sensitivity Analysis) ---\n")

print(final_table_with_pca_stats)