#####################################################################################################
# R ENVIRONMENT SETUP ----
#
#####################################################################################################

#### Set working directory ####

setwd(working_directory_data) #for MacOS

#### load RData ####

load("commondata.Rdata")
downwards_solar <- read.csv("DecadeMeans_Downwards_Solar_07_29_2024.csv", header = TRUE, na.strings = "NA")
sensitivity70_89_03_22_merged <- left_join(
  sensitivity70_89_03_22_merged, 
  downwards_solar, 
  by = "CLIMDIV"  # Join based on matching CLIMDIV values
)

sensitivity70_89_03_22_merged$downwards_solar_diff70_89_03_22_P_Mill <- sensitivity70_89_03_22_merged$downwards_solar_diff70_89_03_22 / 1000000
sensitivity70_89_03_22_merged$evap_diff70_89_03_22_P_Ten <- sensitivity70_89_03_22_merged$evap_diff70_89_03_22 * 10

# Select columns containing "70_89_03_22" in their names
sensitivity70_89_03_22_filtered <- sensitivity70_89_03_22_merged %>% 
  select(contains("70_89_03_22"))

setwd(working_directory_output)
