# Please update the working directory to the folder this master script is in

working_directory <- "~/Desktop/HI_CVD_Figures_Project" # CHANGE THIS
working_directory_data <- paste0(working_directory,"/data")
working_directory_output <- paste0(working_directory,"/outputs")

# RUN THESE ONCE AND COMMENT THEM OUT
# install.packages("dplyr")
# install.packages("lme4")
# install.packages("ggplot2")
# install.packages("tableone")
# install.packages("performance")
# install.packages("corrplot")
# install.packages("MuMIn")
# install.packages("sjstats")
# install.packages("car")
# install.packages("Hmisc")
# install.packages("grid")
# install.packages("tibble")
# install.packages("igraph")
# install.packages("ape")
# install.packages("spdep")
# install.packages("extrafont")
# install.packages("stringr")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("ggeffects")

library("dplyr")
library("lme4")
library("ggplot2")
library("tableone")
library("performance")
library("corrplot")
library("MuMIn")
library("sjstats")
library("car")
library("Hmisc")
library("grid")
library("tibble")
library("igraph")
library("ape")
library("spdep")
library("extrafont")
library("stringr")
library("tidyverse")
library("readxl")
library("ggeffects")

setwd(working_directory)
source("01_data_loading_oldscripts.R")

setwd(working_directory)
source("02_setup.R")

setwd(working_directory)
source("03_migration_analysis.R")

setwd(working_directory)
source("04_model_finder_R2.R")

setwd(working_directory)
source("05_model_generation.R")

setwd(working_directory)
source("06_table_generation.R")

setwd(working_directory)
source("07_visualization.R")

setwd(working_directory)
source("08_sensitivity_analysis.R")

setwd(working_directory)
source("09_stratification.R")

setwd(working_directory)
source("10_mainfigures_oldscripts.R")