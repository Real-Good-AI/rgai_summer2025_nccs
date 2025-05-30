# Script to download data and do some cleaning/pre-processing; df is the final product
library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
source("SCRIPTS/clean_CORE.R")
source("SCRIPTS/clean_helper.R")
source("SCRIPTS/download_data.R")
source("SCRIPTS/clean_BMF.R")

# Download data (make sure you are in the home directory "rgai_summer2025")
tscope_values <- c("501c3") # Download files from Tax Exempt Types in this list; possible values: "501c3", "501ce" 
fscope_values <- c("-pz") # Download files from IRS 990 Form Scope in this list; possible values: "-pz", "-pc", "-pf"
year_values <- seq(from = 1989, to = 2022, by = 1) # Download files from years in this list

download_CORE(tscope_values, fscope_values, year_values) # this function from download_data.R will download the data for you onto your local computer
download_dicts() # this function from download_data.R will download the data dictionaries and Business Master File (BMF) for you onto your local computer

# Clean Business Master File (BMF)
vars_to_keep <- c("EIN2", "NTEEV2", "F990_TOTAL_ASSETS_RECENT", "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME", "ORG_YEAR_FIRST", "ORG_YEAR_LAST", "LATITUDE", "LONGITUDE", "CENSUS_CBSA_FIPS")
thresh <- 0.5*length(vars_to_keep) # Any rows with more NA values than this threshold will be dropped
dir.create("CLEAN")
clean_BMF(vars_to_keep, thresh) # this function from clean_BMF.R cleans the BMF file for you and saves to your computer in CLEAN folder

# Clean CORE Data and merge with BMF
outcome_vars <- c("F9_10_ASSET_TOT_BOY", "F9_10_ASSET_TOT_EOY", "F9_01_REV_TOT_CY", "F9_08_REV_TOT_TOT")
predictor_vars <- c("F9_00_EXEMPT_STAT_501C3_X", "TAX_YEAR")
all_relevant_vars <- c(outcome_vars, predictor_vars, "EIN2", "F9_00_TAX_PERIOD_END_DATE")

prop_NA <- 0.5 # If a column has more than this fraction of missing values, warning message prints
file_name_tag <- "-501C3-CHARITIES-PZ-HRMN.csv" 
file_dir <- "CORE/pz/CORE-"
save_dir <- "CLEAN/pz/"

dir.create(c("CLEAN/pz", "CLEAN/pf", "CLEAN/pc"))
clean_CORE(all_relevant_vars, year_values, file_name_tag, file_dir, save_dir, prop_NA) # this function from clean_CORE.R cleans the CORE files, merges with cleaned BMF file, and saves resulting data frame

# Load the (cleaned/merged) data frame into the environment
filename <- paste(save_dir, "cleanCORE_", year_values[1], "-", year_values[length(year_values)], ".csv", sep="")
df <- as.data.table(read_csv(filename, show_col_types = FALSE))