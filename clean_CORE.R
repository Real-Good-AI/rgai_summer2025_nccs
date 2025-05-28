library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
source("reviewing_duplicate_EIN.R")
source("clean_helper.R")

asset_vars <- c("F9_10_ASSET_TOT_BOY", "F9_10_ASSET_TOT_EOY")
revenue_vars <- c("F9_01_REV_TOT_CY", "F9_08_REV_TOT_TOT")
expenses_vars <- c("F9_01_EXP_TOT_CY")
benefits_vars <- c("F9_01_EXP_BEN_PAID_MEMB_CY")

all_outcome_vars <- c(asset_vars, revenue_vars, expenses_vars, benefits_vars)

prop_NA <- 0.55 # If a column has more than this fraction of missing values, it will be dropped
year_values <- seq(from = 2019, to = 2022, by = 1)
file_name_tag <- "-501C3-CHARITIES-PC-HRMN.csv" #"-501C3-CHARITIES-PC-HRMN.csv"
file_dir <- "CORE/pc/CORE-"
save_dir <- "CLEAN/pc/"

# load data into environment
for (i in year_values){
      filename <- paste(file_dir, i, file_name_tag, sep = "")
      varname <- paste("core", i, sep = "")
      assign(varname, as.data.frame(read_csv(filename, show_col_types = FALSE)))
}

# replace empty strings with NAs
for (i in year_values){
      varname <- paste("core", i, sep = "")
      dat <- get(varname) |> mutate_if(is.character, ~na_if(.,''))
      assign(varname, dat)
}

# get list of duplicate EIN plus data frame grouped by each EIN dupe
for (i in year_values){
      varname_load <- paste("core", i, sep = "")
      dat <- dupes(get(varname_load)) 
      varname <- paste("dupes", i, sep = "")
      assign(varname, dat)
}

for (i in year_values){
      varname_load <- paste("dupes", i, sep = "")
      varname <- paste("core", i, sep = "")
      print(paste("Year:", i, ", Num dupes:", nrow(get(varname_load)$dupes), ", Total num:", nrow(get(varname)), ", Prop:", nrow(get(varname_load)$dupes)/nrow(get(varname)), sep = " "))
}

# How much data is missing and where?
for (i in year_values){
      varname_load <- paste("core", i, sep = "")
      varname <- paste("na", i, sep = "")
      assign(varname, na_counts_df(get(varname_load)))
}

# Create list of variable names where data is missing for more than half the records in any dataframe
cols_to_drop <- c()
num_records <- c()

for (i in year_values){
      varname_load <- paste("na", i, sep = "")
      dat <- get(varname_load) |> filter(percent > prop_NA)
      cols_to_drop <- union(cols_to_drop, dat[,1])
      num_records <- c(num_records, c(nrow(dat)))
}

print(paste("The following outcome variables are going to be dropped: ",intersect(all_outcome_vars, cols_to_drop)))

# drop columns that have too much missing data
for (i in year_values){
      varname <- paste("core", i, sep = "")
      dat <- get(varname) 
      assign(varname, dat[, !(names(dat) %in% cols_to_drop)])
}

# remove any duplicate rows
for (i in year_values){
      varname <- paste("core", i, sep = "")
      assign(varname, get(varname) |> distinct())
}

# For any repeated EIN2 rows, keep row with highest/latest F9_00_TAX_PERIOD_END_DATE
# Reasoning: most repeated EIN2 have a discrepancy in this column, should take the one filed with latest date! If have the same date, one is chosen arbitrarily (due to the "tie")
for (i in year_values){
      varname <- paste("core", i, sep = "")
      dat <- get(varname) %>%
            arrange(desc(F9_00_TAX_PERIOD_END_DATE)) %>% # sort rows by date in descending order (latest to oldest)
            distinct(EIN2, .keep_all = TRUE)   # keep first row per EIN2 only
      assign(varname, dat)
}

# Save current core dataframes into separate CSV files
for (i in year_values){
      varname <- paste("core", i, sep = "")
      write_csv(get(varname), paste(save_dir, varname, ".csv", sep = ""))
      print(paste(save_dir, varname, ".csv", sep = ""))
}
