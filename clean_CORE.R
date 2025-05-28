library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
source("reviewing_duplicate_EIN.R")
source("clean_helper.R")

clean_CORE <- function(all_relevant_vars, year_values, file_name_tag, file_dir, save_dir, prop_NA){
      for (i in year_values){
      filename <- paste(file_dir, i, file_name_tag, sep = "")
      varname <- paste("core", i, sep = "")
      
      # load data into environment and replace empty strings with NAs
      dat <-  as.data.frame(read_csv(filename, show_col_types = FALSE)) %>% mutate_if(is.character, ~na_if(.,''))
      
      # drop columns that are not relevant to current task and remove any rows that are exact duplicates of each other
      dat <- dat[, names(dat) %in% all_relevant_vars] |> distinct()
      
      # save the data into a variable
      assign(varname, dat)
      }
      
      # get list of duplicate EIN plus data frame grouped by each EIN dupe
      cat("\nAny duplicate EIN in orginal files?")
      for (i in year_values){
            varname_load <- paste("core", i, sep = "")
            varname <- paste("dupes", i, sep = "")
            assign(varname, dupes(get(varname_load)))
            print(paste("Year:", i, ", Num dupes:", nrow(get(varname)$dupes), ", Total num:", nrow(get(varname_load)), ", Prop:", nrow(get(varname)$dupes)/nrow(get(varname_load)), sep = " "))
            
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
      
      # Create Total Revenue Col TOT_REV
      # if both revenue variables exist, choose max value for each row to save in TOT_REV
      # otherwise, rename the one revenue variable that does exist to TOT_REV
      # if neither revenue variable is in the data, skip and throw a warning message
      for (i in year_values){
            varname <- paste("core", i, sep = "")
            dat <- get(varname)
      
            has_col1 <- "F9_01_REV_TOT_CY" %in% names(dat)
            has_col2 <- "F9_08_REV_TOT_TOT" %in% names(dat)
            
            if (has_col1 && has_col2) {
                  dat <- dat |>
                        mutate(TOT_REV = pmax(`F9_01_REV_TOT_CY`, `F9_08_REV_TOT_TOT`, na.rm = TRUE)) |>
                        select(-`F9_01_REV_TOT_CY`, -`F9_08_REV_TOT_TOT`)
            } else if (has_col1) {
                  dat <- dat |> rename(TOT_REV = `F9_01_REV_TOT_CY`)
            } else if (has_col2) {
                  dat <- dat |> rename(TOT_REV = `F9_08_REV_TOT_TOT`)
            } else {
                  warning(paste("Skipping year", i, "- neither revenue column found in", varname))
                  next
            }
            # save data into variable and remove the F9_00_TAX_PERIOD_END_DATE column since no longer needed
            assign(varname, dat |> select(-`F9_00_TAX_PERIOD_END_DATE`))
      }
      
      # How much data is missing and where?
      cols_to_drop <- c()
      cat("\n")
      print(paste("Any columns with data missing in over", prop_NA*100, "% of the rows?"))
      for (i in year_values){
            varname_load <- paste("core", i, sep = "")
            varname <- paste("na", i, sep = "")
            assign(varname, na_counts_df(get(varname_load)))
            
            # Create list of variable names where data is missing for more than half the records in any dataframe
            dat <- get(varname) |> filter(percent > prop_NA)
            cols_to_drop <- union(cols_to_drop, dat[,1])
            print(paste(i, ":", dat[,1]))
      }
      
      # Create ORG_TYPE column containing a number indicating the type of 501(c) org
      for (i in year_values){
            varname <- paste("core", i, sep = "")
            dat <- get(varname)
      
            if ("F9_00_EXEMPT_STAT_501C3_X" %in% names(dat)) {
                  dat <- dat |> rename(ORG_TYPE = `F9_00_EXEMPT_STAT_501C3_X`)
                  dat$ORG_TYPE <- as.numeric(dat$ORG_TYPE)
            } else { dat <- dat |> mutate(ORG_TYPE = -1) }
            
            # save data into variable
            assign(varname, dat)
      }
      
      # Get the column names that are common to all data frames and only keep those columns and save to .csv
      common_cols <- Reduce(intersect, lapply(mget(paste("core", year_values, sep="")), names))
      cat(paste("\nThe following variable names will not be in the dataset:\n", paste(setdiff(all_relevant_vars, common_cols), collapse = "\n"), sep = ""))
      
      cat("\nSaving CORE files...")
      for (i in year_values){
            varname <- paste("core", i, sep = "")
            dat <- get(varname) 
            assign(varname, dat[, (names(dat) %in% common_cols)])
            
            # Save current core dataframes into separate CSV files
            write_csv(get(varname), paste(save_dir, varname, ".csv", sep = ""))
            print(paste(save_dir, varname, ".csv", sep = ""))
      }
      
      # stack the rows from each data frame on top of each other (to keep in long format)
      long_core <- as.data.table(bind_rows(mget(paste("core", year_values, sep=""))))
      
      # Merge with metadata from Business Master File (BMF)
      bmf <- as.data.table(read_csv("CLEAN/cleanBMF.csv", show_col_types = FALSE))
      long_core <- long_core |> left_join(bmf, join_by(EIN2))
      
      # Save
      filename <- paste(save_dir, "cleanCORE_", year_values[1], "-", year_values[length(year_values)], ".csv", sep="")
      print(filename)
      write_csv(long_core, filename)
}