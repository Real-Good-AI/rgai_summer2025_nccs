library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
source("SCRIPTS/reviewing_duplicate_EIN.R")
source("SCRIPTS/clean_helper.R")

clean_CORE <- function(all_relevant_vars, year_values, file_name_tag, file_dir, save_dir, prop_NA, filename_save){
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
      
      # If F9_00_EXEMPT_STAT_501C3_X is one of the relevant variables, create ORG_TYPE column containing a number indicating the type of 501(c) org
      if ("F9_00_EXEMPT_STAT_501C3_X" %in% all_relevant_vars){      
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
      
      # If the organization type is -1 or 0, replace with NA (because must be mistake)
      long_core$ORG_TYPE <- na_if(long_core$ORG_TYPE, -1)
      long_core$ORG_TYPE <- na_if(long_core$ORG_TYPE, 0)
      
      # In any numeric columns except LAT and LONG, convert negative values to NA and rename F990_TOTAL_ASSETS_RECENT to SIZE
      long_core <- long_core |> 
        mutate(across(
          .cols = where(is.numeric) & !any_of(c("LATITUDE", "LONGITUDE")),
          .fns = ~ replace(., . < 0, NA))) |>
        rename(SIZE.CTS = `F990_TOTAL_ASSETS_RECENT`)
      
      # Create a factor version of size by splitting SIZE into categories
      # Levels: [0,100000) [100000,500000) [500000,1000000) [1000000,5000000) [5000000,10000000) [10000000,7.02e+10)
      breaks_vec = c(0,100000,500000,1000000,5000000,10000000,max(long_core$SIZE, na.rm = TRUE)+1)
      long_core$SIZE.CAT <- long_core$SIZE.CTS |> cut(breaks = breaks_vec, right = FALSE, labels = FALSE) |> factor()
      
      # Add in Census Regions and Divisions based on state (CENSUS_STATE_ABBR)
      if ("CENSUS_STATE_ABBR" %in% names(long_core)){long_core <- add_regions_and_divisions(long_core) |> rename(STATE = `CENSUS_STATE_ABBR`)}
      
      # Rename NTEE category mainly for aesthetic reasons :P
      if ("NTEEV2" %in% names(long_core)){long_core <- long_core |> rename(NTEE = NTEEV2)}
      
      # Save
      if (missing(filename_save)){filename_save <- paste(save_dir, "cleanCORE_", year_values[1], "-", year_values[length(year_values)], ".csv", sep="")}
      print(filename_save)
      write_csv(long_core, filename_save)
}