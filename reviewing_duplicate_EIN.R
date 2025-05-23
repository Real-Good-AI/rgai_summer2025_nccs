library(tidyverse)
library(data.table)
library(cchsflow) # for is_equal(), which will return TRUE for NA==NA

# function that takes in data_file and returns a data table with information about which
# columns differ when a set of rows have the same EIN
duplicateEIN2_info <- function(data_file){
    # Get list of EIN2 to check; all EIN2 that appear more than once in the data file
    duplicate_EIN2 <- data_file |>
        count(EIN2) |>
        filter(n > 1)
    
    # Subset of data file but only rows corresponding to EIN2 that appear more than once
    core_duplicates <- data_file |>
        filter(EIN2 %in% duplicate_EIN2$EIN2)
    
    # Split the data by EIN2 to loop over groups
    duplicate_groups <- split(core_duplicates, core_duplicates$EIN2)
    
    # Initialize counters, one for counting differences and one for counting differences that may be due to missing value
    count_list <- rep(0, ncol(data_file))
    count_list_NA <- rep(0, ncol(data_file))
    
    # For each EIN2 in list, compare the corresponding rows and note columns where there are differences
    for (subset in duplicate_groups) {
        # depending on how many times the EIN was repeated, do different comparisons (if more than 3 times, skipped for now)
        if (nrow(subset) == 2){
            diff_cols <- is_equal(subset[1,], subset[2,])
            diff_cols_NA <- (subset[1,] == subset[2,])
        } else if (nrow(subset) == 3) {
            diff_cols <- (is_equal(subset[1,], subset[2,]) & is_equal(subset[1,],subset[3,]) & is_equal(subset[2,],subset[3,]))
            diff_cols_NA <- ((subset[1,] == subset[2,]) & (subset[2,] == subset[3,]) & (subset[1,] == subset[3,]))
        } else {
            print(paste("EIN", subset[1,1], "occurs", nrow(subset), "times, skipped", sep = " "))
            next
        }
        # indices of columns where there was a mismatch (or missing value)
        indices <- which(!diff_cols)
        indices_NA <- which(is.na(diff_cols_NA))
        
        # increment where there was a mismatch (or missing value)
        count_list[indices] <- count_list[indices] + 1
        count_list_NA[indices_NA] <- count_list_NA[indices_NA] + 1
    }
    
    # Create a table with the info we just gathered
    duplicates_info <- data.table(variable_name = colnames(data_file), num_differences = count_list, num_NA = count_list_NA, dtype = sapply(data_file, class))
    return(list(info = duplicates_info, dupes = duplicate_EIN2))
}

# Example Usage (from another file):
# source("reviewing_duplicate_EIN.R")
# core_data_2022 <- read_csv("CORE/CORE-2022-501C3-CHARITIES-PZ-HRMN.csv")
# dupe2022_info <- duplicateEIN2_info(core_data_2022)
# dupe2022_info_tab <- dupe2022_info$info
# dupe2022_info_EIN_list <- dupe2022_info$dupes 