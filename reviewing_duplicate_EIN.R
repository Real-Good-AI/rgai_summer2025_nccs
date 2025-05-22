library(tidyverse)
library(data.table)
library(cchsflow)

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
    
    # Instantiate list of counts; entry i is number of times there was a mismatch in the i-th column
    count_list <- rep(0, ncol(data_file))
    
    # This is to count the number of times the difference might be due to missing values
    count_list_NA <- rep(0, ncol(data_file))
    
    # For each EIN2 in list, compare the corresponding rows and note columns where there are differences
    for (idx in 1:nrow(duplicate_EIN2)) {
        #print(paste(idx, ",", duplicate_EIN2$EIN2[idx], ",", duplicate_EIN2$n[idx]), sep=" ")
        ein <- duplicate_EIN2$EIN2[idx] # current EIN
        subset <- core_duplicates |> filter(EIN2 == ein) # give me the rows corresponding to current EIN
        
        # depending on how many times the EIN was repeated, do different comparisons (if more than 3 times, skipped for now)
        if (nrow(subset) == 2){
            diff_cols <- is_equal(subset[1,], subset[2,])
            diff_cols_NA <- (subset[1,] == subset[2,])
        } else if (nrow(subset) == 3) {
            diff_cols <- (is_equal(subset[1,], subset[2,]) & is_equal(subset[1,],subset[3,]) & is_equal(subset[2,],subset[3,]))
            diff_cols_NA <- ((subset[1,] == subset[2,]) & (subset[2,] == subset[3,]) & (subset[1,] == subset[3,]))
        } else {
            print(paste("EIN", ein, "occurs", nrow(subset), "times, skipped", sep = " "))
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
    duplicates_info <- data.table(variable_name = colnames(data_file), num_differences = count_list, num_NA = count_list_NA, dtype = sapply(core_data_1989, class))
    return(list(info = duplicates_info, dupes = duplicate_EIN2))
}

# Example Usage:
# core_data_2022 <- read_csv("CORE/CORE-2022-501C3-CHARITIES-PZ-HRMN.csv")
# dupe2022_info <- duplicateEIN2_info(core_data_2022)