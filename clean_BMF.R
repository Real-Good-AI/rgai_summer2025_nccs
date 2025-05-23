library(tidyverse)
library(data.table)
library(dplyr)
library(tibble)
source("reviewing_duplicate_EIN.R")

unified_bmf <- as.data.frame(read.csv("CORE/BMF_UNIFIED_V1.1.csv"))

# Replace any empty strings '' with NA values (so later when checking for missing values is easier)
unified_bmf <- unified_bmf |> mutate_if(is.character, ~na_if(.,''))

# Remove any records corresponding to "EIN-00-0000000" since that's an impossible EIN
unified_bmf <- unified_bmf[!(unified_bmf$EIN2 %in% "EIN-00-0000000"),]

# Keep only variables we care about
vars_to_keep <- c("EIN2", "NTEEV2", "F990_TOTAL_REVENUE_RECENT", "F990_TOTAL_INCOME_RECENT", "F990_TOTAL_ASSETS_RECENT", "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME", "ORG_YEAR_FIRST", "ORG_YEAR_LAST")
bmf_subset <- unified_bmf[vars_to_keep]
bmf_subset$NTEEV2 <- substr(bmf_subset$NTEEV2, 1, 3)
bmf_subset <- bmf_subset |> distinct() # remove any duplicate rows

# For any repeated EIN2 rows, keep row with fewest NA values
# When there are ties, it keeps the first one: this means we are somewhat arbitrarily choosing an NTEEV2  industry group when the records differ
cleanBMF <- bmf_subset %>%
    arrange(rowSums(is.na(.))) %>%        # sort rows by number of NAs
    distinct(EIN2, .keep_all = TRUE)   # keep first row per EIN2 only

# How much of the data am I dropping by dropping missing values?
n <- nrow(cleanBMF)
na_count <- sapply(cleanBMF, function(y) sum(length(which(is.na(y))))) # get na_counts per column
na_count <- data.frame(na_cnt = na_count)
na_count <- na_count |>
    mutate(percent = na_cnt/n)
na_count <- tibble::rownames_to_column(na_count, "var_name")
write_csv(na_count, "CLEAN/cleanBMF_NA_counts.csv")

# drop records with missing values
cleanBMF <- cleanBMF |> drop_na()
write_csv(cleanBMF, "CLEAN/cleanBMF.csv")