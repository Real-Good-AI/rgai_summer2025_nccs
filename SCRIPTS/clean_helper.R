library(tibble)
library(dplyr)

compare_pair_dt <- function(dt_group, dollar_cols) {
      # number of records in this group
      n_records <- nrow(dt_group)
      
      # drop keys (convert to numeric matrix)
      mat <- as.matrix(dt_group)
      
      # names of all columns in the group
      coln <- colnames(dt_group)
      
      # indices of dollar columns
      dollar_idx <- match(dollar_cols, coln)
      
      # compute differences only if there are exactly 2 rows
      if (n_records == 2) {
            r1 <- as.numeric(mat[1, ])
            r2 <- as.numeric(mat[2, ])
            
            diffs <- r1 - r2
            abs_diffs <- abs(diffs)
            
            # check missing-caused differences only for dollar columns
            had_missing_diff <- any(xor(is.na(r1[dollar_idx]), is.na(r2[dollar_idx])))
            
      } else {
            # for groups with != 2 rows, set diffs to NA
            diffs <- abs_diffs <- numeric(ncol(dt_group))
            diffs[] <- NA
            abs_diffs[] <- NA
            had_missing_diff <- NA
      }
      
      # differences for dollar columns (NA if group != 2 rows)
      diffs_named <- as.list(setNames(diffs[match(dollar_cols, coln)], dollar_cols))
      
      # combine into one data.table row
      data.table(
            n_records      = n_records,                 # <-- new column
            n_diff_cols    = sum(abs_diffs != 0 & !is.na(abs_diffs)),
            n_diff_gt1     = sum(abs_diffs > 1, na.rm = TRUE),
            max_abs_diff   = max(abs_diffs, na.rm = TRUE),
            had_missing_diff = had_missing_diff,
            multi_col_diff = sum(abs_diffs != 0, na.rm = TRUE) > 1
      )[, c(.SD, diffs_named)]
}

na_counts_df <- function(df){
    n <- nrow(df)
    na_count <- sapply(df, function(y) sum(length(which(is.na(y))))) # get na_counts per column
    na_count <- data.frame(na_cnt = na_count)
    na_count <- na_count |>
        mutate(percent = na_cnt/n)
    na_count <- tibble::rownames_to_column(na_count, "var_name")
    return(na_count)
}

add_regions_and_divisions <- function(df){
      # Census Regions
      west <- c("AK","WA", "OR", "CA", "HI", "ID", "NV", "AZ", "MT", "UT", "WY", "CO", "NM")
      midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH")
      northeast <- c("NY", "PA", "NJ", "VT", "MA", "CT", "NH", "RI", "ME")
      south <- c("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "NC", "SC", "GA", "FL", "MD", "DC", "DE")
      
      # Add regions
      df <- df |> mutate(REGION = case_when(
            is.na(CENSUS_STATE_ABBR) ~ NA,
            CENSUS_STATE_ABBR %in% west ~ "WEST",
            CENSUS_STATE_ABBR %in% midwest ~ "MIDWEST",
            CENSUS_STATE_ABBR %in% northeast ~ "NORTHEAST",
            CENSUS_STATE_ABBR %in% south ~ "SOUTH",
            .default = "TERRITORY"
      ))
      
      # Census Divisions
      pacific <- c("AK","WA", "OR", "CA", "HI")
      mountain <- c("ID", "NV", "AZ", "MT", "UT", "WY", "CO", "NM")
      northWest.central <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO")
      northEast.central <- c("WI", "IL", "MI", "IN", "OH")
      southWest.central <- c("OK", "TX", "AR", "LA")
      southEast.central <- c("KY", "TN", "MS", "AL")
      south.atlantic <- c("WV", "VA", "NC", "SC", "GA", "FL", "MD", "DC", "DE")
      middle.atlantic <- c("NY", "PA", "NJ")
      newEngland <- c("VT", "MA", "CT", "NH", "RI", "ME")
      
      # Add divisions
      df <- df |> mutate(DIVISION = case_when(
            is.na(CENSUS_STATE_ABBR) ~ NA,
            CENSUS_STATE_ABBR %in% pacific ~ "PACIFIC",
            CENSUS_STATE_ABBR %in% mountain ~ "MOUNTAIN",
            CENSUS_STATE_ABBR %in% northWest.central ~ "CENTRAL-NW",
            CENSUS_STATE_ABBR %in% northEast.central ~ "CENTRAL-NE",
            CENSUS_STATE_ABBR %in% southWest.central ~ "CENTRAL-SW",
            CENSUS_STATE_ABBR %in% southEast.central ~ "CENTRAL-SE",
            CENSUS_STATE_ABBR %in% south.atlantic ~ "ATLANTIC-S",
            CENSUS_STATE_ABBR %in% middle.atlantic ~ "ATLANTIC-M",
            CENSUS_STATE_ABBR %in% newEngland ~ "NEW-ENGLAND",
            .default = "TERRITORY"
      ))
      
      return(df)
}