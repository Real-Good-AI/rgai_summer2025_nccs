library(tibble)

na_counts_df <- function(df){
    n <- nrow(df)
    na_count <- sapply(df, function(y) sum(length(which(is.na(y))))) # get na_counts per column
    na_count <- data.frame(na_cnt = na_count)
    na_count <- na_count |>
        mutate(percent = na_cnt/n)
    na_count <- tibble::rownames_to_column(na_count, "var_name")
    return(na_count)
}