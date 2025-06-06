# Business Master File
* When examing missing values, made sure to do the following:
    - in character columns, replace empty strings with `NA`
    - if both latitute and longitude were equal to 0, replaced both columns with `NA`
      
* Any records with an Employee Identification Number (EIN) of 0 were dropped. These could have been replaced with `NA` instead but since there were only about 16 of them, and since the EIN is the merge key, we figured it was okay to drop.
* During cleaning, a subset of variables were user-specified as relevant to the problem--all other variables were dropped. 
    - We kept the following: EIN2, NTEEV2, F990_TOTAL_ASSETS_RECENT, CENSUS_STATE_ABBR, CENSUS_COUNTY_NAME, ORG_YEAR_FIRST, ORG_YEAR_LAST, LATITUDE, LONGITUDE, CENSUS_CBSA_FIPS
* Whenever there were multiple rows with the same EIN (note variable name is `EIN2`), were sorted by number of missing values such that we would keep the row corresponding to the fewest missing values. Note that if all rows corresponding to a single EIN had the same number of missing values, one was arbitrarily chosen. In particular, the first one in the ordering was kept.
* Any rows with more than 5 missing values were dropped.

# CORE Files
So far have only used this with 501(c)(3) PZ files!!!

* We started by dropping any columns corresponding to variables we were not planning to use for the analysis. 
    - We kept the following: EIN2, F9_00_TAX_PERIOD_END_DATE, F9_10_ASSET_TOT_BOY, F9_10_ASSET_TOT_EOY, F9_01_REV_TOT_CY, F9_08_REV_TOT_TOT, F9_00_EXEMPT_STAT_501C3_X, TAX_YEAR
* When examing missing values, made sure to do the following: 
    - in character columns, replace empty strings with `NA`
    - in numeric columns, negative numbers were replaced with `NA`

* If there were any rows that were exact duplicates of each other, we only kept one of them
* In each core file, there were instances of multiple (2-3) rows with the same EIN (the variable name is `EIN2`), we ordered them by F9_00_TAX_PERIOD_END_DATE and kept the row with the latest date. The reasoning is that many repeated EIN2 have a discrepancy in this column, should take the one filed with latest date because it was probably a correction! If have the same date, one is chosen arbitrarily (due to the "tie"). 
    - Note: It's possible that an organization would file two tax returns in one year but not as a correction. For example, if an orgnization gets an extension one year, the following year they might file two returns, one for each year (the current and the one they got an extension for). Not too worried about this because there are so many records overall.
    - Afterwards, the column corresponding to the variable F9_00_TAX_PERIOD_END_DATE was dropped since we no longer were planning to use it.

* Created a column called `TOT_REV` for total revenue based on the two revenue variables F9_01_REV_TOT_CY and F9_08_REV_TOT_TOT.
    - if both revenue variables exist in that dataset, choose max value for each row to save in TOT_REV
    - If only one of the revenue variables is in the dataset, rename that variable TOT_REV
* LEFT OFF: ORG_TYPE!

