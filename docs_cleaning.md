On this page, I walk through the data cleaning process I followed.

# Business Master File
* When examing missing values, made sure to do the following:
    - in character columns, replace empty strings with `NA`
    - if both latitute and longitude were equal to 0, replaced both columns with `NA`
      
* Any records with an Employee Identification Number (EIN) of 0 were dropped. These could have been replaced with `NA` instead but since there were only about 16 of them, and since the EIN is the merge key, we figured it was okay to drop.
* During cleaning, a subset of variables were user-specified as relevant to the problem--all other variables were dropped. 
    - We kept the following: EIN2, NTEEV2, F990_TOTAL_ASSETS_RECENT, CENSUS_STATE_ABBR, CENSUS_COUNTY_NAME, ORG_YEAR_FIRST, ORG_YEAR_LAST, LATITUDE, LONGITUDE, CENSUS_CBSA_FIPS
* Whenever there were multiple rows with the same EIN (note variable name is `EIN2`), were sorted by number of missing values such that we would keep the row corresponding to the fewest missing values. Note that if all rows corresponding to a single EIN had the same number of missing values, one was arbitrarily chosen. In particular, the first one in the ordering was kept.
* Any rows with more than 5 missing values were dropped.
* In the CORE folder, the file BMF_NA_counts.csv shows the different variables in the unprocessed Unified BMF file along with counts and proportion of the missing values in those variables.

# CORE Files
So far have only used this with 501(c)(3) PZ files!!!

* We started by dropping any columns corresponding to variables we were not planning to use for the analysis. 
    - We kept the following: EIN2, F9_00_TAX_PERIOD_END_DATE, F9_10_ASSET_TOT_BOY, F9_10_ASSET_TOT_EOY, F9_01_REV_TOT_CY, F9_08_REV_TOT_TOT, F9_00_EXEMPT_STAT_501C3_X, TAX_YEAR
* When examining missing values, made sure to do the following: 
    - in character columns, replace empty strings with `NA`
    - in numeric columns, negative numbers were replaced with `NA`

* If there were any rows that were exact duplicates of each other, we only kept one of them
* In each core file for a particular year, there were instances of multiple (2-3) rows with the same EIN (the variable name is `EIN2`), we ordered them by F9_00_TAX_PERIOD_END_DATE and kept the row with the latest date. The reasoning is that many repeated EIN2 have a discrepancy in this column, should take the one filed with latest date because it was probably a correction! If have the same date, one is chosen arbitrarily (due to the "tie"). 
    - Note: It's possible that an organization would file two tax returns in one year but not as a correction. For example, if an orgnization gets an extension one year, the following year they might file two returns, one for each year (the current and the one they got an extension for). Not too worried about this because there are so many records overall.
    - Afterwards, the column corresponding to the variable F9_00_TAX_PERIOD_END_DATE was dropped since we no longer were planning to use it.

* Created a column called `TOT_REV` for total revenue based on the two revenue variables F9_01_REV_TOT_CY and F9_08_REV_TOT_TOT.
    - if both revenue variables exist in that dataset, choose max value for each row to save in TOT_REV
    - If only one of the revenue variables is in the dataset, rename that variable TOT_REV
    
* Created a column called `ORG_TYPE` from the variable `F9_00_EXEMPT_STAT_501C3_X` which, according to the NCCS Data Dictionary, indicates a 501(c)(3) organization. This corresponds to what the organization would have input in the Form 990EZ, PART 0, SECTION J or full Form 990 PART 0, SECTION I.
    - If the organization type is -1 or 0, replace with NA (because must be mistake -- those are not valid 501c codes.)
    - Note: This variable confused me somewhat, because it was my understanding that since I was using 501(c)(3) PZ files, then either this variable is redundant, or leftover from a previous data split and therefore every record would the same value. Let me explain. If I'm looking at only 501(c)(3) data supposedly and there is a variable indicating whether the data came from a 501(c)(3) organization, then I would expect that column to be filled with "Yes" or "1" to indicate "Yes, this is a 501(c)(3)" but they are all, so there would only be one unique value in that column. Or perhaps the original variable had all the numbers corresponding to 501(c), e.g 501(c)(14) or something. Then maybe this column will still only contain 1 value and the value will be 3. None of this was the case. Numbers in this column were all over the place, numbers between 0 and ranging through to the 20's. I am still confused about this to be honest, and I ended up not using this variable in the analysis. 

* Since I wanted to have the same information for each organization each year, the next thing I did was merge all the years of data but only using columns (variables) that appeared in each year. 

* Last, I wanted to merge the CORE data with the cleaned BMF file to get the information that is in the BMF but not CORE.
    - In any numeric columns except LAT and LONG, convert negative values to NA since none of the remaining columns would make sense with negative numbers
    - rename F990_TOTAL_ASSETS_RECENT to SIZE -- this was to keep in line with the NCCS Dashboard; 
    - Created a factor version of size by splitting SIZE into categories -- also to keep in line with the categories on the NCCS Dashboard
    - I added in Census Regions and Divisions based on state (CENSUS_STATE_ABBR)
    
And finally, saved the final merged file! Yay!
    

