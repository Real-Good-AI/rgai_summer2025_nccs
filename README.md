# Nonprofit Revenue Prediction Tool

Note: Data is from open source NCCS Core Series Data: [website link](https://nccs.urban.org/nccs/datasets/core/) 
Citation: Jesse Lecy, (2024). NCCS Core Series.

## Main Directory
* app.R : the code for the UI for our revenue prediction tool RShiny App
* app_helper.R : helpder code for the app 
* demo.Rmd: walks you through downloading data, cleaning it, and making some plots.
* docs_cleaning.md: walks you through out data cleaning steps and reasoning
* gp_outlier_detection.Rmd: results from the outlier detection
* preprocessing.Rmd: walks you through the data cleaning and preprocessing steps that we took from start to finish
* modeling.Rmd: walks you through out modeling

## Folders

CORE : this is the directory where you would save files downloaded directly from the NCCS Core Series Dataset
      - BMF_NA_counts.csv contains the counts/proportions of missing values for variables in the Unified BMF file (pre-cleaning)
      
CLEAN : this is the directory where you would save cleaned CORE files
      - cleanBMF_NA_counts.csv contains the counts/proportions of missing values for variables in the Unified BMF file (post-cleaning)
      
PREPROCESSING : this is the directory where you would save files related to the post-cleaning preprocessing step
      - processed_mega_df_app.rds contains the data frame object used in the app
      
SCRIPTS: Helper functions used throughout the repo
      - clean_BMF.R: contains one function called `clean_BMF()` that cleans the unified BMF file downloaded from [NCCS BMF](https://nccs.urban.org/nccs/datasets/bmf/) according to some specified parameters (refer to docs_cleaning.md for more info on our specific choices)
      - clean_CORE.R: contains one function called `clean_CORE()` that cleans the CORE files downloaded from [NCCS CORE](https://nccs.urban.org/nccs/catalogs/catalog-core.html) according to some specified parameters (refer to docs_cleaning.md for more info on our specific choices)
      - clean_helper.R:
            - `na_counts_df()`: takes in a dataframe df, then returns a dataframe with three columns: the first contains all the column names of the input df, the second is the count of NA values for that column in df, the third is proportion
            - `add_regions_and_divisions()`: For merging the CORE and BMF data sets while adding in US CENSUS regions and divisions based on states ([source](https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf))
      - download_data.R
            - `downloadCORE()`: download CORE files from NCCS site
            - `download_dicts()`: download data dictionaries from NCCS site
            - `download_bmf()`: download Unified BMF file from NCCS site
      - imputation_GP.R : This is the script used to impute missing values in the mega dataset
            - See preprocessing.Rmd Section "Preprocessing", subsection "Creating "Full" Dataset" for more details
            - The main idea was that we wanted to create a dataset that had records for each year from 1989 to 2021 for EACH organization. Some organizations just didn't exist that entire timeframe and so the beginning and end years are padded with 0's. But still, there were times where an organization for example had data from 2012-2021 but the year 2016 was missing, so we imputed the value based on a Gaussian Process trained on its remaining data. We also imputed revenue values for years that were previously identified as outliers. 
      - model_LOO_eval.R : this is the script we used to do a "leave one out" style evaluation of our nearest neighbor-based prediction model.
            - For each organization with at least 6 years of reported data, we picked a contiguous subset of 6 years of time series revenue data, using the first three years as our hypothetical "user input" to the app, and the last three years were what we "predicted" but since we had the true value we were able to evaluate the precision and accuracy of those predictions
            - We had an $R^2$ value of roughly 0.96 and the true values were contained in the corresponding 95% confidence interval about 99% of the time (showing our confidence intervals are conservative)
      - outlier_detection.R : This is the script we used for outlier detection!
            - For each organization, we train a GP on it's timeseries revenue data leave-one-out style: for example, if an organization had data for 2010-2020 (ten years), we trained 10 different GP's, each time leaving out one year of data and only training the model on the rest.
            - Then we used the model to predict the data for the missing year, and if the standardized residuals were greater than 3, we labeled that year as an outlier.
      - outlier_detection_helper.R : Contains helper functions for the outlier detection scheme, mainly helper functions related to computing likelihoods (for hyperparameter optimization) and for plotting results
      - plotting_helper.R: one additional plotting function
      - reviewing_duplicate_EIN.R: this contains two helpder functions for the data cleaning stage, related to identifying and analyzing duplicated records (in particular, there were times when there were multiple records for an EIN in the BMF file or for one year, so we had to resolve those duplicates)