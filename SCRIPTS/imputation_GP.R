library(fields)
library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(mvtnorm)
library(pracma)
library(doParallel)

# Load data and remove outliers
df <- as.data.table(readRDS("PREPROCESSING/df_orginal_processed.rds")) |> filter(!outlier)

# Redo the means / centering to exlcude the outliers
df <- df |> mutate(LOG_REV.original = log(TOT_REV)) |> group_by(EIN2) |> mutate(LOG_REV.mean = mean(LOG_REV.original)) |> ungroup() |> mutate(LOG_REV = LOG_REV.original - LOG_REV.mean)

# After removing an outlier, some of the orgs' remaining data is the same each year and will thus need to be removed since LOG_REV=0 for every year in that time series
df <- left_join(df,
                df |> group_by(EIN2) |> summarize(all0 = all(LOG_REV == 0)), # For each org, get indicator of whether or not they reported same log_rev value each year
                by = "EIN2")

orgs.no.change <- df |> filter(all0 == TRUE) |> select(EIN2) |> unique()
orgs.no.change <- orgs.no.change$EIN2
#saveRDS(orgs.no.change, "PREPROCESSING/orgs_reported_same_after_outlier_removal.rds")

df <- df |> filter(all0 == FALSE)
setDT(df)

# Use a semi-join to only keep orgs we need to predict on
missing.years.list <- readRDS("PREPROCESSING/years_to_predict.rds") # named list; names correspond to orgs, element is list of years we need to impute for that org
missing.years.list <- missing.years.list[setdiff(names(missing.years.list), orgs.no.change)] # if an org appears in orgs.no.change, we'll take care of imputation later without the GP

df <- df[data.table(EIN2 = names(missing.years.list)), on = .(EIN2)]

# Get "best" hyperparameters for Matern Covariance for the organizations we need to impute data for
res <- readRDS("PREPROCESSING/best_parameters.rds")
all.orgs.gp <- intersect(unique(res$EIN), names(missing.years.list))

# distance matrix of years
dist_mat <- as.matrix(dist(sort(unique(df$YEAR)), diag=TRUE, upper=TRUE))

# normalize
dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))

# Predict
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time() # 1.5 hours for all orgs without imputing outliers, 2.4 hours with imputing outliers
res.predictions <- foreach(ein = all.orgs.gp, .packages = c("dplyr", "fields", "mvtnorm"), .verbose = FALSE) %dopar% {
      df.sub <- filter(df, EIN2==ein) # get subset of data for this organization
      years.to.predict <- missing.years.list[[ein]] # the years we will need to predict on
      years.original <- df.sub$YEAR + 1 # all the years we have data for that org
      
      my.list <- list(EIN = ein, 
                      TAX_YEAR = years.to.predict,
                      sig.sqd = numeric(), 
                      LOG_REV = numeric(length(years.to.predict)), 
                      standard_errors = matrix(0,ncol=length(years.to.predict),nrow=length(years.to.predict)))
      
      years.to.predict <- years.to.predict - 1989 + 1 # shift to match with YEAR variable instead of TAX_YEAR
      mu_1 <- numeric(length(years.to.predict)) # all zeros
      mu_2 <- numeric(nrow(df.sub)) # all zeros
      
      # Generate Matern covariance matrix using "best" hyperparameters from res data.frame
      mat_cov <- Matern(d = dist_mat, 
                        smoothness = (res |> filter(EIN == ein))$nu, 
                        range = 1, 
                        phi = 1) + diag((res |> filter(EIN == ein))$nugget, dim(dist_mat)[1])
      
      sigma.squared <- (1/length(df.sub$LOG_REV)) * (df.sub$LOG_REV %*% chol2inv(chol(mat_cov[years.original, years.original])) %*% df.sub$LOG_REV)[1,1]
      mat_cov <- sigma.squared * mat_cov
      
      my.list[["sig.sqd"]] <- sigma.squared
      
      SIGMA.11 <- mat_cov[years.to.predict, years.to.predict]
      SIGMA.22.inv <- chol2inv(chol(mat_cov[years.original, years.original]))
      SIGMA.12 <- mat_cov[years.to.predict, years.original, drop = FALSE]
      
      my.list[["LOG_REV"]] <- (mu_1 + (SIGMA.12 %*% SIGMA.22.inv %*% (df.sub$LOG_REV - mu_2)))[,1] #mu_bar
      my.list[["standard_errors"]]<- sqrt((SIGMA.11 - (SIGMA.12 %*% SIGMA.22.inv %*% t(SIGMA.12))))
      
      my.list
}
print(Sys.time() - start.time)
stopCluster(cl = cluster)

names(res.predictions) <- all.orgs.gp
saveRDS(res.predictions, "PREPROCESSING/imputation_results.rds")