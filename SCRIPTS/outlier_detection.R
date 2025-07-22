library(fields)
library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(mvtnorm)
library(pracma)
library(doParallel)
source("SCRIPTS/outlier_detection_helper.R")

#############################################################################################
# Preparing data
#############################################################################################
df <- readRDS("MODEL/df.rds") |>
      group_by(EIN2) |>
      mutate(LOG_REV = LOG_REV - mean(LOG_REV)) |>
      ungroup()

df <- df |> filter(DATA_COUNT >= 5)

# distance matrix of years
dist_mat <- as.matrix(dist(sort(unique(df$YEAR)), diag=TRUE, upper=TRUE))

# normalize
dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))

# Get only run scheme on orgs that reported some different data 
orgs.no.change <- readRDS("MODEL/orgs_reported_same.rds")
all.orgs <- unique(df$EIN2) 
all.orgs <- setdiff(all.orgs, orgs.no.change) #1:100000, 100001:200000, 200001:length(all.orgs)

# Previous times: 3hrs, 3.5hrs, 11 hrs 
# df.1 <- df |> filter(EIN2 %in% all.orgs[1:100000])
# df.2 <- df |> filter(EIN2 %in% all.orgs[100001:200000])
df.3 <- df |> filter(EIN2 %in% all.orgs[200001:300000])
df.4 <- df |> filter(EIN2 %in% all.orgs[300001:400000])
df.5 <- df |> filter(EIN2 %in% all.orgs[400001:length(all.orgs)])

# So when I made "orgs_reported_same" I did not center the LOG_REV so I didn't catch all the orgs that reported the same number each year, just the orgs that reported 0 each year
# removes 1 org
# df.sub <- df.4 |> group_by(EIN2) |> summarize(all0 = all(LOG_REV == 0))
# df.4 <- left_join(df.4, df.sub, by = "EIN2")
# df.sub <- df.4 |> filter(all0 == TRUE)
# df.4 <- df.4 |> filter(all0 == FALSE)

# Removes 29 orgs
# df.sub <- df.5 |> group_by(EIN2) |> summarize(all0 = all(LOG_REV == 0))
# df.5 <- left_join(df.5, df.sub, by = "EIN2")
# df.sub <- df.5 |> filter(all0 == TRUE)
# df.5 <- df.5 |> filter(all0 == FALSE)

# Matern covariance matrix hyperparameters
rho <- 1
initial.conds <- readRDS("MODEL/outlier_detection/centroids5_96orgs.rds")$centers # nu and nugget

#rm(df)

#############################################################################################
# Hyperparameter Optimization
#############################################################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)
# 1: 2.65 hours, 2: 3.15 hours, 3: 3.65 hours, 
start.time <- Sys.time() 
res <- foreach(ein = unique(df.4$EIN2), .combine = 'rbind', .packages = c("dplyr", "fields", "mvtnorm"), .verbose = FALSE) %dopar% {
      df.sub <- filter(df.4, EIN2==ein)
      tst <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("EIN", "nu", "nugget", "likelihood", "trial"))
      likelihood_wrapper <- function(pars.vec){
            return(-1 * get_likelihood_sigma(row = pars.vec,
                                             dist_mat = dist_mat,
                                             rho = rho,
                                             all_years = df.sub$YEAR + 1,
                                             data = df.sub$LOG_REV))
      }
      
      for (i in 1:nrow(initial.conds)){
            res <- constrOptim(theta = initial.conds[i,], 
                               f = likelihood_wrapper,
                               grad = NULL,
                               ui = rbind(c(1,0),c(-1,0),c(0,1)), #rbind(c(1,0),c(-1,0),c(0,1))
                               ci = c(0.001,-3.5,0)) #c(0.001,-3.5, 0)
            tst[nrow(tst)+1,1] <- ein
            tst[nrow(tst),2:ncol(tst)] <- c(c(res$par[1], res$par[2], -1*res$value, i))
      }
      tst[which.max(tst$likelihood),]
}
timeDiff <- Sys.time() - start.time
print(timeDiff)
cat("\n",paste("outlier_results_4.rds", timeDiff, attr(timeDiff, "units")), file = "PREPROCESSING/times.txt", append = TRUE)

stopCluster(cl = cluster)
saveRDS(res, "PREPROCESSING/outlier_results_4.rds")

rm(res, df.4)
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time() # 
res <- foreach(ein = unique(df.5$EIN2), .combine = 'rbind', .packages = c("dplyr", "fields", "mvtnorm"), .verbose = FALSE) %dopar% {
      df.sub <- filter(df.5, EIN2==ein)
      tst <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("EIN", "nu", "nugget", "likelihood", "trial"))
      likelihood_wrapper <- function(pars.vec){
            return(-1 * get_likelihood_sigma(row = pars.vec,
                                             dist_mat = dist_mat,
                                             rho = rho,
                                             all_years = df.sub$YEAR + 1,
                                             data = df.sub$LOG_REV))
      }
      
      for (i in 1:nrow(initial.conds)){
            res <- constrOptim(theta = initial.conds[i,], 
                               f = likelihood_wrapper,
                               grad = NULL,
                               ui = rbind(c(1,0),c(-1,0),c(0,1)), #rbind(c(1,0),c(-1,0),c(0,1))
                               ci = c(0.001,-3.5,0)) #c(0.001,-3.5, 0)
            tst[nrow(tst)+1,1] <- ein
            tst[nrow(tst),2:ncol(tst)] <- c(c(res$par[1], res$par[2], -1*res$value, i))
      }
      tst[which.max(tst$likelihood),]
}
timeDiff <- Sys.time() - start.time
print(timeDiff)
cat("\n",paste("outlier_results_5.rds", timeDiff, attr(timeDiff, "units")), file = "PREPROCESSING/times.txt", append = TRUE)

stopCluster(cl = cluster)
saveRDS(res, "PREPROCESSING/outlier_results_5.rds")


#############################################################################################
# Outlier Detection
#############################################################################################
# df <- df |> filter(EIN2 %in% all.orgs[1:100000])
# res <- readRDS("MODEL/outlier_detection/full_results_1.rds")

# df <- df |> filter(EIN2 %in% all.orgs[100001:200000])
# res <- readRDS("MODEL/outlier_detection/full_results_2.rds")
# 
# df <- df |> filter(EIN2 %in% all.orgs[200001:length(all.orgs)])
# res <- readRDS("MODEL/outlier_detection/full_results_3.rds")

# Looping over all organizations; 100K took 1.5 hrs w/out parallel, 18min w/ parallel; 42min for #3  
# 26 min, 26 min, 20 min, 15 min, 11 min
cluster <- makeCluster(7)
registerDoParallel(cluster)

mu_1 <- 0
start.time <- Sys.time() 
res.outliers <- foreach(ein = all.orgs[200001:length(all.orgs)], .packages = c("dplyr", "fields", "mvtnorm"), .verbose = TRUE) %dopar% {
      df.sub <- filter(df, EIN2==ein)
      all_years <- df.sub$YEAR + 1 # all the years we have data for that org
      
      my.list <- list(EIN = ein, sigs = numeric(length(all_years)), predictions = numeric(length(all_years)), standard_errors = numeric(length(all_years)), candidates = numeric(0))
      mu_2 <- numeric(nrow(df.sub) - 1) # all zeros
      
      for(i in 1:length(all_years)){
            # Leave out i-th year
            log_revenue <- (df.sub |> filter(YEAR != all_years[i]-1))$LOG_REV
            
            # Generate Matern covariance matrix using "best" hyperparameters
            mat_cov <- Matern(d = dist_mat, 
                              smoothness = (res |> filter(EIN == ein))$nu, 
                              range = rho, 
                              phi = 1) + diag((res |> filter(EIN == ein))$nugget, dim(dist_mat)[1])
            
            sigma.squared <- (1/length(log_revenue)) * (log_revenue %*% chol2inv(chol(mat_cov[all_years[-i], all_years[-i]])) %*% log_revenue)[1,1]
            mat_cov <- sigma.squared * mat_cov
            
            my.list[["sigs"]][i] <- sigma.squared
            
            SIGMA.11 <- mat_cov[all_years[i], all_years[i]]
            SIGMA.22.inv <- chol2inv(chol(mat_cov[all_years[-i], all_years[-i]]))
            SIGMA.12 <- mat_cov[all_years[i], all_years[-i], drop = FALSE]
            
            my.list[["predictions"]][i] <- mu_1 + (SIGMA.12 %*% SIGMA.22.inv %*% (log_revenue - mu_2)) #mu_bar
            my.list[["standard_errors"]][i] <- sqrt(as.numeric(SIGMA.11 - (SIGMA.12 %*% SIGMA.22.inv %*% t(SIGMA.12))))
      }
      
      residuals <- abs(df.sub$LOG_REV - my.list[["predictions"]]) / my.list[["standard_errors"]] # standardized residuals
      my.list[["candidates"]] <-  all_years[which(residuals > 3)] - 1 + 1989
      my.list
}
print(Sys.time() - start.time)
stopCluster(cl = cluster)

#############################################################################################
# Save Data
#############################################################################################
saveRDS(res.outliers, "MODEL/outlier_detection/outlier_res_3.rds")
# saveRDS(all_candidates, "MODEL/outlier_detection/full_candidates_1.rds")
# saveRDS(all_predictions, "MODEL/outlier_detection/full_predictions_1.rds")
# saveRDS(all_se, "MODEL/outlier_detection/full_se_1.rds")
# saveRDS(all.sigma.squared, "MODEL/outlier_detection/full_sigmas_1.rds")

#############################################################################################
# Save Data
#############################################################################################
