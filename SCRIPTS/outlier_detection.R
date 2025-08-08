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
df <- readRDS("PREPROCESSING/df.rds") |>
      group_by(EIN2) |>
      mutate(LOG_REV = LOG_REV - mean(LOG_REV)) |>
      ungroup()

df <- df |> filter(DATA_COUNT >= 5)

# distance matrix of years
dist_mat <- as.matrix(dist(sort(unique(df$YEAR)), diag=TRUE, upper=TRUE))

# normalize
dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))

# Get only run scheme on orgs that reported some different data 
orgs.no.change <- readRDS("PREPROCESSING/orgs_reported_same.rds") # Refer to preprocessing.Rmd section "Finding Orgs that reported same log_rev every year"
all.orgs <- unique(df$EIN2) 
all.orgs <- setdiff(all.orgs, orgs.no.change)

# The reason I split these is that I knew the code would take a long time to run but I was worries about running into memory issues and I didn't want it to crash 20 hours after running due to space. 
# It's entirely possible this was completely unecessary. I ran this script in 5 separate pieces, each time uncommenting the corresponding line below. 
df.1 <- df |> filter(EIN2 %in% all.orgs[1:100000])
# df.2 <- df |> filter(EIN2 %in% all.orgs[100001:200000])
# df.3 <- df |> filter(EIN2 %in% all.orgs[200001:300000])
# df.4 <- df |> filter(EIN2 %in% all.orgs[300001:400000])
# df.5 <- df |> filter(EIN2 %in% all.orgs[400001:length(all.orgs)])

# Matern covariance matrix hyperparameters
rho <- 1
initial.conds <- readRDS("PREPROCESSING/outlier_detection/centroids5_96orgs.rds")$centers # nu and nugget

rm(df)

#############################################################################################
# Hyperparameter Optimization
#############################################################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time() 

# IMPORTANT: You need to update the name of the df subset you are using in the first argument of foreach() and in the very next line, first argment of filter()
# Also note you have to update the save filenames after the foreach loop
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

# MAKE SURE THIS IS THE RIGHT NUMBER e.g. if df.4 then outlier_results_4.rds
cat("\n",paste("outlier_results_4.rds", timeDiff, attr(timeDiff, "units")), file = "PREPROCESSING/times.txt", append = TRUE)

stopCluster(cl = cluster)
# MAKE SURE THIS IS THE RIGHT NUMBER e.g. if df.4 then outlier_results_4.rds
saveRDS(res, "PREPROCESSING/outlier_results_4.rds")


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

# IMPORTANT: You need to update the first argument of foreach() to correspond to the right results from the hyperparameter optimization step above. 
res.outliers <- foreach(ein = all.orgs[200001:length(all.orgs)], .packages = c("dplyr", "fields", "mvtnorm"), .verbose = TRUE) %dopar% {
      df.sub <- filter(df, EIN2==ein)
      all_years <- df.sub$YEAR + 1 # all the years we have data for that org
      
      my.list <- list(EIN = ein, sigs = numeric(length(all_years)), predictions = numeric(length(all_years)), standard_errors = numeric(length(all_years)), candidates = numeric(0), residuals = numeric(0))
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
      
      my.residuals <- abs(df.sub$LOG_REV - my.list[["predictions"]]) / my.list[["standard_errors"]] # standardized residuals
      my.list[["residuals"]] <- my.residuals
      my.list[["candidates"]] <-  all_years[which(my.residuals > 3)] - 1 + 1989
      my.list
}
print(Sys.time() - start.time)
stopCluster(cl = cluster)

saveRDS(res.outliers, "PREPROCESSING/gp_res_4.rds") # MAKE SURE THIS IS THE RIGHT NUMBER e.g. if df.4 then gp_res_4.rds
