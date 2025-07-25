library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(RANN)
library(fields)
library(ggplot2)
library(cowplot)
library(lhs) # for latin hypercube sampling
library(doParallel)
source("SCRIPTS/outlier_detection_helper.R")
rm(plot_surface, plot_true_vs_pred, plot_heatmap, get_likelihood_bayesopt)

####################################################
# Load data and do a little bit of pre-processing
####################################################
df <- readRDS("PREPROCESSING/processed_mega_df.rds") |> filter(NTEE != "UNU") # UNU means unknown

# Add in the first and last year we had original data for each org
org_bounds <- df |>
      filter(IMPUTE_STATUS != "imputed") |>
      group_by(EIN2) |>
      summarize(FIRST_APPEARED = min(TAX_YEAR),
                LAST_APPEARED = max(TAX_YEAR),
                .groups = "drop")

df <- left_join(df, org_bounds, by = "EIN2")

rm(org_bounds) # remove variable since we no longer need it

# Index the original data for each org
row.numbers <- df |> 
      filter(TAX_YEAR >= FIRST_APPEARED & TAX_YEAR <= LAST_APPEARED & IMPUTE_STATUS == "original") |>
      group_by(EIN2) |>
      mutate(NUM_ORIGINAL = n()) |>
      ungroup() |>
      select(EIN2, TAX_YEAR, NUM_ORIGINAL)

df <- left_join(df, row.numbers, by = c("EIN2", "TAX_YEAR"))

rm(row.numbers) # remove variable since we no longer need it

all.orgs.data <- readRDS("MODEL/nearest_neighbors/all_orgs_data.rds")
orgs.no.change <- readRDS("PREPROCESSING/orgs_reported_same.rds")

all.orgs <- setdiff(unique(df$EIN2), orgs.no.change) # we should not proceed for any orgs that reported the same number each year, since no need to predict
eins <- sample(all.orgs, 300) # small test set

####################################################
# Get K nearest neighbors for each organization
####################################################
start.year <- 1989 # first year in the entire data set
end.year <- 2021 # last year in the entire data set
K <- 5 # number of nearest neighbors to search for in each window

cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time()
res <- foreach(ein = eins, .combine = 'rbind', .packages = c("dplyr", "data.table", "RANN"), .verbose = FALSE) %dopar% { 
      # STEP 1: Get this organization's history + info 
      user.data <- all.orgs.data |> filter(EIN2 == ein)
      n.predict <- sum(user.data$PREDICT)
      n.data <- nrow(user.data) - n.predict
      category <- unique(user.data$NTEE)
      
      # STEP 2: Turn user.data into usable format for next part (basically wide format where each year is a column and TOT_REV is the value in that column)
      user.data <- user.data |> 
            filter(!PREDICT) |> 
            select(EIN2, TAX_YEAR, TOT_REV) |> 
            dcast(EIN2 ~ TAX_YEAR, value.var = "TOT_REV") |>
            select(-EIN2)
      
      # STEP 3: Search for K nearest neighbors in each time window 
      df.search <- df |> filter(EIN2 != ein & NTEE == category) |> select(EIN2, TAX_YEAR, TOT_REV, IMPUTE_STATUS) # Data to search through
      
      nearest.neighbors <- setNames(data.table(matrix(ncol = 4, nrow = 0)), c("EIN2", "START_YEAR", "END_YEAR", "DISTANCE")) # empty data frame that we'll fill with nearest neighbors
      
      for (year in seq(start.year, end.year-n.predict-n.data+1, 1)){
            curr.interval <- year:(year+n.data-1) # Time interval to be searched for K nearest neighbors
            if (curr.interval[length(curr.interval)] > end.year-n.predict){break} # If the interval contains years it shouldn't, exit for loop
            
            # Get the data corresponding to this time interval, reshape to wide format so there is a different column per year
            df.search.sub <- df.search |> filter(TAX_YEAR %in% curr.interval) |> select(EIN2, TAX_YEAR, TOT_REV) |> dcast(EIN2 ~ TAX_YEAR, value.var = "TOT_REV")
            
            # Get K nearest neighbors for this time window
            nearest <- nn2(df.search.sub |> select(-EIN2), user.data, k=K) 
            nearest.neighbors <- rbind(nearest.neighbors, as.data.table(list("EIN2" = df.search.sub[as.vector(nearest$nn.idx),]$EIN2,
                                                                             "START_YEAR" = curr.interval[1],
                                                                             "END_YEAR" = curr.interval[length(curr.interval)],
                                                                             "DISTANCE" = as.vector(nearest$nn.dists))))
      }
      
      # STEP 4: Get the top 5 neighbors for this org
      
      # Create an END_YEAR + n.predict column to help with ranges
      nearest.neighbors[, END_PLUS := END_YEAR + n.predict]
      
      # Get the data for the orgs in nearest.neighbors
      comparison.orgs <- df.search |> filter(EIN2 %in% nearest.neighbors$EIN2) # full data for nearest neighbor orgs
      
      # Perform a non-equi join, where rows from nearest.neighbors are matched with rows in comparison.orgs based on conditions
      comparison.orgs <- comparison.orgs |> mutate(TAX_YEAR_COMP = TAX_YEAR) # Rename TAX_YEAR in comparison.orgs before joining to avoid name conflict
      setkey(comparison.orgs, EIN2, TAX_YEAR_COMP) # Set keys for the join
      comparison.orgs <- comparison.orgs[nearest.neighbors,
                                         on = .(EIN2, TAX_YEAR_COMP >= START_YEAR, TAX_YEAR_COMP <= END_PLUS),
                                         allow.cartesian = TRUE,
                                         nomatch = 0] # For each org in nearest.neighbors, give me the rows for that org from comparison.orgs corresponding to the TAX_YEAR values from START_YEAR through END_PLUS
      comparison.orgs <- comparison.orgs |> rename(START_YEAR = TAX_YEAR_COMP, END_PLUS = TAX_YEAR_COMP.1)
      
      # Compute the proportion/number of imputed data points for each row in nearest.neighbors
      comparison.orgs[, NON_ORIGINAL := IMPUTE_STATUS != "original"] # turn to boolean for easy counting of imputed values
      result <- comparison.orgs |> 
            group_by(EIN2, DISTANCE) |> 
            summarize(PROP_IMPUTED = mean(NON_ORIGINAL), NUM_IMPUTED = sum(NON_ORIGINAL))
      
      # Merge back with nearest.neighbors and filter out any that have too many imputed values
      # n.data + n.predict ranges between 3 and 8; if 3, allow no imputed values, if 4 or 5 or allow up to 1 imputed value, if 6-8 allow up to 2 imputed values
      comparison.orgs <- left_join(comparison.orgs, result, by = c("EIN2", "DISTANCE"))
      comparison.orgs <- comparison.orgs |> filter(PROP_IMPUTED <= 2/6)
      
      # Get top 5 neighbors' data
      # TODO: Case where top 5 includes original org
      # TODO: Case where there are repeated orgs in top 5
      comparison.orgs <- comparison.orgs[order(DISTANCE)]
      comparison.orgs <- comparison.orgs[1:(K*(n.data + n.predict))]
      
      # Give each matched neighbor an ID number
      comparison.orgs <- comparison.orgs |> 
            left_join(comparison.orgs |> group_by(DISTANCE, EIN2) |> summarise(GROUP_ID = cur_group_id()) |> ungroup(),
                      by = c("EIN2", "DISTANCE"))
      
      comparison.orgs$ORIGINAL_ORG <- ein
      
      # STEP 5: save this, for each should merge this data table with the ones from the other loops
      comparison.orgs
}
print(Sys.time()-start.time)
stopCluster(cl = cluster)

# saveRDS(res, "MODEL/nearest_neighbors/res_nn.rds")

####################################################
# Hyperparamter Optimization
####################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time()
res.pars <- foreach(ein = eins, .combine = 'rbind', .packages = c("dplyr", "fields", "mvtnorm", "lhs"), .verbose = FALSE, .errorhandling = "remove") %dopar% {
      # Neearest Neighbors Data, grouping by EIN2,DISTANCE in case EIN2 is repeated (if repeated, distance will be difference), adding shifted YEAR variable and centering the total revenue values
      df.nn <- res |> 
            filter(ORIGINAL_ORG == ein) |> 
            group_by(EIN2, DISTANCE) |> 
            mutate(YEAR = row_number()) |> 
            ungroup() |> 
            filter(IMPUTE_STATUS == "original") |> 
            mutate(TOT_REV.centered = TOT_REV - mean(TOT_REV)) 
      
      # Relevant data for current org
      user.data <- all.orgs.data |> filter(EIN2 == ein)
      n.predict <- sum(user.data$PREDICT)
      n.data <- nrow(user.data) - n.predict
      
      # Normalized distance matrix
      dist_mat <- rdist(c(1:n.predict +n.data, df.nn$YEAR)) # the n.predict years we need to predict on and the years we have nearest neighbor data for
      dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))
      
      initial.conds <- randomLHS(10,2)
      initial.conds[, 1] <- 0.05 + initial.conds[, 1] * (2.5 - 0.05) # nu in [0.05,2.5)
      initial.conds[, 2] <- 0 + initial.conds[, 2] * (3 - 0) # nugget in [0,3]
      
      tst <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("EIN2", "nu", "nugget", "likelihood", "trial"))
      likelihood_wrapper <- function(pars.vec){
            return(-1 * get_likelihood_sigma(row = pars.vec,
                                             dist_mat = dist_mat,
                                             rho = 1,
                                             all_years = (n.predict+1):dim(dist_mat)[1],
                                             data = df.nn$TOT_REV.centered))
      }
      
      # Gradient Descent with multiple starts
      for (i in 1:nrow(initial.conds)){
            res.opt <- constrOptim(theta = initial.conds[i,], 
                                   f = likelihood_wrapper,
                                   grad = NULL,
                                   ui = rbind(c(1,0),c(-1,0),c(0,1)), #rbind(c(1,0),c(-1,0),c(0,1))
                                   ci = c(0.001,-3.5,0)) #c(0.001,-3.5, 0)
            tst[nrow(tst)+1,1] <- ein
            tst[nrow(tst),2:ncol(tst)] <- c(c(res.opt$par[1], res.opt$par[2], -1*res.opt$value, i))
      }
      tst[which.max(tst$likelihood),]
}

print(Sys.time()-start.time)
stopCluster(cl = cluster)

# saveRDS(res.pars, "MODEL/nearest_neighbors/res_parameters.rds")

####################################################
# GP Predictions
####################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time()
res.predictions <- foreach(ein = eins, .packages = c("dplyr", "fields", "mvtnorm"), .verbose = FALSE, .errorhandling = "remove") %dopar% {
      # Nearest Neighbors data, grouping by EIN2,DISTANCE in case EIN2 is repeated (if repeated, distance will be difference), adding shifted YEAR variable and centering the total revenue values
      df.nn <- res |> 
            filter(ORIGINAL_ORG == ein) |> 
            group_by(EIN2, DISTANCE) |> 
            mutate(YEAR = row_number()) |> 
            ungroup() |> 
            filter(IMPUTE_STATUS == "original") |> 
            mutate(TOT_REV.centered = TOT_REV - mean(TOT_REV)) 
      
      # Get relevant data for this organization
      user.data <- all.orgs.data |> filter(EIN2 == ein)
      n.predict <- sum(user.data$PREDICT)
      n.data <- nrow(user.data) - n.predict
      
      # Normalized distance matrix
      dist_mat <- rdist(c(1:n.predict + n.data, df.nn$YEAR)) # the n.predict years we need to predict on and the years we have nearest neighbor data for
      dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))
      
      # Hyperparameters
      nu <- (res.pars |> filter(EIN2 == ein))$nu
      nugget <- (res.pars |> filter(EIN2 == ein))$nugget
      rho <- 1
      
      # For indexing the Matern covariance matrix
      years.to.predict <- 1:n.predict # indices (w.r.t dist_mat) of the years we need to predict on
      years.original <- (n.predict+1):dim(dist_mat)[1] # indices (w.r.t dist_mat) of the years we have nearest neighbor data for
      
      res.list <- list(EIN = ein, 
                       YEAR = years.to.predict + n.data,
                       sig.sqd = numeric(), 
                       TOT_REV = numeric(length(years.to.predict)), 
                       standard_errors = matrix(0,ncol=length(years.to.predict),nrow=length(years.to.predict)))
      
      mu_1 <- numeric(length(years.to.predict)) # all zeros
      mu_2 <- numeric(nrow(df.nn)) # all zeros
      
      # Generate Matern covariance matrix using "best" hyperparameters from res data.frame
      mat_cov <- Matern(d = dist_mat, 
                        smoothness = nu, 
                        range = rho, 
                        phi = 1) + diag(nugget, dim(dist_mat)[1])
      
      # Compute sigma^2 and scale covariance matrix
      data.nn <- df.nn$TOT_REV.centered
      
      sigma.squared <- (1/length(data.nn)) * (data.nn %*% chol2inv(chol(mat_cov[years.original,years.original])) %*% data.nn)[1,1]
      res.list[["sig.sqd"]] <- sigma.squared
      
      mat_cov <- sigma.squared * mat_cov
      
      # Conditional Distributions
      SIGMA.11 <- mat_cov[years.to.predict, years.to.predict]
      SIGMA.22.inv <- chol2inv(chol(mat_cov[years.original, years.original]))
      SIGMA.12 <- mat_cov[years.to.predict, years.original, drop = FALSE]
      
      res.list[["TOT_REV"]] <- (mu_1 + (SIGMA.12 %*% SIGMA.22.inv %*% (data.nn - mu_2)))[,1] 
      res.list[["standard_errors"]] <- diag(sqrt((SIGMA.11 - (SIGMA.12 %*% SIGMA.22.inv %*% t(SIGMA.12)))))
      
      res.list
}
print(Sys.time() - start.time)
stopCluster(cl = cluster)

names(res.predictions) <- eins
# saveRDS(res.predictions, "MODEL/nearest_neighbors/res_predictions.rds")