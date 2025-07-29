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

# Read in data, remove any organizations with an unknown NTEE category
df <- readRDS("PREPROCESSING/processed_mega_df.rds") |> filter(NTEE != "UNU")

# Index the original data for each org
data.per.org <- df |> 
      filter(IMPUTE_STATUS == "original") |>
      group_by(EIN2) |>
      mutate(ROW_NUM = row_number(), NUM_ORIGINAL = n()) |>
      ungroup() |>
      select(EIN2, TAX_YEAR, ROW_NUM, NUM_ORIGINAL)

df <- left_join(df, data.per.org |> select(EIN2, TAX_YEAR, ROW_NUM), by = c("EIN2", "TAX_YEAR"))
df <- left_join(df, data.per.org |> select(EIN2, NUM_ORIGINAL) |> distinct(), by = "EIN2")

rm(data.per.org) # remove variable since we no longer need it

# Remove any orgs with less than 6 years of data
df <- df |> filter(NUM_ORIGINAL >= 6)

all.orgs.data <- readRDS("MODEL/nearest_neighbors/all_orgs_data.rds")

eins <- sample(unique(all.orgs.data$EIN2), 1000)

####################################################
# Get K nearest neighbors for each organization
####################################################
start.year <- 1989 # first year in the entire data set
end.year <- 2021 # last year in the entire data set
K = 6

n.data = 3
n.predict = 3

categories <- unique(all.orgs.data$NTEE)
all.comparison.orgs <- setNames(data.table(matrix(ncol = 12, nrow = 0)), 
                                c("EIN2", "TAX_YEAR", "TOT_REV", "NTEE", "IMPUTE_STATUS", "NUM_ORIGINAL", "START_YEAR", "END_PLUS", "ORIGINAL_ORG", "END_YEAR", "DISTANCE", "NEIGHBOR_ID"))

start.time <- Sys.time() # ~ 5 min
res <- foreach(category = categories, .combine = 'rbind', .packages = c("dplyr", "data.table", "RANN"), .verbose = TRUE) %do% {
      # STEP 1: get all the user.data for each org in current category
      user.data <- all.orgs.data |> 
            filter(NTEE == category) |>
            filter(!PREDICT) |> 
            group_by(EIN2) |>
            mutate(YEAR = row_number()) |>
            ungroup() |>
            select(EIN2, YEAR, TOT_REV) 
      
      setDT(user.data)
      user.data <- user.data |> dcast(EIN2 ~ YEAR, value.var = "TOT_REV") # Reshape to wide format with years as columns
      
      # STEP 2: Get all data from this category
      df.search <- df |> filter(NTEE == category)
      
      # STEP 3: Nearest Neighbor search (55s)
      nearest.neighbors <- setNames(data.table(matrix(ncol = 5, nrow = 0)), c("EIN2", "START_YEAR", "END_YEAR", "DISTANCE", "ORIGINAL_ORG")) # empty data frame that we'll fill with nearest neighbors
      for (year in seq(start.year, end.year-n.predict-n.data+1, 1)){
            curr.interval <- year:(year+n.data-1) # Time interval to be searched for K nearest neighbors
            
            # Get the data to search through and filter out any orgs with too much imputed data in the current n.data+n.predict interval
            df.search.sub <- df.search |>
                  filter(TAX_YEAR %in% year:(year+n.data-1+n.predict)) |> 
                  mutate(IMPUTED = (IMPUTE_STATUS != "original")) |>
                  select(EIN2, TAX_YEAR, TOT_REV, IMPUTED) |>
                  group_by(EIN2) |>
                  mutate(NUM_IMPUTED = sum(IMPUTED), YEAR = row_number()) |>
                  ungroup()
            
            df.search.sub <- df.search.sub |> filter(NUM_IMPUTED <= 2) |> filter(TAX_YEAR %in% curr.interval)
            
            # Reshape to wide format where columns are years
            setDT(df.search.sub)
            df.search.sub <- df.search.sub |> select(EIN2, YEAR, TOT_REV) |> dcast(EIN2 ~ YEAR, value.var = "TOT_REV")
            
            if (curr.interval[length(curr.interval)] > end.year-n.predict){break} # If the interval contains years it shouldn't, exit for loop
            
            # Get K nearest neighbors for this time window
            nearest <- nn2(df.search.sub |> select(-EIN2), user.data |> select(-EIN2), k=K) 
            nearest.neighbors <- rbind(nearest.neighbors, as.data.table(list("EIN2" = df.search.sub[as.vector(nearest$nn.idx),]$EIN2,
                                                                             "START_YEAR" = curr.interval[1],
                                                                             "END_YEAR" = curr.interval[length(curr.interval)],
                                                                             "DISTANCE" = as.vector(nearest$nn.dists),
                                                                             "ORIGINAL_ORG" = rep(user.data$EIN2, 6))))
      }
      
      # start.time <- Sys.time() # 30 s
      # STEP 4: Get the top 5 neighbors for each org
      nearest.neighbors <- nearest.neighbors |> 
            arrange(ORIGINAL_ORG, DISTANCE) |> # Within each organization, order by distance
            filter(!(ORIGINAL_ORG==EIN2 & DISTANCE == 0)) # since one of the matches is always exactly itself, remove it
      
      nearest.neighbors <- nearest.neighbors |> 
            group_by(ORIGINAL_ORG) |>
            group_modify(~{
                  subset <- head(.x,5) # Keep the top 5 rows in each group (as those correspond to closest neighbors)
                  return(subset)
            }) |>
            ungroup()
      
      # Give each matched neighbor an ID number
      nearest.neighbors <- nearest.neighbors |> group_by(ORIGINAL_ORG) |> mutate(NEIGHBOR_ID = row_number()) |> ungroup()
      
      # Create an END_YEAR + n.predict column to help with ranges
      nearest.neighbors <- nearest.neighbors |> mutate(END_PLUS = END_YEAR + n.predict)
      
      # STEP 5: Get the full data for each nearest neighbor
      
      # Perform a non-equi join, where rows from nearest.neighbors are matched with rows in comparison.orgs based on conditions
      df.search <- df.search |> mutate(TAX_YEAR_COMP = TAX_YEAR) # Rename TAX_YEAR before joining to avoid name conflict
      df.search <- df.search |> select(EIN2, TAX_YEAR, TOT_REV, NTEE, IMPUTE_STATUS, NUM_ORIGINAL, TAX_YEAR_COMP)
      setkey(df.search, EIN2, TAX_YEAR_COMP) # Set keys for the join
      comparison.orgs <- df.search[nearest.neighbors,
                                   on = .(EIN2, TAX_YEAR_COMP >= START_YEAR, TAX_YEAR_COMP <= END_PLUS),
                                   allow.cartesian = TRUE,
                                   nomatch = 0] # For each org in nearest.neighbors, give me the rows for that org from comparison.orgs corresponding to the TAX_YEAR values from START_YEAR through END_PLUS
      comparison.orgs <- comparison.orgs |> rename(START_YEAR = TAX_YEAR_COMP, END_PLUS = TAX_YEAR_COMP.1)
      
      all.comparison.orgs <- rbind(all.comparison.orgs, comparison.orgs)
}
print(Sys.time()-start.time)

saveRDS(all.comparison.orgs, "MODEL/nearest_neighbors/res_nn.rds")

res = all.comparison.orgs
rm(df, all.comparison.orgs, comparison.orgs, df.search, df.search.sub, nearest, nearest.neighbors, user.data)
####################################################
# Hyperparamter Optimization
####################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time() # 16 hours
res.pars <- foreach(ein = unique(all.orgs.data$EIN2), .combine = 'rbind', .packages = c("dplyr", "fields", "mvtnorm", "lhs"), .verbose = FALSE, .errorhandling = "remove") %dopar% {
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
      initial.conds[, 2] <- 0.05 + initial.conds[, 2] * (3 - 0.05) # nugget in [0.05,3]
      
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
                                   ci = c(0.001,-3.5,0.001)) #c(0.001,-3.5, 0)
            tst[nrow(tst)+1,1] <- ein
            tst[nrow(tst),2:ncol(tst)] <- c(c(res.opt$par[1], res.opt$par[2], -1*res.opt$value, i))
      }
      tst[which.max(tst$likelihood),]
}

print(Sys.time()-start.time)
stopCluster(cl = cluster)

saveRDS(res.pars, "MODEL/nearest_neighbors/res_parameters.rds")

####################################################
# GP Predictions
####################################################
cluster <- makeCluster(7)
registerDoParallel(cluster)

start.time <- Sys.time() # 5-6 hours?
res.predictions <- foreach(ein = unique(res.pars$EIN2), .packages = c("dplyr", "fields", "mvtnorm"), .verbose = FALSE) %dopar% {
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
                       MEAN = mean(df.nn$TOT_REV),
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

names(res.predictions) <- unique(res.pars$EIN2)
saveRDS(res.predictions, "MODEL/nearest_neighbors/res_predictions.rds")