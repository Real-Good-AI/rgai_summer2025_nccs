library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(RANN)
library(fields)
library(mvtnorm)
library(ggplot2)
library(cowplot)
library(plotly)
library(scales)
library(lhs) # for latin hypercube sampling
library(memoise)

readMegaDF <- memoise(function() {
      message("Reading mega.df from disk…")
      readRDS("PREPROCESSING/processed_mega_df_app.rds")
})

nn.search <- function(category, user.EIN, user.years, user.history, n.predict){
      df <- df |> filter(NTEE == category) |> mutate(IMPUTED = (IMPUTE_STATUS != "original"))
      
      start.year <- min(df$TAX_YEAR) # first year in the data set
      end.year <-  max(df$TAX_YEAR) # last year in the data set
      
      # STEP 1: Set up user.data with shifted year variable
      n.data <- length(user.history)
      user.data <- as.data.table(list(EIN2 = user.EIN, TAX_YEAR = user.years, TOT_REV = user.history)) |>
            arrange(TAX_YEAR) |>
            mutate(YEAR = row_number()) |>
            select(EIN2, YEAR, TOT_REV) 
      
      setDT(user.data)
      user.data <- user.data |> dcast(EIN2 ~ YEAR, value.var = "TOT_REV") # Reshape to wide format with years as columns
      
      # Weed out any organizations whose revenues fall well outside the range of the current user's revenue
      user.min <- min(user.history)*0.5
      user.max <- max(user.history)*2
      df <- df |> select(EIN2, TAX_YEAR, TOT_REV, IMPUTED, ORG_NAME_CURRENT) |> 
            group_by(EIN2) |>
            mutate(MIN = min(TOT_REV) , MAX = max(TOT_REV)) |>
            mutate(KEEP = (MAX >= user.min & MIN <= user.max) ) |>
            ungroup() |>
            filter(KEEP) |> 
            select(EIN2, TAX_YEAR, TOT_REV, IMPUTED, ORG_NAME_CURRENT)
      
      # STEP 2: Nearest Neighbor search
      nearest.neighbors <- setNames(data.table(matrix(ncol = 4, nrow = 0)), c("EIN2", "START_YEAR", "END_YEAR", "DISTANCE")) # empty data frame that we'll fill with nearest neighbors
      for (year in seq(start.year, end.year-n.predict-n.data+1, 1)){
            curr.interval <- year:(year+n.data-1) # Time interval to be searched for K nearest neighbors
            
            # Get the data to search through and filter out any orgs with too much imputed data in the current n.data+n.predict interval
            df.search <- df |> select(-ORG_NAME_CURRENT) |> filter(TAX_YEAR %in% curr.interval)
            
            # Reshape to wide format where columns are years
            setDT(df.search)
            df.search <- df.search |> select(EIN2, TAX_YEAR, TOT_REV) |> dcast(EIN2 ~ TAX_YEAR, value.var = "TOT_REV")
            
            if (curr.interval[length(curr.interval)] > end.year-n.predict){break} # If the interval contains years it shouldn't, exit for loop
            
            # Get K nearest neighbors for this time window
            nearest <- nn2(df.search |> select(-EIN2), user.data |> select(-EIN2), k=7) 
            nearest.neighbors <- rbind(nearest.neighbors, as.data.table(list("EIN2" = df.search[as.vector(nearest$nn.idx),]$EIN2,
                                                                             "START_YEAR" = curr.interval[1],
                                                                             "END_YEAR" = curr.interval[length(curr.interval)],
                                                                             "DISTANCE" = as.vector(nearest$nn.dists))))
      }
      
      # STEP 3: If we matched with ourselves, filter that out; Create an END_YEAR + n.predict column to help with ranges
      nearest.neighbors <- nearest.neighbors |> filter(EIN2!=user.EIN) |> mutate(END_PLUS = END_YEAR + n.predict)
      
      # STEP 4: Get the full data for each nearest neighbor
      
      # Perform a non-equi join, where rows from nearest.neighbors are matched with rows in comparison.orgs based on conditions
      df <- df |> mutate(TAX_YEAR_COMP = TAX_YEAR) # Rename TAX_YEAR before joining to avoid name conflict
      df <- df |> select(EIN2, TAX_YEAR, TOT_REV, IMPUTED, TAX_YEAR_COMP, ORG_NAME_CURRENT)
      setDT(df)
      setkey(df, EIN2, TAX_YEAR_COMP) # Set keys for the join
      res <- df[nearest.neighbors,
                on = .(EIN2, TAX_YEAR_COMP >= START_YEAR, TAX_YEAR_COMP <= END_PLUS),
                allow.cartesian = TRUE,
                nomatch = 0] # For each org in nearest.neighbors, give me the rows for that org from comparison.orgs corresponding to the TAX_YEAR values from START_YEAR through END_PLUS
      res <- res |> rename(START_YEAR = TAX_YEAR_COMP, END_PLUS = TAX_YEAR_COMP.1)
      
      # Filter out any macthed that have too much imputed data
      res <- res |> 
            group_by(EIN2, DISTANCE) |>
            mutate(PROP_IMPUTED = sum(IMPUTED)/n()) |>
            ungroup() |> 
            filter(PROP_IMPUTED <= 1/3)
      
      # Get top 5 neighbors and give each matched neighbor an ID number      
      res <- res |> arrange(DISTANCE) |>
            head(5*(n.data + n.predict)) |> 
            group_by(DISTANCE) |> 
            mutate(NEIGHBOR_ID = cur_group_id()) |> 
            ungroup()
      
      return(res)
}

get_likelihood_sigma <- function(row, dist_mat, rho, all_years, data){
      # Compute Matérn Covariance Matrix
      mat_cov <- Matern(d = dist_mat, smoothness = row[1], range = rho, phi = 1) + diag(row[2], dim(dist_mat)[1])
      
      #Compute sigma^2
      sigma.squared <- (1/length(data)) * (data %*% chol2inv(chol(mat_cov[all_years, all_years])) %*% data)[1,1] #chol2inv(chol(mat_cov[all_years, all_years])) solve(mat_cov[all_years, all_years])
      
      # Compute likelihood
      likelihood <- dmvnorm(data, 
                            mean = rep(0, length(data)), 
                            sigma = sigma.squared * mat_cov[all_years, all_years],
                            log = TRUE)
      return(likelihood)
}

gp.param.opt <- function(res, user.EIN, user.years, user.history, n.predict){
      df.nn <- res |> 
            group_by(EIN2, DISTANCE) |> 
            mutate(YEAR = row_number()) |> 
            ungroup() |> 
            filter(!IMPUTED) |> 
            mutate(TOT_REV.centered = TOT_REV - mean(TOT_REV)) 
      
      # Relevant data for current org
      n.data <- length(user.history)
      user.data <- as.data.table(list(EIN2 = user.EIN, TAX_YEAR = user.years, TOT_REV = user.history)) |>
            arrange(TAX_YEAR) |>
            mutate(YEAR = row_number()) |>
            select(EIN2, YEAR, TOT_REV) 
      
      # Normalized distance matrix
      dist_mat <- rdist(c(1:n.predict +n.data, df.nn$YEAR)) # the n.predict years we need to predict on and the years we have nearest neighbor data for
      dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))
      
      initial.conds <- randomLHS(10,2)
      initial.conds[, 1] <- 0.05 + initial.conds[, 1] * (2.5 - 0.05) # nu in [0.05,2.5)
      initial.conds[, 2] <- 0.05 + initial.conds[, 2] * (3 - 0.05) # nugget in [0.05,3]
      
      res.pars <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("EIN2", "nu", "nugget", "likelihood", "trial"))
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
            res.pars[nrow(res.pars)+1,1] <- user.EIN
            res.pars[nrow(res.pars),2:ncol(res.pars)] <- c(c(res.opt$par[1], res.opt$par[2], -1*res.opt$value, i))
      }
      # Parameters corresponding to max likelihood
      res.pars <- res.pars[which.max(res.pars$likelihood),]
      
      return(res.pars)
}

gp.predict <- function(res, res.pars, user.EIN, user.years, user.history, n.predict){     
      df.nn <- res |> 
            group_by(EIN2, DISTANCE) |> 
            mutate(YEAR = row_number()) |> 
            ungroup() |> 
            filter(!IMPUTED) |> 
            mutate(TOT_REV.centered = TOT_REV - mean(TOT_REV)) 
      
      # Relevant data for current org
      n.data <- length(user.history)
      user.data <- as.data.table(list(EIN2 = user.EIN, TAX_YEAR = user.years, TOT_REV = user.history)) |>
            arrange(TAX_YEAR) |>
            mutate(YEAR = row_number()) |>
            select(EIN2, YEAR, TOT_REV) 
      
      # Hyperparameters
      nu <- res.pars$nu
      nugget <- res.pars$nugget
      rho <- 1
      
      # Normalized distance matrix
      dist_mat <- rdist(c(1:n.predict +n.data, df.nn$YEAR)) # the n.predict years we need to predict on and the years we have nearest neighbor data for
      dist_mat <- (dist_mat - min(dist_mat)) / (max(dist_mat) - min(dist_mat))
      
      # For indexing the Matern covariance matrix
      years.to.predict <- 1:n.predict # indices (w.r.t dist_mat) of the years we need to predict on
      years.original <- (n.predict+1):dim(dist_mat)[1] # indices (w.r.t dist_mat) of the years we have nearest neighbor data for
      
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
      mat_cov <- sigma.squared * mat_cov
      
      # Conditional Distributions
      SIGMA.11 <- mat_cov[years.to.predict, years.to.predict]
      SIGMA.22.inv <- chol2inv(chol(mat_cov[years.original, years.original]))
      SIGMA.12 <- mat_cov[years.to.predict, years.original, drop = FALSE]
      
      predictions <- (mu_1 + (SIGMA.12 %*% SIGMA.22.inv %*% (data.nn - mu_2)))[,1] 
      standard_errors <- diag(sqrt((SIGMA.11 - (SIGMA.12 %*% SIGMA.22.inv %*% t(SIGMA.12)))))
      
      # add in predicted values and standard errors
      user.data <- user.data |>  mutate(TAX_YEAR = user.years,
                                        IMPUTE_STATUS = "Reported", 
                                        SE = 0,
                                        NEIGHBOR_ID = 0) 
      
      user.data <- bind_rows(user.data, as.data.frame(list(EIN2 = user.EIN,
                                                           YEAR = 1:n.predict + n.data,
                                                           TOT_REV = predictions + mean(df.nn$TOT_REV),
                                                           IMPUTE_STATUS = "Predicted",
                                                           SE = standard_errors,
                                                           NEIGHBOR_ID = 0)))
      return(user.data)
}

# res <- nn.search("ART", "EIN-00-0000000", c(2022, 2023), c(1, 1), 2)
# res.pars <- gp.param.opt(res, "EIN-00-0000000", c(2022, 2023), c(1, 1), 2)
# res.gp <- gp.predict(res, res.pars, "EIN-00-0000000", c(2022, 2023), c(1, 1), 2)

# res <- nn.search("ART", "EIN-00-0000000", c(2022, 2023, 2024), c(62369, 100199, 185830), 2)
# res.pars <- gp.param.opt(res, "EIN-00-0000000", c(2022, 2023, 2024), c(62369, 100199, 185830), 2)
# res.gp <- gp.predict(res, res.pars, "EIN-00-0000000", c(2022, 2023, 2024), c(62369, 100199, 185830), 2)

# res.gp$TAX_YEAR <- seq(2022, 2024 + 2)
# 
# deg.freedom <- nrow(res |> filter(IMPUTE_STATUS=="original")) - 1
# user_data_df <- res.gp |>
#       mutate(CI.LOWER = case_when(
#             IMPUTE_STATUS == "Reported" ~ TOT_REV,
#             IMPUTE_STATUS == "Predicted" ~ TOT_REV - qt(0.95, df = deg.freedom) * SE)) |>
#       mutate(CI.UPPER = case_when(
#             IMPUTE_STATUS == "Reported" ~ TOT_REV,
#             IMPUTE_STATUS == "Predicted" ~ TOT_REV + qt(0.95, df = deg.freedom) * SE)) 
# 
# df_reported <- user_data_df %>% filter(IMPUTE_STATUS == "Reported")
# df_predicted <- user_data_df %>% filter(IMPUTE_STATUS == "Predicted")
# df_transition <- user_data_df %>% filter((TAX_YEAR == 2024) | (TAX_YEAR == 2024+1))
# 
# df_peers <- res |> 
#       mutate(label = paste("Organization ", NEIGHBOR_ID, ": ", EIN2, sep = "")) |> 
#       group_by(NEIGHBOR_ID) |> 
#       mutate(TAX_YEAR = seq(2022, 2024 + 2)) |> 
#       ungroup() |> 
#       mutate(label = factor(label, levels = unique(label[order(NEIGHBOR_ID)])))