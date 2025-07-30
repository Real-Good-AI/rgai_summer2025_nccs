library(readr)
library(tidyverse, warn.conflicts = FALSE)
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(RANN)
library(fields)
library(ggplot2)
library(cowplot)
library(plotly)
library(scales)
library(lhs) # for latin hypercube sampling
source("SCRIPTS/outlier_detection_helper.R")

########################################################################
# User inputs
########################################################################
# User-supplied information
user.EIN <- "EIN-00-0000000" # user-supposed Employee Identification Number
user.years <- c(2022, 2023, 2024) # user-supplied years, must be minimum length 2, maximum length 5
user.history <- c(62369, 100199, 185830) # user-supplied revenue history, 1 per year
category <- "ART" # NTEE Category selected by the user from a dropdown menu
n.data <- length(user.history) # number of data points in user-supplied history
n.predict <- 2 # number of years the user wants to predict into (selected from a drop down list with options 1, 2, 3)

########################################################################
# Nearest Neighbor Search
########################################################################
nn.search <- function(category, user.EIN, user.years, user.history, n.predict){
    df <- readRDS("PREPROCESSING/processed_mega_df.rds") |> 
      filter(NTEE == category)
    
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
    
    # STEP 2: Nearest Neighbor search
    nearest.neighbors <- setNames(data.table(matrix(ncol = 4, nrow = 0)), c("EIN2", "START_YEAR", "END_YEAR", "DISTANCE")) # empty data frame that we'll fill with nearest neighbors
    for (year in seq(start.year, end.year-n.predict-n.data+1, 1)){
      curr.interval <- year:(year+n.data-1) # Time interval to be searched for K nearest neighbors
      
      # Get the data to search through and filter out any orgs with too much imputed data in the current n.data+n.predict interval
      df.search <- df |>
        filter(TAX_YEAR %in% year:(year+n.data-1+n.predict)) |> 
        mutate(IMPUTED = (IMPUTE_STATUS != "original")) |>
        select(EIN2, TAX_YEAR, TOT_REV, IMPUTED) |>
        group_by(EIN2) |>
        mutate(PROP_IMPUTED = sum(IMPUTED)/n(), YEAR = row_number()) |>
        ungroup() |> 
        filter(PROP_IMPUTED <= 1/3) |> 
        filter(TAX_YEAR %in% curr.interval)
      
      # Reshape to wide format where columns are years
      setDT(df.search)
      df.search <- df.search |> select(EIN2, YEAR, TOT_REV) |> dcast(EIN2 ~ YEAR, value.var = "TOT_REV")
      
      if (curr.interval[length(curr.interval)] > end.year-n.predict){break} # If the interval contains years it shouldn't, exit for loop
      
      # Get K nearest neighbors for this time window
      nearest <- nn2(df.search |> select(-EIN2), user.data |> select(-EIN2), k=5) 
      nearest.neighbors <- rbind(nearest.neighbors, as.data.table(list("EIN2" = df.search[as.vector(nearest$nn.idx),]$EIN2,
                                                                       "START_YEAR" = curr.interval[1],
                                                                       "END_YEAR" = curr.interval[length(curr.interval)],
                                                                       "DISTANCE" = as.vector(nearest$nn.dists))))
    }
    
    # STEP 3: Get the top 5 neighbors for each org
    nearest.neighbors <- nearest.neighbors |> 
      arrange(DISTANCE) |> # Within each organization, order by distance
      head(5)
    
    # Give each matched neighbor an ID number
    nearest.neighbors <- nearest.neighbors |> mutate(NEIGHBOR_ID = row_number()) |> ungroup()
    
    # Create an END_YEAR + n.predict column to help with ranges
    nearest.neighbors <- nearest.neighbors |> mutate(END_PLUS = END_YEAR + n.predict)
    
    # STEP 4: Get the full data for each nearest neighbor
    
    # Perform a non-equi join, where rows from nearest.neighbors are matched with rows in comparison.orgs based on conditions
    df <- df |> mutate(TAX_YEAR_COMP = TAX_YEAR) # Rename TAX_YEAR before joining to avoid name conflict
    df <- df |> select(EIN2, TAX_YEAR, TOT_REV, IMPUTE_STATUS, TAX_YEAR_COMP)
    setkey(df, EIN2, TAX_YEAR_COMP) # Set keys for the join
    res <- df[nearest.neighbors,
              on = .(EIN2, TAX_YEAR_COMP >= START_YEAR, TAX_YEAR_COMP <= END_PLUS),
              allow.cartesian = TRUE,
              nomatch = 0] # For each org in nearest.neighbors, give me the rows for that org from comparison.orgs corresponding to the TAX_YEAR values from START_YEAR through END_PLUS
    res <- res |> rename(START_YEAR = TAX_YEAR_COMP, END_PLUS = TAX_YEAR_COMP.1)
    
    return(res)
}
########################################################################
# Hyperparameter Optimization
########################################################################
df.nn <- res |> 
  group_by(EIN2, DISTANCE) |> 
  mutate(YEAR = row_number()) |> 
  ungroup() |> 
  filter(IMPUTE_STATUS == "original") |> 
  mutate(TOT_REV.centered = TOT_REV - mean(TOT_REV)) 

# Relevant data for current org
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

########################################################################
# Prediction with Gaussian Process
########################################################################
# Hyperparameters
nu <- res.pars$nu
nugget <- res.pars$nugget
rho <- 1

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

########################################################################
# Create a plot for the user based on results
########################################################################
user.data <- user.data |>  mutate(TAX_YEAR = user.years,
  IMPUTE_STATUS = "original", 
  SE = 0,
  NEIGHBOR_ID = 0) 

# add in predicted values and standard errors
user.data <- bind_rows(user.data, as.data.frame(list(EIN2 = user.EIN,
                                                 YEAR = 1:n.predict + n.data,
                                                 TOT_REV = predictions + mean(df.nn$TOT_REV),
                                                 IMPUTE_STATUS = "predicted",
                                                 SE = standard_errors,
                                                 NEIGHBOR_ID = 0)))
df.plot <- df.nn |> mutate(SE = 0)
deg.freedom <- nrow(df.plot) - 1
df.plot <- bind_rows(df.plot, user.data |> mutate(EIN2 = "Your organization"))
df.plot <- df.plot |> mutate(TAX_YEAR = YEAR + min(user.years) - 1) 

# Confidence intervals
df.plot <- df.plot |>
  mutate(CI.LOWER = case_when(
    IMPUTE_STATUS == "original" ~ TOT_REV,
    IMPUTE_STATUS == "predicted" ~ TOT_REV - qt(0.95, df = deg.freedom) * SE)) |>
  mutate(CI.UPPER = case_when(
    IMPUTE_STATUS == "original" ~ TOT_REV,
    IMPUTE_STATUS == "predicted" ~ TOT_REV + qt(0.95, df = deg.freedom) * SE)) |>
  mutate(LINETYPE = case_when(
    EIN2 == "Your organization" ~ "your organization",
    .default = "similar organizations"))

p <- df.plot |>
  ggplot(mapping = aes(x = TAX_YEAR, y = TOT_REV, color = EIN2)) +
  geom_line(mapping = aes(linewidth = LINETYPE, alpha = LINETYPE)) + # the lines for each organization, with line width and opacity (alpha) varying by the variable LINETYPE
  geom_line(data = (df.plot |> filter(IMPUTE_STATUS == "predicted")), linetype="dashed", color="white", linewidth=1.5) + # add dashed portion of line to predictions for your org
  geom_point(mapping = aes(shape = EIN2, size = LINETYPE)) + # add in markers, where shape varies by org and marker size varies by LINETYPE
  geom_text(data = (df.plot |> filter(IMPUTE_STATUS == "original", EIN2 == "Your organization")), # add labels to markers for the user input data
            aes(label = paste("$", scales::comma(TOT_REV)), sep=""), # Specify the label aesthetic
            nudge_x = -0.2, # Adjust label position horizontally
            nudge_y = 20000) + # nudge_x and nudge_y should somehow be determined dynamically for each organization to be as readable as possible
  geom_ribbon(data = (df.plot |> filter(IMPUTE_STATUS == "predicted")),
              aes(ymin = CI.LOWER, ymax = CI.UPPER,fill = IMPUTE_STATUS),
              alpha = 0.25, color = "darkgrey") + # confidence interval
  scale_color_manual(values = c("#009E73",  "#E8DA1D", "#0072B2", "#D55E00", "#CC79A7", "#000000"), name = "Organization ID") +
  scale_shape_manual(values = c(15, 17, 18, 7, 10, 19), name = "Organization ID") +
  scale_size_manual(values = c(2.5,3.5), guide="none") +
  scale_linewidth_manual(values = c(0.75, 1), guide="none") +
  scale_alpha_manual(values = c(0.75, 1), guide="none") +
  scale_fill_manual(values = "grey", labels="95% Confidence Interval", name="") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Total Revenue (USD)", title = "Comparing Your Organization to Similar Organizations") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=1)) + 
  theme_bw()

ggplotly(p)